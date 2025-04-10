library(DBI)
library(dplyr)
library(RMariaDB)
library(readr)
library(tools)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(factoextra)
library(plotly)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(xgboost)
library(pROC)
library(shiny)
library(ggplot2)
library(ggforce)
library(gt)
library(ipred)
library(reshape2)
library(grid)
library(MLmetrics)
library(glmnet)
library(scales)
library(lubridate)

# Opret forbindelse
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "fodbolddata",
  host = "localhost",
  port = 3306,
  user = "root",
  password = "Rev1l0 hceb"
)

#### DATAINDHENTNING ####
# Hent og join direkte med SQL
query_events <- "
SELECT *
FROM wyscout_matchevents_common_sl AS common
LEFT JOIN wyscout_matchevents_passes_sl AS passes ON common.EVENT_WYID = passes.EVENT_WYID
LEFT JOIN wyscout_matchevents_shots_sl AS shots ON common.EVENT_WYID = shots.EVENT_WYID
LEFT JOIN wyscout_matchevents_carry_sl AS carry ON common.EVENT_WYID = carry.EVENT_WYID
LEFT JOIN wyscout_matchevents_infractions_sl AS infractions ON common.EVENT_WYID = infractions.EVENT_WYID
LEFT JOIN wyscout_matchevents_groundduel_sl AS groundduel ON common.EVENT_WYID = groundduel.EVENT_WYID
LEFT JOIN wyscout_matchevents_secondarytype_sl AS secondary ON common.EVENT_WYID = secondary.EVENT_WYID
LEFT JOIN wyscout_matchevents_possessiontypes_sl AS possession ON common.EVENT_WYID = possession.EVENT_WYID
"

all_events <- dbGetQuery(con, query_events)


query_players <- "
SELECT *
FROM wyscout_players_sl AS players
LEFT JOIN wyscout_playermatches_sl AS pm
  ON players.PLAYER_WYID = pm.PLAYER_WYID AND players.SEASON_WYID = pm.SEASON_WYID
LEFT JOIN wyscout_matchdetail_players_sl AS mdp
  ON players.PLAYER_WYID = mdp.PLAYER_WYID
LEFT JOIN wyscout_playercareer_sl AS career
  ON players.PLAYER_WYID = career.PLAYER_WYID AND players.SEASON_WYID = career.SEASON_WYID
"

all_players <- dbGetQuery(con, query_players)


query_matches <- "
SELECT *
FROM wyscout_matchdetail_base_sl AS matchbase
LEFT JOIN wyscout_teammatches_sl AS teammatch
  ON matchbase.MATCH_WYID = teammatch.MATCH_WYID
  AND matchbase.TEAM_WYID = teammatch.TEAM_WYID
LEFT JOIN wyscout_teams_sl AS teams
  ON matchbase.TEAM_WYID = teams.TEAM_WYID
"

all_matches <- dbGetQuery(con, query_matches)


query_formations <- "
SELECT *
FROM wyscout_matchformations_sl
"

all_formations <- dbGetQuery(con, query_formations)


query_substitutions <- "
SELECT *
FROM wyscout_matchdetail_substitutions_sl
"

all_substitutions <- dbGetQuery(con, query_substitutions)



#### OPRYDNING AF DATA HENTET FRA SQL ####
all_events_unique <- all_events[ , !duplicated(colnames(all_events))]
all_matches_unique <- all_matches[ , !duplicated(colnames(all_matches))]
all_matches_dedup <- all_matches_unique %>%
  distinct(MATCH_WYID, TEAM_WYID, .keep_all = TRUE)
all_players <- all_players %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)

hjemme_ude_score <- all_matches_dedup %>%
  mutate(
    Hjemme = ifelse(SIDE == "home", OFFICIALNAME, NA),
    Ude = ifelse(SIDE == "away", OFFICIALNAME, NA),
    Hjemme_score = ifelse(SIDE == "home", SCORE, NA),
    Ude_score = ifelse(SIDE == "away", SCORE, NA)
  ) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    Hjemme = first(na.omit(Hjemme)),
    Ude = first(na.omit(Ude)),
    Hjemme_score = first(na.omit(Hjemme_score)),
    Ude_score = first(na.omit(Ude_score)),
    DATE = first(DATE),
    GAMEWEEK = first(GAMEWEEK),
    .groups = "drop"
  )

Samlet <- all_events_unique %>%
  left_join(hjemme_ude_score, by = "MATCH_WYID") %>%
  left_join(all_players %>% select(PLAYER_WYID, SHORTNAME, ROLENAME, FOOT), by = "PLAYER_WYID")

#### OPGAVE 1 ####
#### Opgave 1.1 / 1.2 / 1.3 ####
#Relevante variabler for sæson 2023-2024
xg_data <- Samlet %>%
  filter(PRIMARYTYPE == "shot",
  SEASON_WYID == 188945) %>%
  select(
    EVENT_WYID,
    MATCH_WYID,
    PLAYER_WYID,
    TEAM_WYID,
    PRIMARYTYPE,
    LOCATIONX,
    LOCATIONY,
    SHOTISGOAL,
    SHOTXG,
    SHOTONTARGET,
    SHOTXG,
    SHOTBODYPART,
    SHOTGOALZONE,
    POSSESSIONDURATION,
    POSSESSIONTYPE1,
    ATTACKFLANK,
  )


#Feautere Creation
field_length_m <- 105
field_width_m <- 68
# Stolpeafstand i meter (mål er 7.32 m bredt)
goal_width <- 7.32

# Midtpunkt (y = 50), vi konverterer det til meter vha. banebredden
goal_y_center_m <- 50 / 100 * field_width_m

# Stolper i meter
left_post_y <- goal_y_center_m - goal_width / 2
right_post_y <- goal_y_center_m + goal_width / 2
goal_x_m <- 100 / 100 * field_length_m  # altid 100%

# Beregning
xg_data <- xg_data %>%
  mutate(
    x_m = LOCATIONX / 100 * field_length_m,
    y_m = LOCATIONY / 100 * field_width_m,
    
    # afstand til mål som før
    distance_to_goal = sqrt((x_m - goal_x_m)^2 + (y_m - goal_y_center_m)^2),
    
    # vinkel mellem stolper (i radianer → grader)
    angle_to_goal = atan2(right_post_y - y_m, goal_x_m - x_m) -
      atan2(left_post_y - y_m, goal_x_m - x_m),
    angle_to_goal = angle_to_goal * (180 / pi)  # konverter til grader
  )



#Findet det event, som kom før målet sammen med placeringen (location.x.y)
Samlet <- Samlet %>%
  arrange(MATCH_WYID, TEAM_WYID, MINUTE, SECOND) %>%
  group_by(MATCH_WYID, TEAM_WYID) %>%
  mutate(
    prev_event_type = lag(PRIMARYTYPE),
    prev_location_x = lag(LOCATIONX),
    prev_location_y = lag(LOCATIONY)
  ) %>%
  ungroup()

#Smider det ind i datasettet
xg_data <- xg_data %>%
  left_join(
    Samlet %>%
      filter(PRIMARYTYPE == "shot") %>%
      transmute(
        EVENT_WYID,
        assist_type = prev_event_type,
        assist_location_x = prev_location_x,
        assist_location_y = prev_location_y
      ),
    by = "EVENT_WYID"
  )

#Distance og vinkel fra assist
xg_data <- xg_data %>%
  mutate(
    assist_x_m = assist_location_x / 100 * field_length_m,
    assist_y_m = assist_location_y / 100 * field_width_m,
    
    assist_to_goal_distance = sqrt((assist_x_m - goal_x_m)^2 + (assist_y_m - goal_y_center_m)^2),
    
    assist_angle_to_goal = atan2(right_post_y - assist_y_m, goal_x_m - assist_x_m) -
      atan2(left_post_y - assist_y_m, goal_x_m - assist_x_m),
    assist_angle_to_goal = assist_angle_to_goal * (180 / pi)
  )

#Fjerner 1 række med NA-værdier
sum(!complete.cases(xg_data))
xg_data <- na.omit(xg_data)

#Udvælgelse af variable og nye dataset
#Vores y = SHOTISGOAL
#Vores x = SHOTBODYPART, FOOT (Om det er spillerens gode fod), distance_to_goal, angle_to_goal,
#POSSESSIONTYPE1, assist_angle_to_goal, assist_to_goal_distance
xg_udvalgt <- xg_data %>%
  select(distance_to_goal, angle_to_goal, SHOTBODYPART, POSSESSIONTYPE1, 
         assist_angle_to_goal, assist_to_goal_distance, SHOTISGOAL)

xg_udvalgt <- xg_udvalgt %>%
  mutate(
    SHOTBODYPART = as.factor(SHOTBODYPART),
    POSSESSIONTYPE1 = as.factor(POSSESSIONTYPE1),
    SHOTISGOAL = as.factor(SHOTISGOAL)
  )



# Split: 70% træning, 30% test
set.seed(123)  # for reproducerbarhed

splitIndex <- createDataPartition(xg_udvalgt$SHOTISGOAL, p = 0.7, list = FALSE)

trainData <- xg_udvalgt[splitIndex, ]
testData  <- xg_udvalgt[-splitIndex, ]

prop.table(table(trainData$SHOTISGOAL))
prop.table(table(testData$SHOTISGOAL))

# Beregn fordeling i procent
train_dist <- prop.table(table(trainData$SHOTISGOAL)) * 100
test_dist  <- prop.table(table(testData$SHOTISGOAL)) * 100

# Saml i en dataframe
dist_df <- data.frame(
  Klasse = names(train_dist),
  Træningsdata = round(as.numeric(train_dist), 1),
  Testdata = round(as.numeric(test_dist), 1)
)

dist_df %>%
  gt() %>%
  tab_header(
    title = "Både træning og test indeholder 11.7% mål",
    subtitle = "I trænings- og testdatasæt (%-andel)"
  ) %>%
  cols_label(
    Klasse = "Klasse",
    Træningsdata = "Træningsdata (%)",
    Testdata = "Testdata (%)"
  ) %>%
  fmt_number(columns = c("Træningsdata", "Testdata"), decimals = 1) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#Y-variable
trainTarget <- trainData$SHOTISGOAL
testTarget  <- testData$SHOTISGOAL

#Fjern target fra feature-data
træningsdata <- trainData %>% select(-SHOTISGOAL)
testdata  <- testData %>% select(-SHOTISGOAL)


#### Opg 1.4 ####
set.seed(423)
#Random Forest model
# 1. Random Forest – standard (uden tuneGrid)
model_rf <- train(
  x = træningsdata,
  y = trainTarget,
  method = "rf"
)

predictions_rf <- predict(model_rf, testdata)
confusionMatrix(predictions_rf, testTarget)
varImp(model_rf)

# 2. Logistisk regression – standard
model_log <- train(
  x = træningsdata,
  y = trainTarget,
  method = "glm",
  family = "binomial",
)

predictions_log <- predict(model_log, testdata)
confusionMatrix(predictions_log, testTarget)
summary(model_log$finalModel)

# 3. Beslutningstræ – standard
træningsdata_træ <- træningsdata
træningsdata_træ$SHOTISGOAL <- trainTarget

model_tree <- rpart(SHOTISGOAL ~ ., data = træningsdata_træ, method = "class")
rpart.plot(model_tree, type = 2, extra = 104, tweak = 1.2)

predictions_tree <- predict(model_tree, testdata, type = "class")
confusionMatrix(predictions_tree, testTarget)
model_tree$variable.importance

# 4. Bagging – standard (mtry = alle features)
model_bagging <- randomForest(
  x = træningsdata,
  y = trainTarget,
  ntree = 500  # Default er 500, men tydeliggøres her
)

predictions_bagging <- predict(model_bagging, testdata)
confusionMatrix(predictions_bagging, testTarget)
varImpPlot(model_bagging, main = "Variabelvigtighed – Bagging")


#### ROC-KURVER ####
# 1. Forudsig sandsynligheder (type = "prob") for alle modeller:
probs_rf      <- predict(model_rf, testdata, type = "prob")[, "1"]
probs_log     <- predict(model_log, testdata, type = "prob")[, "1"]
probs_tree    <- predict(model_tree, testdata, type = "prob")[, "1"]
probs_bagging <- predict(model_bagging, testdata, type = "prob")[, "1"]

# 2. Beregn ROC
roc_rf      <- roc(testTarget, probs_rf)
roc_log     <- roc(testTarget, probs_log)
roc_tree    <- roc(testTarget, probs_tree)
roc_bagging <- roc(testTarget, probs_bagging)

# 3. Tegn alle ROC-kurver
plot(roc_rf, col = "blue", lwd = 2, main = "Logistisk regression har den højeste AUC (Area under the curve)")
plot(roc_log, col = "red", lwd = 2, add = TRUE)
plot(roc_tree, col = "green", lwd = 2, add = TRUE)
plot(roc_bagging, col = "purple", lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2)
legend("bottomright",
       legend = c(
         paste("Random Forest (AUC =", round(auc(roc_rf), 2), ")"),
         paste("Logistisk Regression (AUC =", round(auc(roc_log), 2), ")"),
         paste("Beslutningstræ (AUC =", round(auc(roc_tree), 2), ")"),
         paste("Bagging (AUC =", round(auc(roc_bagging), 2), ")")
       ),
       col = c("blue", "red", "green", "purple"),
       lwd = 2,
       cex = 0.4)  # Her styrer du størrelsen

# Konverter sandsynligheder til forudsigelser (cutoff = 0.5)
# Navngiv modeller og predictions i en liste
threshold <- 0.5  # Du kan justere den senere
pred_rf      <- factor(ifelse(probs_rf > threshold, "1", "0"), levels = c("0", "1"))
pred_log     <- factor(ifelse(probs_log > threshold, "1", "0"), levels = c("0", "1"))
pred_tree    <- factor(ifelse(probs_tree > threshold, "1", "0"), levels = c("0", "1"))
pred_bagging <- factor(ifelse(probs_bagging > threshold, "1", "0"), levels = c("0", "1"))

#### Test 2 - tuning og 10-CV ####
set.seed(123)

# --- 1. Genskab Y-variabler (hvis SHOTISGOAL er fjernet fra dine X) ---

# Til træmodellen:
#træningsdata_træ <- cbind(træningsdata, SHOTISGOAL = trainTarget)

trainTarget <- factor(trainData$SHOTISGOAL, levels = c(0, 1), labels = c("No", "Yes"))
testTarget  <- factor(testData$SHOTISGOAL,  levels = c(0, 1), labels = c("No", "Yes"))

# --- 2. Træn ALLE 4 MODELLER med Accuracy som mål (robust) ---

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = "final")

# 2.1 Random Forest
rf_tuned <- train(
  x = træningsdata,
  y = trainTarget,
  method = "rf",
  metric = "Accuracy",
  trControl = ctrl,
  tuneGrid = expand.grid(mtry = c(2, 3, 4, 5))
)

# 2.2 Logistisk regression
log_tuned <- train(
  x = træningsdata,
  y = trainTarget,
  method = "glm",
  family = "binomial",
  metric = "Accuracy",
  trControl = ctrl
)

# 2.3 Beslutningstræ
tree_tuned <- train(
  x = træningsdata,
  y = trainTarget,
  method = "rpart",
  #metric = "Accuracy",
  trControl = ctrl,
  tuneGrid = expand.grid(cp = c(0.001, 0.01, 0.05))
)

rpart.plot(tree_tuned$finalModel,
           type = 2,            # Viser splits og labels
           extra = 104,         # Viser klasse, procent og antal obs
           under = TRUE,        # Viser information under noder
           fallen.leaves = TRUE,
           tweak = 1,
           cex = 0.5,
           main = "Beslutningstræ: Vinkel og afstand har størst betydning")

# 2.4 Bagging
bagging_tuned <- train(
  x = træningsdata,
  y = trainTarget,
  method = "treebag",
  metric = "Accuracy",
  trControl = ctrl,
  nbagg = 100  # standard er 25
)


# --- 3. FORUDSIG på testdata (probabilities) og evaluer modellerne ---

# 3.1 Predict probabilities
probs_rf      <- predict(rf_tuned, testdata, type = "prob")[, "Yes"]
probs_log     <- predict(log_tuned, testdata, type = "prob")[, "Yes"]
probs_tree    <- predict(tree_tuned, testdata, type = "prob")[, "Yes"]
probs_bagging <- predict(bagging_tuned, testdata, type = "prob")[, "Yes"]

# 3.2 Predict klasser (threshold = 0.5)
pred_rf      <- factor(ifelse(probs_rf > 0.5, "Yes", "No"), levels = c("No", "Yes"))
pred_log     <- factor(ifelse(probs_log > 0.5, "Yes", "No"), levels = c("No", "Yes"))
pred_tree    <- factor(ifelse(probs_tree > 0.5, "Yes", "No"), levels = c("No", "Yes"))
pred_bagging <- factor(ifelse(probs_bagging > 0.5, "Yes", "No"), levels = c("No", "Yes"))

# 3.3 Confusion Matrices
cm_rf      <- confusionMatrix(pred_rf, testTarget, positive = "Yes")
cm_log     <- confusionMatrix(pred_log, testTarget, positive = "Yes")
cm_tree    <- confusionMatrix(pred_tree, testTarget, positive = "Yes")
cm_bagging <- confusionMatrix(pred_bagging, testTarget, positive = "Yes")

# 3.4 ROC-kurver
roc_rf      <- roc(testTarget, probs_rf)
roc_log     <- roc(testTarget, probs_log)
roc_tree    <- roc(testTarget, probs_tree)
roc_bagging <- roc(testTarget, probs_bagging)

# 3.5 AUC
auc_rf      <- auc(roc_rf)
auc_log     <- auc(roc_log)
auc_tree    <- auc(roc_tree)
auc_bagging <- auc(roc_bagging)


# Tegn samlet ROC-kurve
plot(roc_rf, col = "blue", lwd = 2, main = "ROC-kurve for alle modeller")
plot(roc_bagging, col = "red", lwd = 2, add = TRUE)
plot(roc_tree, col = "green", lwd = 2, add = TRUE)
plot(roc_log, col = "purple", lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2)
legend("bottomright",
       legend = c(
         paste("Random Forest (AUC =", round(auc(roc_rf), 2), ")"),
         paste("Logistisk Regression (AUC =", round(auc(roc_log), 2), ")"),
         paste("Beslutningstræ (AUC =", round(auc(roc_tree), 2), ")"),
         paste("Bagging (AUC =", round(auc(roc_bagging), 2), ")")
       ),
       col = c("purple", "red", "green", "purple"),
       lwd = 2,
       cex = 0.5)

# 3.5 Saml nøgletal i en tabel
summary_results <- data.frame(
  Model = c("Logistisk regression", "Beslutningstræ", "Random Forest", "Bagging"),
  F1 = c(cm_log$byClass["F1"], cm_tree$byClass["F1"], cm_rf$byClass["F1"], cm_bagging$byClass["F1"]),
  AUC = c(auc_log, auc_tree, auc_rf, auc_bagging),
  Accuracy = c(cm_log$overall["Accuracy"], cm_tree$overall["Accuracy"], cm_rf$overall["Accuracy"], cm_bagging$overall["Accuracy"]),
  Recall = c(cm_log$byClass["Sensitivity"], cm_tree$byClass["Sensitivity"], cm_rf$byClass["Sensitivity"], cm_bagging$byClass["Sensitivity"]),
  Precision = c(cm_log$byClass["Precision"], cm_tree$byClass["Precision"], cm_rf$byClass["Precision"], cm_bagging$byClass["Precision"])
)

#GT tabel
summary_results %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  gt() %>%
  tab_header(
    title = "Bagging har en højere F1 og Recall score end de andre modeller",
    subtitle = "10-fold cross-validation og tuning af hyperparametre"
  ) %>%
  cols_label(
    Model = "Model",
    F1 = "F1-score",
    AUC = "AUC",
    Accuracy = "Accuracy",
    Recall = "Recall",
    Precision = "Precision"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#Variablerne
# 1. Hent importance-data fra modellen
importance_log <- varImp(log_tuned)$importance
importance_log$Variable <- rownames(importance_log)

# 2. Sortér efter betydning
importance_log <- importance_log %>%
  arrange(Overall)

# 3. Lav lækkert linjeplot
ggplot(importance_log, aes(x = reorder(Variable, Overall), y = Overall, group = 1)) +
  geom_line(color = "#E31A1C", linewidth = 1, linetype = "dashed") +
  geom_point(aes(color = Overall), size = 4) +
  scale_color_gradient(low = "#FDBBA1", high = "#E31A1C") +
  labs(
    title = "De vigtigste forklarende variabler i den logistiske regression er afstand og vinkel på skuddet",
    x = "Forklarende Variabel",
    y = "Importance (absolut koefficient)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    legend.position = "none"
  )


#Grænseværdi
thresholds <- seq(0.1, 0.9, by = 0.01)

# 2. Beregn nøgletal for hver grænseværdi
f1_scores <- recall_scores <- precision_scores <- accuracy_scores <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  pred <- ifelse(probs_log > thresholds[i], "Yes", "No")
  cm <- confusionMatrix(as.factor(pred), testTarget, positive = "Yes")
  
  f1_scores[i]        <- cm$byClass["F1"]
  recall_scores[i]    <- cm$byClass["Sensitivity"]
  precision_scores[i] <- cm$byClass["Precision"]
  accuracy_scores[i]  <- cm$overall["Accuracy"]
}

# 3. Oprindelig og "egen" grænseværdi
best_threshold <- 0.50  # Fast, standard

# 4. Nyt: Hvor F1, Recall og Precision er tættest på hinanden
distance_metric <- function(f1, rec, prec) {
  abs(f1 - rec) + abs(f1 - prec) + abs(rec - prec)
}
f1_recall_precision_diff <- mapply(distance_metric, f1_scores, recall_scores, precision_scores)
balanced_idx <- which.min(f1_recall_precision_diff)
balanced_threshold <- thresholds[balanced_idx]

# 5. Plot
plot(thresholds, f1_scores, type = "l", col = "darkgreen", lwd = 2, ylim = c(0, 1),
     xlab = "Grænseværdi", ylab = "Score", main = "Recall stiger ved en lavere grænseværdi")

lines(thresholds, recall_scores, col = "steelblue", lty = 2, lwd = 2)
lines(thresholds, precision_scores, col = "orange", lty = 3, lwd = 2)
lines(thresholds, accuracy_scores, col = "purple", lty = 4, lwd = 2)

# 6. Markér punkter
abline(v = best_threshold, col = "black", lty = 2)
text(best_threshold + 0.01, 0.91, 
     labels = "Oprindelig grænseværdi", pos = 4, col = "black", font = 0.2)

# 7. Nyt punkt hvor F1 ≈ Recall ≈ Precision
abline(v = balanced_threshold, col = "red", lty = 3)
points(balanced_threshold, f1_scores[balanced_idx], col = "red", pch = 19)
text(balanced_threshold + 0.015, f1_scores[balanced_idx] + 0.05,
     labels = paste("Ny grænseværdi:", round(balanced_threshold, 2)),
     col = "red", font = 0.2, pos = 4)

# 8. Forklaring
legend("bottomright",
       legend = c("F1-score", "Recall", "Precision", "Accuracy"),
       col = c("darkgreen", "steelblue", "orange", "purple"),
       lty = 1:4, lwd = 2, cex = 0.5)


#### Matricer (alle) ####
#Logistisk regression
acc_log <- paste0(round(cm_log$overall["Accuracy"] * 100, 1), "%")

# Klargør confusion matrix
cm_log_matrice <- as.data.frame(cm_log$table)
colnames(cm_log_matrice) <- c("Reference", "Prediction", "Freq")

# Sæt rækkefølge: YES nederst
cm_log_matrice$Reference <- factor(cm_log_matrice$Reference, levels = c("Yes", "No"))
cm_log_matrice$Prediction <- factor(cm_log_matrice$Prediction, levels = c("No", "Yes"))

# Plot
ggplot(cm_log_matrice, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", guide = "none") +  # Fjern farvelegend
  labs(
    title = "Logistisk Regression identificerer 20 mål korrekt",
    subtitle = paste("GOAL = YES, NO GOAL = NO\nAccuracy =", acc_log),
    x = "Prediction",
    y = "Reference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )

#Træ
acc_tree <- paste0(round(cm_tree$overall["Accuracy"] * 100, 1), "%")

# Klargør confusion matrix
cm_tree_matrice <- as.data.frame(cm_tree$table)
colnames(cm_tree_matrice) <- c("Reference", "Prediction", "Freq")

# Sæt rækkefølge: YES nederst
cm_tree_matrice$Reference <- factor(cm_tree_matrice$Reference, levels = c("Yes", "No"))
cm_tree_matrice$Prediction <- factor(cm_tree_matrice$Prediction, levels = c("No", "Yes"))

# Plot
ggplot(cm_tree_matrice, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", guide = "none") +  # Fjern farvelegend
  labs(
    title = "Beslutningstræ identificerer 15 mål korrekt",
    subtitle = paste("GOAL = YES, NO GOAL = NO\nAccuracy =", acc_tree),
    x = "Prediction",
    y = "Reference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )

#Random Forrest
acc_rf <- paste0(round(cm_rf$overall["Accuracy"] * 100, 1), "%")

# Klargør confusion matrix
cm_rf_matrice <- as.data.frame(cm_rf$table)
colnames(cm_rf_matrice) <- c("Reference", "Prediction", "Freq")

# Sæt rækkefølge: YES nederst
cm_rf_matrice$Reference <- factor(cm_rf_matrice$Reference, levels = c("Yes", "No"))
cm_rf_matrice$Prediction <- factor(cm_rf_matrice$Prediction, levels = c("No", "Yes"))

# Plot
ggplot(cm_rf_matrice, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", guide = "none") +  # Fjern farvelegend
  labs(
    title = "Random Forrest identificerer 19 mål korrekt",
    subtitle = paste("GOAL = YES, NO GOAL = NO\nAccuracy =", acc_rf),
    x = "Prediction",
    y = "Reference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )

#Bagging
acc_bag <- paste0(round(cm_bagging$overall["Accuracy"] * 100, 1), "%")

# Klargør confusion matrix
cm_bag_matrice <- as.data.frame(cm_bagging$table)
colnames(cm_bag_matrice) <- c("Reference", "Prediction", "Freq")

# Sæt rækkefølge: YES nederst
cm_bag_matrice$Reference <- factor(cm_bag_matrice$Reference, levels = c("Yes", "No"))
cm_bag_matrice$Prediction <- factor(cm_bag_matrice$Prediction, levels = c("No", "Yes"))

# Plot
ggplot(cm_bag_matrice, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", guide = "none") +  # Fjern farvelegend
  labs(
    title = "Bagging identificerer 23 mål korrekt",
    subtitle = paste("GOAL = YES, NO GOAL = NO\nAccuracy =", acc_bag),
    x = "Prediction",
    y = "Reference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )


#### Grænseværdi på .32 ####
prob_log_new <- predict(log_tuned, testdata, type = "prob")[, "Yes"]
pred_log_new <- factor(ifelse(prob_log_new > 0.24, "Yes", "No"), levels = c("No", "Yes"))
cm_log_new <- confusionMatrix(pred_log_new, testTarget, positive = "Yes")
roc_log_new<- roc(testTarget, prob_log_new)
auc_log_new <- auc(roc_log_new)

acc <- paste0(round(cm_log_new$overall["Accuracy"] * 100, 1), "%")

# Klargør confusion matrix
cm_df <- as.data.frame(cm_log_new$table)
colnames(cm_df) <- c("Reference", "Prediction", "Freq")

# Sæt rækkefølge: YES nederst
cm_df$Reference <- factor(cm_df$Reference, levels = c("Yes", "No"))
cm_df$Prediction <- factor(cm_df$Prediction, levels = c("No", "Yes"))

# Plot
ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", guide = "none") +  # Fjern farvelegend
  labs(
    title = "Logistisk regression identificerer 49 mål korrekt",
    subtitle = paste("GOAL = YES, NO GOAL = NO\nAccuracy =", acc),
    x = "Prediction",
    y = "Reference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )

#MANGLER MATRICE TIL NY GRÆNSEVÆRDI

#### XG ####

#Med spiller info
players_udvalgt <- all_players %>%
  select(
    PLAYER_WYID,
    SHORTNAME,
    ROLENAME,
    APPEARANCES,
    MINUTESPLAYED)

Samlet_players <- merge(xg_data, players_udvalgt, by = "PLAYER_WYID")

#Med hold info
hold_udvalgt <- all_matches_dedup %>%
  filter(SEASON_WYID == 188945) %>%
  distinct(TEAM_WYID, OFFICIALNAME)


Samlet_players <- merge(Samlet_players, hold_udvalgt, by = "TEAM_WYID")

Samlet_players$xG <- predict(log_tuned, newdata = Samlet_players, type = "prob")[,"Yes"]

#Kampe og score
all_matches_dedup <- all_matches_dedup %>%
  group_by(MATCH_WYID) %>%
  mutate(Resultat = paste0(sum(SCORE[SIDE == "home"]), "-", sum(SCORE[SIDE == "away"]))) %>%
  ungroup()

all_matches_dedup <- all_matches_dedup %>%
  group_by(MATCH_WYID) %>%
  mutate(Kamp = paste0(OFFICIALNAME[SIDE == "home"], "-", OFFICIALNAME[SIDE == "away"])) %>%
  ungroup()

Resultat_kamp <- all_matches_dedup %>%
  select(MATCH_WYID, Resultat, Kamp) %>%
  distinct(MATCH_WYID, .keep_all = TRUE)

Samlet_players <- merge(Samlet_players, Resultat_kamp, by = "MATCH_WYID")

Runde_id <- all_matches_dedup %>%
  filter(SEASON_WYID == 188945) %>%
  mutate(
    Runde = case_when(
      ROUND_WYID == 4429397 ~ "SuperLiga",
      ROUND_WYID == 4431691 ~ "Nedrykningsgruppe",
      ROUND_WYID == 4431692 ~ "Mesterskabet",
      TRUE ~ "Ukendt"
    )
  ) %>%
  distinct(MATCH_WYID, .keep_all = TRUE) %>%
  select(MATCH_WYID, ROUND_WYID, Runde)


Samlet_players <- merge(Samlet_players, Runde_id, by = "MATCH_WYID")


#### Opg. 1.6 ####
#Rensning på samme måde som ved 2023/2024
brøndby_xg <- Samlet %>%
  filter(PRIMARYTYPE == "shot", SEASON_WYID == 189918) %>%
  select(
    EVENT_WYID, MATCH_WYID, PLAYER_WYID, TEAM_WYID,
    PRIMARYTYPE, LOCATIONX, LOCATIONY,
    SHOTISGOAL, SHOTONTARGET, SHOTXG, SHOTBODYPART, SHOTGOALZONE,
    SHOTGOALKEEPERACTION_WYID, SHOTGOALKEEPER_WYID,
    POSSESSIONDURATION, POSSESSIONTYPE1, ATTACKFLANK
  )

#Afstand og vinkel
goal_width <- 7.32
goal_y_center_m <- 50 / 100 * field_width_m
left_post_y <- goal_y_center_m - goal_width / 2
right_post_y <- goal_y_center_m + goal_width / 2
goal_x_m <- 100 / 100 * field_length_m

brøndby_xg <- brøndby_xg %>%
  mutate(
    x_m = LOCATIONX / 100 * field_length_m,
    y_m = LOCATIONY / 100 * field_width_m,
    distance_to_goal = sqrt((x_m - goal_x_m)^2 + (y_m - goal_y_center_m)^2),
    angle_to_goal = atan2(right_post_y - y_m, goal_x_m - x_m) -
      atan2(left_post_y - y_m, goal_x_m - x_m),
    angle_to_goal = angle_to_goal * (180 / pi)
  )

#Tidligere event for assist
Samlet <- Samlet %>%
  arrange(MATCH_WYID, TEAM_WYID, MINUTE, SECOND) %>%
  group_by(MATCH_WYID, TEAM_WYID) %>%
  mutate(
    prev_event_type = lag(PRIMARYTYPE),
    prev_location_x = lag(LOCATIONX),
    prev_location_y = lag(LOCATIONY)
  ) %>%
  ungroup()

# Tilføj assistinformation
brøndby_xg <- brøndby_xg %>%
  left_join(
    Samlet %>%
      filter(PRIMARYTYPE == "shot") %>%
      transmute(
        EVENT_WYID,
        assist_type = prev_event_type,
        assist_location_x = prev_location_x,
        assist_location_y = prev_location_y
      ),
    by = "EVENT_WYID"
  )

# Beregn assistafstand og assistvinkel
brøndby_xg <- brøndby_xg %>%
  mutate(
    assist_x_m = assist_location_x / 100 * field_length_m,
    assist_y_m = assist_location_y / 100 * field_width_m,
    assist_to_goal_distance = sqrt((assist_x_m - goal_x_m)^2 + (assist_y_m - goal_y_center_m)^2),
    assist_angle_to_goal = atan2(right_post_y - assist_y_m, goal_x_m - assist_x_m) -
      atan2(left_post_y - assist_y_m, goal_x_m - assist_x_m),
    assist_angle_to_goal = assist_angle_to_goal * (180 / pi)
  )

# Udvælg relevante variabler og lav nyt dataset
brøndby_xg_udvalgt <- brøndby_xg %>%
  select(distance_to_goal, angle_to_goal, SHOTBODYPART, POSSESSIONTYPE1, 
         assist_angle_to_goal, assist_to_goal_distance, SHOTISGOAL) %>%
  mutate(
    SHOTBODYPART = as.factor(SHOTBODYPART),
    POSSESSIONTYPE1 = as.factor(POSSESSIONTYPE1),
    SHOTISGOAL = as.factor(SHOTISGOAL)
  )

# Fjern NA'er
#brøndby_xg_udvalgt <- na.omit(brøndby_xg_udvalgt)

train_dist_b <- prop.table(table(xg_udvalgt$SHOTISGOAL)) * 100
test_dist_b  <- prop.table(table(brøndby_xg_udvalgt$SHOTISGOAL)) * 100

# Saml i en dataframe
dist_df_b <- data.frame(
  Klasse = names(train_dist_b),
  Træningsdata = round(as.numeric(train_dist_b), 1),
  Testdata = round(as.numeric(test_dist_b), 1)
)

dist_df_b %>%
  gt() %>%
  tab_header(
    title = "Træning og test har har samme procendel skud",
    subtitle = "I trænings- og testdatasæt (%-andel)"
  ) %>%
  cols_label(
    Klasse = "Klasse",
    Træningsdata = "Træningsdata (%)",
    Testdata = "Testdata (%)"
  ) %>%
  fmt_number(columns = c("Træningsdata", "Testdata"), decimals = 1) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )


#### Klassificering ####
brøndby_trainTarget  <- xg_udvalgt$SHOTISGOAL
brøndby_testTarget   <- brøndby_xg_udvalgt$SHOTISGOAL

brøndby_træningsdata <- xg_udvalgt %>% select(-SHOTISGOAL)
brøndby_testdata     <- brøndby_xg_udvalgt %>% select(-SHOTISGOAL)

# === 1. Genskab y-variabler (faktor med Yes/No) ===
brøndby_trainTarget <- factor(brøndby_trainTarget, levels = c(0,1), labels = c("No", "Yes"))
brøndby_testTarget  <- factor(brøndby_testTarget, levels = c(0,1), labels = c("No", "Yes"))

# === 2. Træningskontrol ===
ctrl_b <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = "final")

# === 2.4 Bagging ===
# 2.2 Logistisk regression
log_tuned_b <- train(
  x = træningsdata,
  y = trainTarget,
  method = "glm",
  family = "binomial",
  metric = "Accuracy",
  trControl = ctrl
)

# === 3. Predict og evaluering ===
# 3.1 Sandsynligheder
probs_log_b <- predict(log_tuned_b, brøndby_testdata, type = "prob")[, "Yes"]

# 3.2 Klassifikation
pred_log_b <- factor(ifelse(probs_log_b > 0.21, "Yes", "No"), levels = c("No", "Yes"))

# 3.3 Confusion Matrices
cm_log_b <- confusionMatrix(pred_log_b, brøndby_testTarget, positive = "Yes")

# 3.4 ROC-kurver
roc_log_b <- roc(brøndby_testTarget, probs_log_b)

# 3.5 AUC
auc_log_b <- auc(roc_log_b)

# === 4. Plot ROC-kurver ===
plot(roc_log_b, col = "purple", lwd = 2,
     main = "Logistisk regression har en AUC på 74%")
abline(a = 0, b = 1, lty = 2)
legend("bottomright",
       legend = paste("Bagging (AUC =", round(auc_log_b, 2), ")"),
       col = "purple",
       lwd = 2,
       cex = 0.8)


#Grænseværdi
thresholds <- seq(0.1, 0.9, by = 0.01)

# 2. Beregn nøgletal for hver grænseværdi
f1_scores <- recall_scores <- precision_scores <- accuracy_scores <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  pred <- ifelse(probs_log_b > thresholds[i], "Yes", "No")
  cm <- confusionMatrix(as.factor(pred), brøndby_testTarget, positive = "Yes")
  
  f1_scores[i]        <- cm$byClass["F1"]
  recall_scores[i]    <- cm$byClass["Sensitivity"]
  precision_scores[i] <- cm$byClass["Precision"]
  accuracy_scores[i]  <- cm$overall["Accuracy"]
}

# 3. Oprindelig og "egen" grænseværdi
best_threshold <- 0.50  # Fast, standard

# 4. Nyt: Hvor F1, Recall og Precision er tættest på hinanden
distance_metric <- function(f1, rec, prec) {
  abs(f1 - rec) + abs(f1 - prec) + abs(rec - prec)
}
f1_recall_precision_diff <- mapply(distance_metric, f1_scores, recall_scores, precision_scores)
balanced_idx <- which.min(f1_recall_precision_diff)
balanced_threshold <- thresholds[balanced_idx]

# 5. Plot
plot(thresholds, f1_scores, type = "l", col = "darkgreen", lwd = 2, ylim = c(0, 1),
     xlab = "Grænseværdi", ylab = "Score", main = "Bagging – valg af grænseværdi")

lines(thresholds, recall_scores, col = "steelblue", lty = 2, lwd = 2)
lines(thresholds, precision_scores, col = "orange", lty = 3, lwd = 2)
lines(thresholds, accuracy_scores, col = "purple", lty = 4, lwd = 2)

# 6. Markér punkter
abline(v = best_threshold, col = "black", lty = 2)
text(best_threshold + 0.01, 0.91, 
     labels = "Oprindelig grænseværdi", pos = 4, col = "black", font = 0.2)

# 7. Nyt punkt hvor F1 ≈ Recall ≈ Precision
abline(v = balanced_threshold, col = "red", lty = 3)
points(balanced_threshold, f1_scores[balanced_idx], col = "red", pch = 19)
text(balanced_threshold + 0.015, f1_scores[balanced_idx] + 0.05,
     labels = paste("Ny grænseværdi:", round(balanced_threshold, 2)),
     col = "red", font = 0.2, pos = 4)

# 8. Forklaring
legend("bottomright",
       legend = c("F1-score", "Recall", "Precision", "Accuracy"),
       col = c("darkgreen", "steelblue", "orange", "purple"),
       lty = 1:4, lwd = 2, cex = 0.5)

#Variablerne
# Hent og forbered data
importance_df_b <- varImp(log_tuned_b)$importance
importance_df_b$Variable <- rownames(importance_df_b)

# Sortér efter betydning
importance_df_b <- importance_df_b %>%
  arrange(Overall)

# Lav linjeplot
ggplot(importance_df_b, aes(x = reorder(Variable, Overall), y = Overall, group = 1)) +
  geom_line(color = "#E31A1C", linewidth = 1, linetype = "dashed") +
  geom_point(aes(color = Overall), size = 4) +
  scale_color_gradient(low = "#FDBBA1", high = "#E31A1C") +
  labs(
    title = "De vigtigste forklarende variabler i den logistiske regression er afstand og vinkel på skuddet",
    x = "Forklarende Variabel",
    y = "Importance (absolut koefficient)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    legend.position = "none"
  )

#Matrice
acc_log_b <- paste0(round(log_tuned_b$overall["Accuracy"] * 100, 1), "%")

# Klargør confusion matrix
cm_log_matrice_b <- as.data.frame(cm_log_b$table)
colnames(cm_log_matrice_b) <- c("Reference", "Prediction", "Freq")

# Sæt rækkefølge: YES nederst
cm_log_matrice_b$Reference <- factor(cm_log_matrice_b$Reference, levels = c("Yes", "No"))
cm_log_matrice_b$Prediction <- factor(cm_log_matrice_b$Prediction, levels = c("No", "Yes"))

# Plot
ggplot(cm_log_matrice_b, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", guide = "none") +  # Fjern farvelegend
  labs(
    title = "Logistisk Regression identificerer 125 mål korrekt",
    subtitle = paste("GOAL = YES, NO GOAL = NO\nAccuracy = 84 %"),
    x = "Prediction",
    y = "Reference"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )

#### Brøndby xG ####
hold_udvalgt_brøndby <- all_matches_dedup %>%
  filter(SEASON_WYID == 189918) %>%
  distinct(TEAM_WYID, OFFICIALNAME)

Samlet_Brøndby <- merge(brøndby_xg, hold_udvalgt_brøndby, by = "TEAM_WYID")
Samlet_Brøndby <- merge(brøndby_xg, Resultat_kamp, by = "MATCH_WYID")
Samlet_Brøndby$xG <- predict(log_tuned_b, newdata = brøndby_xg, type = "prob")[,"Yes"]


#### OPG 2.1 ####
#Laver point og vælger kun Brøndby kampe
set.seed(777)
Brøndby_kampe <- Samlet_players %>%
  mutate(
    Home = str_split_fixed(Kamp, "-", 2)[,1],
    Away = str_split_fixed(Kamp, "-", 2)[,2]
  )

Brøndby_kampe <- Brøndby_kampe %>%
  mutate(
    Resultat = str_trim(Resultat),
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2])
  )

Brøndby_kampe <- Brøndby_kampe %>%
  mutate(
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2]),
    point_home = case_when(
      home_goals > away_goals ~ 3,
      home_goals == away_goals ~ 1,
      home_goals < away_goals ~ 0
    ),
    point_away = case_when(
      away_goals > home_goals ~ 3,
      away_goals == home_goals ~ 1,
      away_goals < home_goals ~ 0
    )
  )

Brøndby_kampe <- Brøndby_kampe %>%
  filter(Home == "Brøndby IF" | Away == "Brøndby IF")


#xP
#xG per hold per kamp
xg_per_team <- Brøndby_kampe %>%
  group_by(MATCH_WYID, OFFICIALNAME, Runde) %>%
  summarise(xG = sum(SHOTXG, na.rm = TRUE), .groups = "drop")

xg_per_match <- xg_per_team %>%
  mutate(
    xG_brondby = if_else(OFFICIALNAME == "Brøndby IF", xG, NA_real_),
    xG_modstander = if_else(OFFICIALNAME != "Brøndby IF", xG, NA_real_),
    modstander = if_else(OFFICIALNAME != "Brøndby IF", OFFICIALNAME, NA_character_)
  ) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    xG_brondby = sum(xG_brondby, na.rm = TRUE),
    xG_modstander = sum(xG_modstander, na.rm = TRUE),
    modstander = first(na.omit(modstander))
  )

#Simulering
simuler_xp <- function(xg_home, xg_away, n = 10000) {
  home_goals <- rpois(n, xg_home)
  away_goals <- rpois(n, xg_away)
  
  home_wins <- sum(home_goals > away_goals)
  draws <- sum(home_goals == away_goals)
  away_wins <- sum(home_goals < away_goals)
  
  xp_home <- (home_wins * 3 + draws * 1) / n
  xp_away <- (away_wins * 3 + draws * 1) / n
  
  return(c(xp_home, xp_away))
}

#xP
xg_xp_resultater <- xg_per_match %>%
  rowwise() %>%
  mutate(
    sim = list(simuler_xp(xG_brondby, xG_modstander)),
    xP_brondby = sim[[1]],
    xP_modstander = sim[[2]]
  ) %>%
  ungroup() %>%
  select(-sim)

#Brøndbys xP
xp_total <- xg_xp_resultater %>%
  summarise(Samlet_xP = sum(xP_brondby))

# Point fra hjemme- og udekampe
faktiske_point <- Brøndby_kampe %>%
  filter(OFFICIALNAME == "Brøndby IF") %>%
  mutate(point = case_when(
    Home == "Brøndby IF" ~ point_home,
    Away == "Brøndby IF" ~ point_away
  )) %>%
  group_by(MATCH_WYID) %>%           # Gruppér pr. kamp
  summarise(point = first(point)) %>%  # Vælg én værdi per kamp
  summarise(Samlet_point = sum(point, na.rm = TRUE))

sammenligning <- bind_cols(xp_total, faktiske_point)
print(sammenligning)

#Graf
xP_brøndby <- xg_xp_resultater %>%
  left_join(
    Brøndby_kampe %>%
      filter(OFFICIALNAME == "Brøndby IF") %>%
      select(MATCH_WYID, Home, Away, point_home, point_away,Runde, Resultat) %>%
      distinct(),
    by = "MATCH_WYID"
  ) %>%
  mutate(
    Brøndby_point = case_when(
      Home == "Brøndby IF" ~ point_home,
      Away == "Brøndby IF" ~ point_away
    ),
    Kamp = paste(Home, "vs", Away)
  )


#### ALLE KAMPE ####
# 1. Tilføj mål og point
Superliga_kampe <- Samlet_players %>%
  mutate(
    Home = str_trim(str_split_fixed(Kamp, "-", 2)[,1]),
    Away = str_trim(str_split_fixed(Kamp, "-", 2)[,2]),
    Resultat = str_trim(Resultat),
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2]),
    point_home = case_when(
      home_goals > away_goals ~ 3,
      home_goals == away_goals ~ 1,
      TRUE ~ 0
    ),
    point_away = case_when(
      away_goals > home_goals ~ 3,
      away_goals == home_goals ~ 1,
      TRUE ~ 0
    )
  )

# 2. xG per hold per kamp
Superliga_xg_team <- Superliga_kampe %>%
  group_by(MATCH_WYID, OFFICIALNAME) %>%
  summarise(xG = sum(SHOTXG, na.rm = TRUE), .groups = "drop")

# 3. Par hold i samme kamp
Superliga_xg_match <- Superliga_xg_team %>%
  inner_join(Superliga_xg_team, by = "MATCH_WYID", suffix = c("_team", "_opp")) %>%
  filter(OFFICIALNAME_team != OFFICIALNAME_opp)

# 4. Simuleringsfunktion
simuler_xp <- function(xg_home, xg_away, n = 10000) {
  home_goals <- rpois(n, xg_home)
  away_goals <- rpois(n, xg_away)
  home_wins <- sum(home_goals > away_goals)
  draws <- sum(home_goals == away_goals)
  (home_wins * 3 + draws * 1) / n
}

# 5. xP per hold
Superliga_xp <- Superliga_xg_match %>%
  rowwise() %>%
  mutate(
    xP = simuler_xp(xG_team, xG_opp),
    Hold = OFFICIALNAME_team,
    Modstander = OFFICIALNAME_opp,
    xG = xG_team,
    xG_modstander = xG_opp
  ) %>%
  ungroup() %>%
  select(MATCH_WYID, Hold, Modstander, xG, xG_modstander, xP)

# 6. Faktiske point via rolle
Superliga_pointdata <- Superliga_kampe %>%
  distinct(MATCH_WYID, Home, Away, point_home, point_away, OFFICIALNAME, Resultat) %>%
  mutate(
    Point = case_when(
      OFFICIALNAME == Home ~ point_home,
      OFFICIALNAME == Away ~ point_away,
      TRUE ~ NA_real_
    )
  ) %>%
  rename(Hold = OFFICIALNAME) %>%
  select(MATCH_WYID, Hold, Point, Resultat)

# 7. Join xP og point
Superliga_xp_full <- Superliga_xp %>%
  left_join(Superliga_pointdata, by = c("MATCH_WYID", "Hold"))

# 8. Totaloversigt
Superliga_summary <- Superliga_xp_full %>%
  filter(!is.na(Point)) %>%
  group_by(Hold) %>%
  summarise(
    xP = sum(xP),
    Points = sum(Point),
    Difference = Points - xP
  ) %>%
  arrange(desc(Points))

# 9. Justering af fejl
Superliga_summary <- Superliga_summary %>%
  mutate(Points = case_when(
    Hold == "FC Midtjylland" ~ 63,
    Hold == "FC København" ~ 59,
    TRUE ~ Points
  )) %>%
  mutate(Difference = Points - xP)

Superliga_summary %>%
  arrange(desc(Points))

# 10. Målstatistik
Superliga_goals <- Superliga_kampe %>%
  distinct(MATCH_WYID, Home, Away, home_goals, away_goals) %>%
  pivot_longer(cols = c(Home, Away), names_to = "HjemUde", values_to = "Hold") %>%
  mutate(
    Goals_For = case_when(HjemUde == "Home" ~ home_goals, TRUE ~ away_goals),
    Goals_Against = case_when(HjemUde == "Home" ~ away_goals, TRUE ~ home_goals)
  ) %>%
  group_by(Hold) %>%
  summarise(
    Goals_For = sum(Goals_For, na.rm = TRUE),
    Goals_Against = sum(Goals_Against, na.rm = TRUE),
    .groups = "drop"
  )

# 11. xG totals per hold
Superliga_xg_total <- Superliga_kampe %>%
  group_by(OFFICIALNAME) %>%
  summarise(xG_total = sum(SHOTXG, na.rm = TRUE), .groups = "drop") %>%
  rename(Hold = OFFICIALNAME)

# 12. Final merge
Superliga_summary <- Superliga_summary %>%
  left_join(Superliga_goals, by = "Hold") %>%
  left_join(Superliga_xg_total, by = "Hold") %>%
  mutate(Difference = Points - xP)

# 13. Resultat
print(Superliga_summary)

Superliga_summary %>%
  arrange(desc(Points)) %>%      # Sorter data først
  gt() %>%
  tab_header(
    title = "FCK havde den højest xP i 2023-2024-sæsonen",
    subtitle = "xP udregnet med Wyscout's egen xG-variable"
  ) %>%
  fmt_number(
    columns = c(xP, Points, Difference, Goals_For, Goals_Against, xG_total),
    decimals = 1
  ) %>%
  data_color(
    columns = vars(Difference),
    colors = col_numeric(
      palette = c("red", "white", "green"),
      domain = range(Superliga_summary$Difference)
    )
  ) %>%
  cols_label(
    Hold = "Hold",
    xP = "xP",
    Points = "Point",
    Difference = "Difference",
    Goals_For = "Scorede mål",
    Goals_Against = "Indkasserede mål",
    xG_total = "xG"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 24,
    heading.subtitle.font.size = 16,
    table.background.color = "white"
  )


#### LOGISTISK REGRESSION ####
# BRØNDBY xP med xG (fra egen model)
Brøndby_kampe_log <- Samlet_players %>%
  mutate(
    Home = str_trim(str_split_fixed(Kamp, "-", 2)[,1]),
    Away = str_trim(str_split_fixed(Kamp, "-", 2)[,2]),
    Resultat = str_trim(Resultat),
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2]),
    point_home = case_when(
      home_goals > away_goals ~ 3,
      home_goals == away_goals ~ 1,
      TRUE ~ 0
    ),
    point_away = case_when(
      away_goals > home_goals ~ 3,
      away_goals == home_goals ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(Home == "Brøndby IF" | Away == "Brøndby IF")

# xG per hold per kamp
xg_per_team_log <- Brøndby_kampe_log %>%
  group_by(MATCH_WYID, OFFICIALNAME, Runde) %>%
  summarise(xG = sum(xG, na.rm = TRUE), .groups = "drop")

# Par hold og beregn xP
xg_per_match_log <- xg_per_team_log %>%
  mutate(
    xG_brondby = if_else(OFFICIALNAME == "Brøndby IF", xG, NA_real_),
    xG_modstander = if_else(OFFICIALNAME != "Brøndby IF", xG, NA_real_),
    modstander = if_else(OFFICIALNAME != "Brøndby IF", OFFICIALNAME, NA_character_)
  ) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    xG_brondby = sum(xG_brondby, na.rm = TRUE),
    xG_modstander = sum(xG_modstander, na.rm = TRUE),
    modstander = first(na.omit(modstander))
  )

# Simuler xP
simuler_xp_log <- function(xg_home, xg_away, n = 10000) {
  home_goals <- rpois(n, xg_home)
  away_goals <- rpois(n, xg_away)
  home_wins <- sum(home_goals > away_goals)
  draws <- sum(home_goals == away_goals)
  away_wins <- sum(home_goals < away_goals)
  xp_home <- (home_wins * 3 + draws * 1) / n
  xp_away <- (away_wins * 3 + draws * 1) / n
  return(c(xp_home, xp_away))
}

# Beregn xP
xg_xp_resultater_log <- xg_per_match_log %>%
  rowwise() %>%
  mutate(
    sim = list(simuler_xp_log(xG_brondby, xG_modstander)),
    xP_brondby = sim[[1]],
    xP_modstander = sim[[2]]
  ) %>%
  ungroup() %>%
  select(-sim)

# Total xP for Brøndby
xp_total_log <- xg_xp_resultater_log %>%
  summarise(xP = sum(xP_brondby))

# Faktiske point
faktiske_point_log <- Brøndby_kampe_log %>%
  filter(OFFICIALNAME == "Brøndby IF") %>%
  mutate(point = case_when(
    Home == "Brøndby IF" ~ point_home,
    Away == "Brøndby IF" ~ point_away
  )) %>%
  group_by(MATCH_WYID) %>%
  summarise(point = first(point)) %>%
  summarise(Points = sum(point, na.rm = TRUE))

# Sammenlign
sammenligning_log <- bind_cols(xp_total_log, faktiske_point_log)
print(sammenligning_log)

xP_brøndby_log <- xg_xp_resultater_log %>%
  left_join(
    Brøndby_kampe %>%
      filter(OFFICIALNAME == "Brøndby IF") %>%
      select(MATCH_WYID, Home, Away, point_home, point_away,Runde, Resultat) %>%
      distinct(),
    by = "MATCH_WYID"
  ) %>%
  mutate(
    Brøndby_point = case_when(
      Home == "Brøndby IF" ~ point_home,
      Away == "Brøndby IF" ~ point_away
    ),
    Kamp = paste(Home, "vs", Away)
  )


#### ALLE KAMPE MED LOG ####
# 1. Klargør kampdata
Superliga_kampe_log <- Samlet_players %>%
  mutate(
    Home = str_trim(str_split_fixed(Kamp, "-", 2)[,1]),
    Away = str_trim(str_split_fixed(Kamp, "-", 2)[,2]),
    Resultat = str_trim(Resultat),
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2]),
    point_home = case_when(
      home_goals > away_goals ~ 3,
      home_goals == away_goals ~ 1,
      TRUE ~ 0
    ),
    point_away = case_when(
      away_goals > home_goals ~ 3,
      away_goals == home_goals ~ 1,
      TRUE ~ 0
    )
  )

# 2. xG per hold per kamp
Superliga_xg_team_log <- Superliga_kampe_log %>%
  group_by(MATCH_WYID, OFFICIALNAME) %>%
  summarise(xG = sum(xG, na.rm = TRUE), .groups = "drop")

# 3. Par hold i samme kamp
Superliga_xg_match_log <- Superliga_xg_team_log %>%
  inner_join(Superliga_xg_team_log, by = "MATCH_WYID", suffix = c("_team", "_opp")) %>%
  filter(OFFICIALNAME_team != OFFICIALNAME_opp)

# 4. Simuleringsfunktion
simuler_xp_log <- function(xg_home, xg_away, n = 10000) {
  home_goals <- rpois(n, xg_home)
  away_goals <- rpois(n, xg_away)
  home_wins <- sum(home_goals > away_goals)
  draws <- sum(home_goals == away_goals)
  (home_wins * 3 + draws * 1) / n
}

# 5. xP per hold
Superliga_xp_log <- Superliga_xg_match_log %>%
  rowwise() %>%
  mutate(
    xP = simuler_xp_log(xG_team, xG_opp),
    Hold = OFFICIALNAME_team,
    Modstander = OFFICIALNAME_opp,
    xG = xG_team,
    xG_modstander = xG_opp
  ) %>%
  ungroup() %>%
  select(MATCH_WYID, Hold, Modstander, xG, xG_modstander, xP)

# 6. Faktiske point
Superliga_pointdata_log <- Superliga_kampe_log %>%
  distinct(MATCH_WYID, Home, Away, point_home, point_away, OFFICIALNAME, Resultat) %>%
  mutate(
    Point = case_when(
      OFFICIALNAME == Home ~ point_home,
      OFFICIALNAME == Away ~ point_away,
      TRUE ~ NA_real_
    )
  ) %>%
  rename(Hold = OFFICIALNAME) %>%
  select(MATCH_WYID, Hold, Point, Resultat)

# 7. Join xP og Point
Superliga_xp_full_log <- Superliga_xp_log %>%
  left_join(Superliga_pointdata_log, by = c("MATCH_WYID", "Hold"))

# 8. Totaloversigt
Superliga_summary_log <- Superliga_xp_full_log %>%
  filter(!is.na(Point)) %>%
  group_by(Hold) %>%
  summarise(
    xP = sum(xP),
    Points = sum(Point),
    Difference = Points - xP,
    .groups = "drop"
  )

# 9. Målstatistik
Superliga_goals_log <- Superliga_kampe_log %>%
  distinct(MATCH_WYID, Home, Away, home_goals, away_goals) %>%
  pivot_longer(cols = c(Home, Away), names_to = "HjemUde", values_to = "Hold") %>%
  mutate(
    Goals_For = case_when(HjemUde == "Home" ~ home_goals, TRUE ~ away_goals),
    Goals_Against = case_when(HjemUde == "Home" ~ away_goals, TRUE ~ home_goals)
  ) %>%
  group_by(Hold) %>%
  summarise(
    Goals_For = sum(Goals_For, na.rm = TRUE),
    Goals_Against = sum(Goals_Against, na.rm = TRUE),
    .groups = "drop"
  )

# 10. xG totals per hold
Superliga_xg_total_log <- Superliga_kampe_log %>%
  group_by(OFFICIALNAME) %>%
  summarise(xG_total = sum(xG, na.rm = TRUE), .groups = "drop") %>%
  rename(Hold = OFFICIALNAME)

# 11. Final merge
Superliga_summary_log <- Superliga_summary_log %>%
  left_join(Superliga_goals_log, by = "Hold") %>%
  left_join(Superliga_xg_total_log, by = "Hold")

# 11b. Justering af point (manuel rettelse)
Superliga_summary_log <- Superliga_summary_log %>%
  mutate(Points = case_when(
    Hold == "FC Midtjylland" ~ 63,
    Hold == "FC København" ~ 59,
    TRUE ~ Points
  )) %>%
  mutate(Difference = Points - xP)

# 12. Vis resultater i gt()
Superliga_summary_log %>%
  arrange(desc(Points)) %>%
  gt() %>%
  tab_header(
    title = "FCK havde den højest xP i 2023-2024-sæsonen",
    subtitle = "xP udregnet med egen xG-variable (logistisk regression)"
  ) %>%
  fmt_number(
    columns = c(xP, Points, Difference, Goals_For, Goals_Against, xG_total),
    decimals = 1
  ) %>%
  data_color(
    columns = vars(Difference),
    colors = col_numeric(
      palette = c("red", "white", "green"),
      domain = range(Superliga_summary_log$Difference, na.rm = TRUE)
    )
  ) %>%
  cols_label(
    Hold = "Hold",
    xP = "xP",
    Points = "Point",
    Difference = "Difference",
    Goals_For = "Scorede mål",
    Goals_Against = "Indkasserede mål",
    xG_total = "xG"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 24,
    heading.subtitle.font.size = 16,
    table.background.color = "white"
  )








#### Opg. 2.2 ####

#Nyt data med alle Brøndbys events
Brøndby_offside <- merge(all_events_unique, players_udvalgt, by = "PLAYER_WYID")

Brøndby_offside <- merge(Brøndby_offside, hold_udvalgt, by = "TEAM_WYID")

Brøndby_offside <- merge(Brøndby_offside, Resultat_kamp, by = "MATCH_WYID")

Brøndby_offside <- merge(Brøndby_offside, Runde_id, by = "MATCH_WYID")


Brøndby_offside <- Brøndby_offside %>%
  mutate(
    Home = str_split_fixed(Kamp, "-", 2)[,1],
    Away = str_split_fixed(Kamp, "-", 2)[,2]
  )

Brøndby_offside <- Brøndby_offside %>%
  mutate(
    Resultat = str_trim(Resultat),
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2])
  )

Brøndby_offside <- Brøndby_offside %>%
  mutate(
    home_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,1]),
    away_goals = as.numeric(str_split_fixed(Resultat, "-", 2)[,2]),
    point_home = case_when(
      home_goals > away_goals ~ 3,
      home_goals == away_goals ~ 1,
      home_goals < away_goals ~ 0
    ),
    point_away = case_when(
      away_goals > home_goals ~ 3,
      away_goals == home_goals ~ 1,
      away_goals < home_goals ~ 0
    )
  )

library(dplyr)
library(lubridate)

Brøndby_offside <- Brøndby_offside %>%
  filter(Home == "Brøndby IF" | Away == "Brøndby IF")

Brøndby_offside_test <- Brøndby_offside %>%
  select(MATCH_WYID, TEAM_WYID, EVENT_WYID, MATCHTIMESTAMP, PRIMARYTYPE, Kamp) %>%
  mutate(MATCHTIMESTAMP = hms(MATCHTIMESTAMP))

offsides <- Brøndby_offside_test %>%
  filter(PRIMARYTYPE == "offside")

# Find sidste to events før offside
pre_offside_events <- lapply(seq_len(nrow(offsides)), function(i) {
  match_id <- offsides$MATCH_WYID[i]
  time <- offsides$MATCHTIMESTAMP[i]
  
  events_before <- Brøndby_offside_test %>%
    filter(MATCH_WYID == match_id, MATCHTIMESTAMP < time) %>%
    arrange(desc(MATCHTIMESTAMP)) %>%
    slice_head(n = 2) %>%
    mutate(offside_index = i)
  
  # Returnér kun hvis én af dem er "shot"
  if ("shot" %in% events_before$PRIMARYTYPE) {
    return(events_before)
  } else {
    return(NULL)
  }
})

# Bind alle events, der opfylder betingelsen
pre_offside_df <- bind_rows(pre_offside_events)

# Tilføj selve offside-hændelsen til hver gruppe
offsides_matched <- offsides %>%
  mutate(offside_index = row_number()) %>%
  filter(offside_index %in% pre_offside_df$offside_index)

pre_offside_df <- bind_rows(pre_offside_df, offsides_matched) %>%
  arrange(MATCH_WYID, offside_index, MATCHTIMESTAMP)

# Tjek hvilke event-typer der optræder i disse sekvenser
table(pre_offside_df$PRIMARYTYPE)


#Fjerner de events som er "offside". 

events_to_remove <- c(2195331908, 2093623533, 1804962021, 
                      1716416920, 1702114900, 1698169540,2126977662)

Brøndby_kampe_log_clean <- Brøndby_kampe_log %>%
  filter(!EVENT_WYID %in% events_to_remove)

Brøndby_kampe_log %>%
  filter(EVENT_WYID %in% events_to_remove) %>%
  mutate(Hold = if_else(TEAM_WYID == 7453, "Brøndby IF", "Modstander")) %>%
  group_by(Hold) %>%
  summarise(
    Antal = n(),
    mean_xG = mean(xG, na.rm = TRUE),
    total_xG = sum(xG, na.rm = TRUE),
    mål = sum(SHOTISGOAL == 1, na.rm = TRUE),
    .groups = "drop"
  )

#xG per hold per kamp
xg_per_team_log_clean <- Brøndby_kampe_log_clean %>%
  group_by(MATCH_WYID, OFFICIALNAME, Runde) %>%
  summarise(xG = sum(xG, na.rm = TRUE), .groups = "drop")

# Par hold og beregn xP (renset)
xg_per_match_log_clean <- xg_per_team_log_clean %>%
  mutate(
    xG_brondby = if_else(OFFICIALNAME == "Brøndby IF", xG, NA_real_),
    xG_modstander = if_else(OFFICIALNAME != "Brøndby IF", xG, NA_real_),
    modstander = if_else(OFFICIALNAME != "Brøndby IF", OFFICIALNAME, NA_character_)
  ) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    xG_brondby = sum(xG_brondby, na.rm = TRUE),
    xG_modstander = sum(xG_modstander, na.rm = TRUE),
    modstander = first(na.omit(modstander))
  )

# Funktion til simulation af xP (genbruges)
simuler_xp_log <- function(xg_home, xg_away, n = 10000) {
  home_goals <- rpois(n, xg_home)
  away_goals <- rpois(n, xg_away)
  home_wins <- sum(home_goals > away_goals)
  draws <- sum(home_goals == away_goals)
  away_wins <- sum(home_goals < away_goals)
  xp_home <- (home_wins * 3 + draws * 1) / n
  xp_away <- (away_wins * 3 + draws * 1) / n
  return(c(xp_home, xp_away))
}

# Beregn xP på det rensede datasæt
xg_xp_resultater_log_clean <- xg_per_match_log_clean %>%
  rowwise() %>%
  mutate(
    sim = list(simuler_xp_log(xG_brondby, xG_modstander)),
    xP_brondby = sim[[1]],
    xP_modstander = sim[[2]]
  ) %>%
  ungroup() %>%
  select(-sim)

# Total xP for Brøndby (renset)
xp_total_log_clean <- xg_xp_resultater_log_clean %>%
  summarise(xP = sum(xP_brondby))

# Faktiske point (fra originalt datasæt – ikke ændret)
faktiske_point_log <- Brøndby_kampe_log %>%
  filter(OFFICIALNAME == "Brøndby IF") %>%
  mutate(point = case_when(
    Home == "Brøndby IF" ~ point_home,
    Away == "Brøndby IF" ~ point_away
  )) %>%
  group_by(MATCH_WYID) %>%
  summarise(point = first(point)) %>%
  summarise(Points = sum(point, na.rm = TRUE))

# Sammenligning: xP (renset) vs faktiske point
sammenligning_log_clean <- bind_cols(xp_total_log_clean, faktiske_point_log)
print(sammenligning_log_clean)

# Resultater pr. kamp for Brøndby (renset)
xP_brøndby_log_clean <- xg_xp_resultater_log_clean %>%
  left_join(
    Brøndby_kampe %>%
      filter(OFFICIALNAME == "Brøndby IF") %>%
      select(MATCH_WYID, Home, Away, point_home, point_away, Runde, Resultat) %>%
      distinct(),
    by = "MATCH_WYID"
  ) %>%
  mutate(
    Brøndby_point = case_when(
      Home == "Brøndby IF" ~ point_home,
      Away == "Brøndby IF" ~ point_away
    ),
    Kamp = paste(Home, "vs", Away)
  )


#Fjerner skuddet fra Wass
Brøndby_Silkerborg <- Brøndby_offside %>%
  select(MATCH_WYID, TEAM_WYID, EVENT_WYID, MATCHTIMESTAMP, PRIMARYTYPE, Kamp, Resultat) %>%
  mutate(MATCHTIMESTAMP = hms(MATCHTIMESTAMP))

Brøndby_Silkerborg <- Brøndby_offside %>%
  filter(MATCH_WYID == 5466044)

Brøndby_Silkerborg_Wass <- Brøndby_offside %>%
  filter(EVENT_WYID == 2126977662)












