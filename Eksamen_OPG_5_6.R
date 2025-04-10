library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(factoextra)
library(plotly)
library(purrr)

#### MongoDB connection ####
con_fe=mongo(
  collection = "events",
  db = "WomenSoccer2",
  url= "mongodb://localhost"
)

conm=mongo(
  collection = "matches",
  db = "WomenSoccer2",
  url= "mongodb://localhost"
)
conp=mongo(
  collection = "lineups",
  db = "WomenSoccer2",
  url= "mongodb://localhost"
)

con_ma=mongo(
  collection = "maleevents",
  db = "WomenSoccer2",
  url= "mongodb://localhost"
)

query <- '{ 
  "$or": [
    { "home_team.home_team_gender": "female" },
    { "away_team.away_team_gender": "female" }
  ]
}'

# Hent data fra MongoDB
female_matches <- conm$find(query)

female_matches <- fromJSON(toJSON(female_matches), flatten = TRUE)

#season.season_id = 90 - England 20/21

query_1 <- '{ 
  "$or": [
    { "home_team.home_team_gender": "male" },
    { "away_team.away_team_gender": "male" }
  ]
}'

# Hent data fra MongoDB
male_matches <- conm$find(query_1)

male_matches <- fromJSON(toJSON(male_matches), flatten = TRUE)

#Kvinde VM
WorldCup_female <- female_matches %>% 
  filter((competition.competition_name == "Women's World Cup"),
         season.season_id == "107")

#Herre VM
WorldCup_male <- male_matches %>% 
  filter((competition.competition_name == "FIFA World Cup"),
         season.season_id == "106")

#Kvinde events
match_ids_fe <- c(3906390, 3893806, 3893822, 3906389, 3904629, 3901797, 3901796, 3902240, 
                  3893814, 3902239, 3893808, 3901833, 3893826, 3901735, 3901733, 3893801, 
                  3901734, 3893823, 3901736, 3893811, 3893813, 3893812, 3904628, 3893810, 
                  3893809, 3902968, 3893797, 3893790, 3893804, 3893788, 3893807, 3902967, 
                  3893789, 3893791, 3893792, 3893798, 3893834, 3893832, 3893827, 3893794, 
                  3893833, 3893824, 3893831, 3893830, 3893787, 3893819, 3893818, 3893825, 
                  3893817, 3893816, 3893828, 3893815, 3893805, 3901832, 3893803, 3893829, 
                  3893802, 3893800, 3893799, 3893796, 3893795, 3893793, 3893821, 3893820)

query_fe <- sprintf('{
  "match_id": { "$in": [%s] }
}', paste(match_ids_fe, collapse = ","))

Events_female <- con_fe$find(query_fe)
Events_female <- fromJSON(toJSON(Events_female), flatten = TRUE)



#### Indhentet så freezeframe kolonne virker - Kvinder ####

Events_female_raw <- con_fe$find(query_fe)

# Filtrer rows hvor shot.freeze_frame ikke er NULL
Events_with_freeze <- Events_female_raw %>%
  filter(!map_lgl(shot$freeze_frame, is.null))


Events_with_freeze_flat <- fromJSON(toJSON(Events_with_freeze), flatten = TRUE)


england_events_female <- Events_with_freeze_flat %>%
  filter(team.name == "England Women's")



#### Indhenter så freezeframe kolonne virker - Mænd ####
#Herre events
match_ids_ma <- c(3857256, 3869321, 3857288, 3869151, 3857258, 3857287, 3857267, 3857257, 
                  3869486, 3869685, 3857266, 3857264, 3857260, 3857289, 3857294, 3857269, 
                  3869254, 3869118, 3869684, 3869519, 3869354, 3869552, 3869420, 3869220, 
                  3869253, 3869219, 3869152, 3869117, 3857270, 3857263, 3857259, 3857283, 
                  3857284, 3857295, 3857301, 3857299, 3857300, 3857297, 3857285, 3857282, 
                  3857290, 3857286, 3857281, 3857298, 3857278, 3857279, 3857277, 3857296, 
                  3857276, 3857275, 3857271, 3857268, 3857265, 3857262, 3857261, 3857255, 
                  3857274, 3857292, 3857254, 3857273, 3857272, 3857293, 3857291, 3857280)

query_ma <- sprintf('{
  "matchId": { "$in": [%s] }
}', paste(match_ids_ma, collapse = ","))

Events_male <- con_ma$find(query_ma)
Events_male_raw <- con_ma$find(query_ma)

# Filtrer rows hvor shot.freeze_frame ikke er NULL
Events_with_freeze_male <- Events_male_raw %>%
  filter(!map_lgl(shot$freeze_frame, is.null))


Events_with_freeze_male_flat <- fromJSON(toJSON(Events_with_freeze_male), flatten = TRUE)


england_events_male <- Events_with_freeze_male_flat %>%
  filter(team.name == "England")




Events_male <- fromJSON(toJSON(Events_male), flatten = TRUE)


#Hvilke events skal vi sammenligne?
#type.name(Pass, Ball Recovery, Foul Committed, Dribble & Dribbled Past, Miscontrol, Shot, Bad Behaviour, Error, Offside)

#Vi laver grafer for kvinder og herre og sammenligner dem på turneringsnivuae, men også på land kun med Danmark
#Vi skal også kigge på koordinaterne (Se script KvindeEM)

#### Pass ####
#Ingen "pass.outcome" registreret → Hvis feltet "pass.outcome" ikke findes, betragtes afleveringen som succesfuld.
Events_male <- Events_male %>%
  mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete"))

Events_male <- Events_male %>%
  mutate(
    location.x = map_dbl(location, ~ if(length(.x) >= 2) .x[[1]] else NA_real_),
    location.y = map_dbl(location, ~ if(length(.x) >= 2) .x[[2]] else NA_real_),
    
    pass.end_location.x = map_dbl(pass.end_location, ~ if(length(.x) >= 2) .x[[1]] else NA_real_),
    pass.end_location.y = map_dbl(pass.end_location, ~ if(length(.x) >= 2) .x[[2]] else NA_real_),
    
    shot.end_location.x = map_dbl(shot.end_location, ~ if(length(.x) >= 2) .x[[1]] else NA_real_),
    shot.end_location.y = map_dbl(shot.end_location, ~ if(length(.x) >= 2) .x[[2]] else NA_real_)
  )


#### Vinkel & distance på skud - Kvinder ####
# Antag at målene er placeret ved (0, 50) og (100, 50)
goal2_x <- 100
goal2_y <- 50

# Feature Creation: Beregn afstand og vinkel til begge mål
Events_male <- Events_male %>%
  mutate(
    distance_to_goal = if_else(type.name == "Shot", 
                               sqrt((location.x - goal2_x)^2 + (location.y - goal2_y)^2), 
                               NA_real_),
    angle_to_goal = if_else(type.name == "Shot", 
                            atan2(abs(location.y - goal2_y), abs(location.x - goal2_x)), 
                            NA_real_)
  )

#Laver det til grader
Events_male <- Events_male %>%
  mutate(angle_to_goal = angle_to_goal * (180 / pi))


#Kvinder
Events_female <- Events_female %>%
  mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete"))

Events_female <- Events_female %>%
  mutate(
    location.x = map_dbl(location, ~ if(length(.x) >= 2) .x[[1]] else NA_real_),
    location.y = map_dbl(location, ~ if(length(.x) >= 2) .x[[2]] else NA_real_),
    
    pass.end_location.x = map_dbl(pass.end_location, ~ if(length(.x) >= 2) .x[[1]] else NA_real_),
    pass.end_location.y = map_dbl(pass.end_location, ~ if(length(.x) >= 2) .x[[2]] else NA_real_),
    
    shot.end_location.x = map_dbl(shot.end_location, ~ if(length(.x) >= 2) .x[[1]] else NA_real_),
    shot.end_location.y = map_dbl(shot.end_location, ~ if(length(.x) >= 2) .x[[2]] else NA_real_)
  )


#### Vinkel & distance på skud - Mænd ####
# Feature Creation: Beregn afstand og vinkel til begge mål
Events_female <- Events_female %>%
  mutate(
    distance_to_goal = if_else(type.name == "Shot", 
                               sqrt((location.x - goal2_x)^2 + (location.y - goal2_y)^2), 
                               NA_real_),
    angle_to_goal = if_else(type.name == "Shot", 
                            atan2(abs(location.y - goal2_y), abs(location.x - goal2_x)), 
                            NA_real_)
  )


#Laver det til grader
Events_female <- Events_female %>%
  mutate(angle_to_goal = angle_to_goal * (180 / pi))


# Opret nyt dataset for mænd
colnames(Events_male)[colnames(Events_male) == "matchId"] <- "match_id"

selected_columns <- c(
  "match_id", "timestamp", "location.x", "location.y", "type.name", "duration",
  "pass.length", "pass.angle", "pass.cross", "pass.through_ball", "pass.goal_assist", "pass.outcome.name", "pass.height.name", "pass.end_location.x", "pass.end_location.y",
  "shot.statsbomb_xg", "shot.type.name", "shot.outcome.name", "play_pattern.name", "shot.deflected", "shot.open_goal", "shot.outcome.name",
  "shot.end_location.x", "shot.end_location.y", "distance_to_goal", "angle_to_goal",
  "interception.outcome.name", "block.deflection", "clearance.head", "duel.outcome.name", "foul_committed.type.name",
  "goalkeeper.shot_saved_to_post", "goalkeeper.shot_saved_off_target", "goalkeeper.lost_in_play",
  "dribble.nutmeg", "dribble.outcome.name", "carry.end_location",
  "foul_committed.card.name", "bad_behaviour.card.name",
  "substitution.outcome.name", "tactics.formation"
)

Events_male_nye <- Events_male %>%
  select(all_of(selected_columns)) %>%
  mutate(gender = "Male")

male_matches_nye <- male_matches %>%
  select(match_id, match_date, 
         home_team.country.name, away_team.country.name, 
         home_score, away_score)

Samlet_male <- Events_male_nye %>%
  left_join(male_matches_nye, by = "match_id")

# Opret nyt dataset for kvinder
Events_female_nye <- Events_female %>%
  select(all_of(selected_columns)) %>%
  mutate(gender = "Female")

female_matches_nye <- female_matches %>%
  select(match_id, match_date, 
         home_team.country.name, away_team.country.name, 
         home_score, away_score)

Samlet_female <- Events_female_nye %>%
  left_join(female_matches_nye, by = "match_id")


# Kombiner de to datasets
Samlet <- bind_rows(Samlet_male, Samlet_female)

#### Grafer - afleveringer ####
#Succes
# Beregn absolutte og relative tal for pasninger
Afleveringer_succes <- Samlet %>%
  filter(type.name == "Pass") %>%
  mutate(pass_group = ifelse(pass.outcome.name == "Complete", "Complete", "Incomplete")) %>%
  group_by(gender, pass_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Plot af absolutte tal (antal pasninger)
ggplot(Afleveringer_succes, aes(x = pass_group, y = count, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "Mænd har et højere antal afleveringer",
       x = "Afleveringer",
       y = "Antal afleveringerr") +
  theme_minimal()

# Plot af relative tal (procentvis fordeling)
ggplot(Afleveringer_succes, aes(x = pass_group, y = percentage, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "Mænd har en højere afleveringsprocent",
       x = "Afleveringer", 
       y = "Procent") +
  theme_minimal()


#### Graf - Længde ####
Samlet_summary <- Samlet %>%
  group_by(gender) %>%
  summarise(mean_pass_length = mean(pass.length, na.rm = TRUE))

ggplot(Samlet_summary, aes(x = gender, y = mean_pass_length, fill = gender)) +
  geom_col() +
  labs(title = "Den gennemsnitslige afleveringslængde er næsten identisk", x = "Køn", y = "Gennemsnitlig pasningslængde") +
  theme_minimal()


#Type aflevering
Pass_height_counts <- Samlet %>%
  filter(!is.na(pass.height.name)) %>%  # Fjerner NA-værdier
  group_by(gender, pass.height.name) %>%
  summarise(count = n(), .groups = "drop")

# Plot søjlediagram
ggplot(Pass_height_counts, aes(x = pass.height.name, y = count, fill = gender)) +
  geom_col(position = "dodge") +  # Søjler side om side
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +  # Tilføj tal på søjlerne
  labs(title = "Kvinder laver flere høje afleveringer",
       x = "Type aflevering",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Graf - Skud ####
#Antal skud og mål
# Beregn absolutte og relative tal for skud
Shot_counts <- Samlet %>%
  filter(type.name == "Shot") %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(relative = count / sum(count) * 100)  # Beregn procentandel

# Plot af absolutte tal (antal skud)
ggplot(Shot_counts, aes(x = gender, y = count, fill = gender)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5, size = 5) +
  labs(title = "Kvinder tager flere skud",
       x = "Køn",
       y = "Antal skud") +
  theme_minimal()

# Plot af relative tal (procent)
ggplot(Shot_counts, aes(x = gender, y = relative, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(relative, 1), "%")), vjust = -0.5, size = 5) +
  labs(title = "Kvinder tager flere skud",
       x = "Køn",
       y = "Andel skud (%)") +
  theme_minimal()


#### Graf - Frispark ####
# Beregn absolutte og relative tal for frispark
Foul_counts <- Samlet %>%
  filter(type.name == "Foul Committed") %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(relative = count / sum(count) * 100)  # Beregn procentandel

# Plot af absolutte tal
ggplot(Foul_counts, aes(x = gender, y = count, fill = gender)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5, size = 5) +
  labs(title = "Mænd laver flere frispark",
       x = "Køn",
       y = "Antal frispark") +
  theme_minimal()

# Plot af relative tal (procent)
ggplot(Foul_counts, aes(x = gender, y = relative, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(relative, 1), "%")), vjust = -0.5, size = 5) +
  labs(title = "Mænd laver flere frispark",
       x = "Køn",
       y = "Andel frispark (%)") +
  theme_minimal()


#### Graf - Kort ####
# Saml data for kort fra frispark og dårlig opførsel
Kort_counts <- Samlet %>%
  filter(!is.na(foul_committed.card.name) | !is.na(bad_behaviour.card.name)) %>%
  mutate(card_type = case_when(
    !is.na(foul_committed.card.name) ~ foul_committed.card.name,
    !is.na(bad_behaviour.card.name) ~ bad_behaviour.card.name
  )) %>%
  group_by(gender, card_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Plot af absolutte tal (antal kort)
ggplot(Kort_counts, aes(x = card_type, y = count, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "Mænd får flere gule kort",
       x = "Korttype",
       y = "Antal kort") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Graf - Driblinger ####
# Filtrér kun driblinger og tæl Complete vs Incomplete
Dribble_counts <- Samlet %>%
  filter(type.name == "Dribble") %>%
  group_by(gender, dribble.outcome.name) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Plot af absolutte tal (antal driblinger)
ggplot(Dribble_counts, aes(x = dribble.outcome.name, y = count, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "Kvinder laver flere driblinger",
       x = "Dribling udfald",
       y = "Antal driblinger") +
  theme_minimal()

# Plot af relative tal (procentvis fordeling)
ggplot(Dribble_counts, aes(x = dribble.outcome.name, y = percentage, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +  # Fjernet fejlagtig + efter size
  labs(title = "Kvinder laver flere driblinger",
       x = "Dribling udfald",
       y = "Antal driblinger") +
  theme_minimal()



#### Graf - Beregn antal mål for mænd og kvinder, unikt per match ####
Goal_counts <- Samlet %>%
  group_by(match_id, gender) %>%
  summarise(
    match_goals = max(home_score, na.rm = TRUE) + max(away_score, na.rm = TRUE),  # Tæl mål én gang per match_id
    .groups = "drop"
  ) %>%
  group_by(gender) %>%
  summarise(
    total_goals = sum(match_goals),
    .groups = "drop"
  ) %>%
  mutate(percentage = (total_goals / sum(total_goals)) * 100)

# Plot af absolutte mål
ggplot(Goal_counts, aes(x = gender, y = total_goals, fill = gender)) +
  geom_col() +
  geom_text(aes(label = total_goals), vjust = -0.5, size = 5) +
  labs(title = "Mænd scorer flere mål",
       x = "Køn",
       y = "Antal mål") +
  theme_minimal()

# Plot af relative mål (procent)
ggplot(Goal_counts, aes(x = gender, y = percentage, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 5) +
  labs(title = "Mænd scorer flere mål",
       x = "Køn", 
       y = "Procent") +
  theme_minimal()

#### Graf - Type skud ####
# Filtrér kun skud og tæl forekomster af shot.type.name per køn
Shot_type_counts <- Samlet %>%
  filter(type.name == "Shot") %>%
  group_by(gender, shot.type.name) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Plot af absolutte tal (antal skud per type)
ggplot(Shot_type_counts, aes(x = shot.type.name, y = count, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "De fleste skud kommer fra åbent spil",
       x = "Skudtype",
       y = "Antal skud") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot af relative tal (procentvis fordeling)
ggplot(Shot_type_counts, aes(x = shot.type.name, y = percentage, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "De fleste skud kommer fra åbent spil",
       x = "Skudtype", 
       y = "Procent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Alle Events - England på male og female (Gik til VM semi og VM finale) ####
Samlet_male_england <- Samlet_male %>% 
  filter(home_team.country.name == "England" | away_team.country.name == "England")

Samlet_female_england <- Samlet_female %>% 
  filter(home_team.country.name == "England" | away_team.country.name == "England")


#### Samler type.name til Shot hos male og female - England ####
Events_female_shots <- Samlet_female_england %>% 
  filter(type.name == "Shot")

Events_male_shots <- Samlet_male_england %>% 
  filter(type.name == "Shot")

#### Female shots count ####
shot_outcome_counts_female <- Events_female_shots %>%
  group_by(shot.outcome.name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(shot.outcome.name = factor(shot.outcome.name, levels = shot.outcome.name))

ggplot(shot_outcome_counts_female, aes(x = shot.outcome.name, y = count, fill = shot.outcome.name)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Antal af forskellige Shot Outcomes",
       x = "Shot Outcome Type",
       y = "Antal skud") +
  theme_minimal()

#### Male shots count - Plot ####
shot_outcome_counts_male <- Events_male_shots %>%
  group_by(shot.outcome.name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(shot.outcome.name = factor(shot.outcome.name, levels = shot.outcome.name))

ggplot(shot_outcome_counts_male, aes(x = shot.outcome.name, y = count, fill = shot.outcome.name)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Antal af forskellige Shot Outcomes",
       x = "Shot Outcome Type",
       y = "Antal skud") +
  theme_minimal()

#### Fjerner alle NA kolonner ####
# For Events_female_shots
Events_female_shots_clean <- Events_female_shots %>% 
  select_if(~ !any(is.na(.)))

# For Events_male_shots
Events_male_shots_clean <- Events_male_shots %>% 
  select_if(~ !any(is.na(.)))


#### Matcher navne op med match-id og timestamp på de givne skud ####
Events_male_shots_clean <- Events_male_shots_clean %>%
  left_join(Events_male %>% select(match_id, timestamp, player.name),
            by = c("match_id", "timestamp"))

Events_female_shots_clean <- Events_female_shots_clean %>%
  left_join(Events_female %>% select(match_id, timestamp, player.name),
            by = c("match_id", "timestamp"))


#### Tilføjer home og away team til datasæt - Kvinder ####
# 1. Filtrér Samlet_female til kun match_id'er som findes i england_events_female
team_info_female <- Samlet_female %>%
  filter(match_id %in% unique(england_events_female$match_id)) %>%
  select(
    match_id,
    home_team = `home_team.country.name`,
    away_team = `away_team.country.name`,
    home_score = `home_score`,
    away_score = `away_score`
  ) %>%
  distinct()  # én række per match_id

# 2. Join til england_events_female (bevarer antal rækker dér)
england_events_female <- england_events_female %>%
  left_join(team_info_female, by = "match_id")

#### Tilføjer home og away team til datasæt - Mænd ####
england_events_male <- england_events_male %>%
  rename(match_id = matchId)


team_info_male <- Samlet_male %>%
  filter(match_id %in% unique(england_events_male$match_id)) %>%
  select(
    match_id,
    home_team = `home_team.country.name`,
    away_team = `away_team.country.name`,
    home_score = `home_score`,
    away_score = `away_score`
  ) %>%
  distinct()  # én række per match_id

# 2. Join til england_events_female (bevarer antal rækker dér)
england_events_male <- england_events_male %>%
  left_join(team_info_male, by = "match_id")


#### Plot til andel af skud til aflevering - Male vs Female ####
# Funktion til point-in-triangle #
point_in_triangle <- function(px, py, triangle) {
  point.in.polygon(
    point.x = px,
    point.y = py,
    pol.x = triangle$x,
    pol.y = triangle$y
  ) > 0
}

# Analysefunktion for ét skud #
analyze_shot <- function(event_row, gender) {
  if (is.null(event_row$shot.freeze_frame[[1]])) {
    return(tibble(gender = gender, should_pass = NA))
  }
  
  freeze_frame <- event_row$shot.freeze_frame[[1]] %>%
    mutate(
      x = map_dbl(location, 1),
      y = map_dbl(location, 2),
      teammate = teammate
    )
  
  shooter <- tibble(
    x = event_row$location[[1]][1],
    y = event_row$location[[1]][2]
  )
  
  left_post <- tibble(x = 120, y = 36)
  right_post <- tibble(x = 120, y = 44)
  
  triangle_shooter <- tibble(
    x = c(shooter$x, left_post$x, right_post$x),
    y = c(shooter$y, left_post$y, right_post$y)
  )
  
  red <- freeze_frame %>% filter(!teammate)
  blue <- freeze_frame %>% filter(teammate)
  
  defenders_in_shooter <- sum(mapply(point_in_triangle, red$x, red$y, MoreArgs = list(triangle = triangle_shooter)))
  
  blue$defenders_in_triangle <- map2_int(blue$x, blue$y, function(px, py) {
    triangle_mate <- tibble(x = c(px, left_post$x, right_post$x), y = c(py, left_post$y, right_post$y))
    sum(mapply(point_in_triangle, red$x, red$y, MoreArgs = list(triangle = triangle_mate)))
  })
  
  if (nrow(blue) == 0) {
    return(tibble(gender = gender, should_pass = FALSE))  # ingen at aflevere til
  }
  
  best_mate <- blue %>% filter(defenders_in_triangle == min(defenders_in_triangle)) %>% slice(1)
  
  should_pass <- best_mate$defenders_in_triangle < defenders_in_shooter
  
  tibble(gender = gender, should_pass = should_pass)
}

# Beregn analyser for mænd #
shots_male <- england_events_male %>%
  filter(!is.na(shot.statsbomb_xg)) %>%
  rowwise() %>%
  mutate(analysis = list(analyze_shot(cur_data(), gender = "Male"))) %>%
  ungroup() %>%
  filter(!is.null(analysis)) %>%
  unnest(analysis)

# Beregn analyser for kvinder #
shots_female <- england_events_female %>%
  filter(!is.na(shot.statsbomb_xg)) %>%
  rowwise() %>%
  mutate(analysis = list(analyze_shot(cur_data(), gender = "Female"))) %>%
  ungroup() %>%
  filter(!is.null(analysis)) %>%
  unnest(analysis)

# Saml data og lav oversigt #
summary_df <- bind_rows(shots_male, shots_female) %>%
  filter(!is.na(should_pass)) %>%
  group_by(gender) %>%
  summarise(
    total_shots = n(),
    passes_better = sum(should_pass),
    percentage_passes_better = round(100 * mean(should_pass), 1)
  )

# Visualisering af plottet #
ggplot(summary_df, aes(x = gender, y = percentage_passes_better, fill = gender)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(percentage_passes_better, 1), "%")), 
            vjust = -0.5, size = 5) +
  labs(
    title = "Male-andel af skud som skulle være afleveret for større chance er højere end Female",
    x = "Køn",
    y = "Andel af skud (%)"
  ) +
  scale_fill_manual(values = c("Male" = "dodgerblue", "Female" = "deeppink")) +
  theme_minimal(base_size = 14)
