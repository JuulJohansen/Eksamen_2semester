library(shiny)
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(jsonlite)
library(tidyverse)
library(stringr)
library(tidyr)

#### indlæs data ####

# Opret forbindelse
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "fodbolddata",
  host = "localhost",
  port = 3306,
  user = "Mig",
  password = "53632785"
)

# Hent og join direkte med SQL

tables <- c(
  "wyscout_matchdetail_base_sl",
  "wyscout_matchdetail_players_sl",
  "wyscout_matchdetail_substitutions_sl",
  "wyscout_matchevents_carry_sl",
  "wyscout_matchevents_common_sl",
  "wyscout_matchevents_groundduel_sl",
  "wyscout_matchevents_infractions_sl",
  "wyscout_matchevents_passes_sl",
  "wyscout_matchevents_possessiontypes_sl",
  "wyscout_matchevents_secondarytype_sl",
  "wyscout_matchevents_shots_sl",
  "wyscout_matchformations_sl",
  "wyscout_playercareer_sl",
  "wyscout_playermatches_sl",
  "wyscout_players_sl",
  "wyscout_teammatches_sl",
  "wyscout_teams_sl"
)

data_list <- lapply(tables, function(table) {
  query <- paste0("SELECT * FROM ", table)
  dbGetQuery(con, query)
})

# Navngiv listeelementerne efter tabellerne
names(data_list) <- tables

# Konverter listen til separate dataframes i Global Environment
list2env(data_list, envir = .GlobalEnv)
    
#### opgave 3 ####

#Indhæntning af Hold


#Indhæntning af Afleværinger
Common <- wyscout_matchevents_common_sl
Afleværing <- wyscout_matchevents_passes_sl
Primary_type <- wyscout_matchevents_possessiontypes_sl
Secondary_type <- wyscout_matchevents_secondarytype_sl

Kamp <- "SELECT *
FROM wyscout_matchdetail_base_sl AS matchbase
LEFT JOIN wyscout_teammatches_sl AS teammatch
  ON matchbase.MATCH_WYID = teammatch.MATCH_WYID
  AND matchbase.TEAM_WYID = teammatch.TEAM_WYID
LEFT JOIN wyscout_teams_sl AS teams
  ON matchbase.TEAM_WYID = teams.TEAM_WYID"

kampplan <- dbGetQuery(con, Kamp)
holdnavn_nummer <- kampplan[,c(6,39)]

hold_udvalgt <- holdnavn_nummer %>%
  distinct(TEAM_WYID, OFFICIALNAME)

hold_udvalgt <- hold_udvalgt %>%
  rename(Hjemmehold = OFFICIALNAME)

Common <- merge(Common, hold_udvalgt, by = "TEAM_WYID")

hold_udvalgt <- hold_udvalgt %>%
  rename(Udehold = Hjemmehold)

hold_udvalgt <- hold_udvalgt %>%
  rename(OPPONENTTEAM_WYID = TEAM_WYID)

Common <- merge(Common, hold_udvalgt, by = "OPPONENTTEAM_WYID")


# Fjern overlappende kolonner (udover dem vi joiner på)
Afleværing_clean <- Afleværing %>%
  select(-PRIMARYTYPE)

Primary_type_clean <- Primary_type %>%
  select(-PRIMARYTYPE)

Secondary_type_clean <- Secondary_type %>%
  select(-PRIMARYTYPE)

# Join alle dataframes på de tre ID'er
afleværing_samlet <- Common %>%
  left_join(Afleværing_clean,     by = c("EVENT_WYID", "MATCH_WYID", "COMPETITION_WYID")) %>%
  left_join(Primary_type_clean,   by = c("EVENT_WYID", "MATCH_WYID", "COMPETITION_WYID")) %>%
  left_join(Secondary_type_clean, by = c("EVENT_WYID", "MATCH_WYID", "COMPETITION_WYID")) 

# Beregn ny variabel: afleveringsretning
afleværing_samlet <- afleværing_samlet[afleværing_samlet$PRIMARYTYPE == "pass", ]
afleværing_samlet <- afleværing_samlet[afleværing_samlet$SEASON_WYID == "188945", ]

afleværing_samlet$delta_x <- afleværing_samlet$ENDLOCATIONX - afleværing_samlet$LOCATIONX
afleværing_samlet$delta_y <- afleværing_samlet$ENDLOCATIONY - afleværing_samlet$LOCATIONY
afleværing_samlet$pass_direction <- atan2(afleværing_samlet$delta_y, afleværing_samlet$delta_x)

cluster_data <- afleværing_samlet[, c("LENGTH", "ANGLE", "ACCURATE", "pass_direction")]
scaled_part <- scale(cluster_data[, c("LENGTH", "ANGLE", "pass_direction")])
cluster_data_scaled <- data.frame(scaled_part, ACCURATE = cluster_data$ACCURATE)

scale_center <- attr(scaled_part, "scaled:center")
scale_scale  <- attr(scaled_part, "scaled:scale")

# Lav en tom vektor til at gemme WSS (Within Sum of Squares)
wss <- numeric()

# Test fx antal clustre fra 1 til 10
for (k in 1:10) {
  kmeans_result <- kmeans(cluster_data_scaled, centers = k, nstart = 25, iter.max = 100)
  wss[k] <- kmeans_result$tot.withinss
}

# Tegn elbow plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Antal Clustre",
     ylab = "sum af kvadrerede afstande",
     main = "Optimal mængde Clustre er 3-5")


set.seed(999)
kmeans_model <- kmeans(cluster_data_scaled, centers = 4)
afleværing_samlet$cluster <- as.factor(kmeans_model$cluster)

# Ekstraher centroids fra kmeans_model
centroids <- as.data.frame(kmeans_model$centers)

centroids$LENGTH <- centroids$LENGTH * scale_scale["LENGTH"] + scale_center["LENGTH"]
centroids$ANGLE <- centroids$ANGLE * scale_scale["ANGLE"] + scale_center["ANGLE"]
centroids$pass_direction <- centroids$pass_direction * scale_scale["pass_direction"] + scale_center["pass_direction"]

# Plot med centroids tilføjet
ggplot(afleværing_samlet, aes(x = LENGTH, y = ANGLE, color = cluster)) +
  geom_point(alpha = 0.4) +
  geom_point(data = centroids, aes(x = LENGTH, y = ANGLE), 
             color = "black", shape = 8, size = 4) +
  labs(
    title = "Clustering af afleveringer i Superligaen",
    x = "Afleveringslængde",
    y = "Afleveringsvinkel"
  ) +
  theme_minimal()
  
afleværing_samlet %>%
  group_by(cluster) %>%
  summarise(
    antal = n(),
    mean_LENGTH = mean(LENGTH, na.rm = TRUE),
    mean_ANGLE = mean(ANGLE, na.rm = TRUE),
    mean_ACCURATE = mean(ACCURATE, na.rm = TRUE),
    mean_pass_direction = mean(pass_direction, na.rm = TRUE),
    sd_LENGTH = sd(LENGTH, na.rm = TRUE),
    sd_ANGLE = sd(ANGLE, na.rm = TRUE)
  )

df <- data.frame(cluster = factor(kmeans_model$cluster))

ggplot(df, aes(x = cluster)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Cluster 2 og 3 har flest afleværinger",
    x = "Cluster",
    y = "Antal afleveringer"
  ) +
  theme_minimal()
  
#### kampe ####

kampe <- afleværing_samlet %>%
  select(MATCH_WYID, Hjemmehold, Udehold) %>%
  distinct(MATCH_WYID, .keep_all = TRUE) %>%
  mutate(kampnavn = paste(Hjemmehold, "vs", Udehold))

#### Opgave 3.2 ####

#### Forså trackingdataen ####
# Indlæs metadata
meta <- fromJSON("R_Projekter/vbob-meta.json")

# Indlæs tracking-data fra CSV
tracking_data <- read.csv("R_Projekter/vbob.csv", header = FALSE, stringsAsFactors = FALSE)

# Giv frameIdx og gameClock rigtige navne
colnames(tracking_data)[1:2] <- c("frameIdx", "gameClock")

# Bane længde
pitch_length <- meta$pitchLength
pitch_width <- meta$pitchWidth
goal_width <- 7.32
penalty_box_length <- 16.5
penalty_box_width <- 40.3
six_yard_box_length <- 5.5
six_yard_box_width <- 18.32
centre_circle_diameter <- 18.3

# --- Hold 1 ---
ids_1 <- tracking_data %>% select(frameIdx, gameClock, V3:V13)
coords_1 <- tracking_data %>% select(frameIdx, gameClock, V14:V35)

# Navngiv kolonner: 11 x’er og 11 y’er
colnames(ids_1)[-c(1,2)] <- paste0("id_", 1:11)
colnames(coords_1)[-c(1,2)] <- c(paste0("x_", 1:11), paste0("y_", 1:11))

# Pivot IDs og koordinater
long_1 <- ids_1 %>%
  pivot_longer(-c(frameIdx, gameClock), names_to = "player", values_to = "id") %>%
  mutate(player_num = parse_number(player))

long_coords_1 <- coords_1 %>%
  pivot_longer(-c(frameIdx, gameClock), names_to = "coord", values_to = "value") %>%
  separate(coord, into = c("type", "player_num"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = value)

long_coords_1 <- long_coords_1 %>%
  mutate(player_num = as.numeric(player_num))

group1 <- left_join(long_1, long_coords_1, by = c("frameIdx", "gameClock", "player_num"))


# --- Hold 2 ---
ids_2 <- tracking_data %>% select(frameIdx, gameClock, V36:V46)
coords_2 <- tracking_data %>% select(frameIdx, gameClock, V47:V68)

colnames(ids_2)[-c(1,2)] <- paste0("id_", 12:22)
colnames(coords_2)[-c(1,2)] <- c(paste0("x_", 12:22), paste0("y_", 12:22))

long_2 <- ids_2 %>%
  pivot_longer(-c(frameIdx, gameClock), names_to = "player", values_to = "id") %>%
  mutate(player_num = parse_number(player))

long_coords_2 <- coords_2 %>%
  pivot_longer(-c(frameIdx, gameClock), names_to = "coord", values_to = "value") %>%
  separate(coord, into = c("type", "player_num"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = value)

long_coords_2 <- long_coords_2 %>%
  mutate(player_num = as.numeric(player_num))

group2 <- left_join(long_2, long_coords_2, by = c("frameIdx", "gameClock", "player_num"))


# --- Saml begge hold og tilføj team-label ---
tracking_long <- bind_rows(group1, group2) %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y),
    team = ifelse(player_num <= 11, "Team A", "Team B")
  )

# Lav opslagstabeller for hjemme- og udehold
lookup_home <- meta$homePlayers %>%
  select(id = ssiId, name) %>%
  mutate(team_side = "home")

lookup_away <- meta$awayPlayers %>%
  select(id = ssiId, name) %>%
  mutate(team_side = "away")

# Kombinér dem
lookup_all <- bind_rows(lookup_home, lookup_away)

# Join med trackingdata
tracking_long_named <- tracking_long %>%
  left_join(lookup_all, by = "id")

tracking_long_named <- tracking_long_named %>%
  mutate(
    club = case_when(
      team_side == "home" ~ "Vejle Boldklub",
      team_side == "away" ~ "Odense Boldklub",
      TRUE ~ NA_character_
    )
  )

# Udtræk boldens koordinater
ball_data <- tracking_data %>%
  select(frameIdx, gameClock, ball_x = V69, ball_y = V70)

# Join til tracking_long_named og juster til pitch-skala
tracking_long_named <- tracking_long_named %>%
  left_join(ball_data, by = c("frameIdx", "gameClock")) %>%
  mutate(
    ball_x_pitch = ball_x + (pitch_length / 2),
    ball_y_pitch = ball_y + (pitch_width / 2)
  )

tracking_long_named <- tracking_long_named %>%
  mutate(
    x_pitch = x + (pitch_length / 2),
    y_pitch = y + (pitch_width / 2)
  )

# Træk boldens placering i frame 0
bold_start <- tracking_long_named %>%
  filter(frameIdx == 0) %>%
  select(ball_x, ball_y) %>%
  distinct()

# Korrekt center
x_center <- 52.425
y_center <- 33.985

# Beregn offset
x_offset <- x_center - bold_start$ball_x
y_offset <- y_center - bold_start$ball_y

tracking_long_named <- tracking_long_named %>%
  mutate(
    x_pitch = x + x_offset,
    y_pitch = y + y_offset,
    ball_x_pitch = ball_x + x_offset,
    ball_y_pitch = ball_y + y_offset
  )

#### Tætteste spiller på bolden ####

#Beregn afstand fra hver spiller til bolden
tracking_with_distance <- tracking_long_named %>%
  mutate(
    distance_to_ball = sqrt((x_pitch - ball_x_pitch)^2 + 
                              (y_pitch - ball_y_pitch)^2)
  )

closest_to_ball <- tracking_with_distance %>%
  group_by(frameIdx) %>%
  filter(distance_to_ball == min(distance_to_ball, na.rm = TRUE)) %>%
  ungroup()

# 2. Tilføj tidligere spiller (fra foregående frame)
closest_to_ball <- closest_to_ball %>%
  arrange(frameIdx) %>%
  mutate(
    previous_holder = lag(id),
    previous_club = lag(club),
    previous_frame = lag(frameIdx)
  )

# 3. Filtrér hvor spiller ændrer sig OG fra samme hold
pass_info <- closest_to_ball %>%
  filter(id != previous_holder & club == previous_club) %>%
  select(
    frameIdx,                    # frame hvor bolden modtages
    position_frame = previous_frame,  # frame hvor bolden afleveres
    passer_id = previous_holder,
    receiver_id = id
  )


#### visuelisering af en afleværing fra målmanden og ud på banen ####

# Målbredde i pitch-system: centralt i y, bredde = 7.32 meter
goal_y_min <- (pitch_width / 2) - (7.32 / 2)
goal_y_max <- (pitch_width / 2) + (7.32 / 2)

tracking_long_named <- tracking_long_named %>%
  mutate(
    x_pitch = x + x_offset,
    y_pitch = y + y_offset,
    ball_x_pitch = ball_x + x_offset,
    ball_y_pitch = ball_y + y_offset
  )


# Find frames hvor bolden er indenfor målet (y) og tæt på mållinjen (x)
goal_frames <- tracking_long_named %>%
  filter(ball_y_pitch >= goal_y_min, ball_y_pitch <= goal_y_max) %>%
  filter(ball_x_pitch <= 1 | ball_x_pitch >= (pitch_length - 1)) %>%
  distinct(frameIdx) %>%
  arrange(frameIdx)

# Find skift mellem målsekvenser (>10 frames imellem = nyt mål)
goal_frames <- goal_frames %>%
  mutate(frame_diff = frameIdx - lag(frameIdx, default = first(frameIdx)),
         new_goal = frame_diff > 10) %>%
  mutate(goal_id = cumsum(new_goal))

# Første frame i hver målsituation
first_goal_frames <- goal_frames %>%
  group_by(goal_id) %>%
  summarise(goal_frame = min(frameIdx)) %>%
  ungroup()

first_goal_frame <- goal_frames %>%
  filter(goal_id == 0) %>%
  summarise(frame = min(frameIdx)) %>%
  pull(frame)

Visuelisering_af_første_mål <- (first_goal_frame - 100):(first_goal_frame)

målforløb_data <- tracking_long_named %>%
  filter(frameIdx %in% Visuelisering_af_første_mål)

goal_sequence <- (first_goal_frame - 1000):(first_goal_frame - 1)

goal_tracking <- tracking_long_named %>%
  filter(frameIdx %in% goal_sequence)

# 1. Filtrér afleveringer i sekvensen op til målet
goal_passes <- pass_info %>%
  filter(position_frame %in% goal_sequence)

# 2. Join afleveringer med spillerkoordinater på afleverings- og modtagelsestidspunkt
passes_with_coords <- goal_passes %>%
  left_join(
    tracking_long_named %>%
      select(frameIdx, id, x_pitch, y_pitch, name),
    by = c("position_frame" = "frameIdx", "passer_id" = "id")
  ) %>%
  rename(passer_x = x_pitch, passer_y = y_pitch, passer_name = name) %>%
  left_join(
    tracking_long_named %>%
      select(frameIdx, id, x_pitch, y_pitch, name),
    by = c("frameIdx" = "frameIdx", "receiver_id" = "id")
  ) %>%
  rename(receiver_x = x_pitch, receiver_y = y_pitch, receiver_name = name)




# Manuel offset, da systemet ikke kan identificere en start på en afleværing ordenligt. For frame 34610 er det 20 tilbage vi skal visuelt.
offset <- 20

# Find afleverings- og spillerdata
selected_pass <- passes_with_coords %>%
  filter(frameIdx == 34610)

frame_data <- tracking_long_named %>%
  filter(frameIdx == selected_pass$position_frame - offset)

selected_pass <- passes_with_coords[1, ]  # eller filter(frameIdx == ...) for specifik aflevering

selected_pass_offset <- tracking_long_named %>%
  filter(frameIdx == selected_pass$position_frame - offset,
         id == selected_pass$passer_id) %>%
  select(passer_x = x_pitch, passer_y = y_pitch) %>%
  bind_cols(selected_pass)

selected_pass_offset <- selected_pass_offset %>%
  rename(
    passer_x = `passer_x...1`,
    passer_y = `passer_y...2`
  )


# Trackingdata for hele holdet i afleveringsøjeblikket
frame_data <- tracking_long_named %>%
  filter(frameIdx == selected_pass$position_frame - offset)

# Tegn banen og aflevering
ggplot() +
  # Baggrund – bane
  geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_width),
            fill = "white", color = "grey80") +
  
  # Midterlinje og cirkel
  geom_segment(aes(x = pitch_length/2, xend = pitch_length/2, y = 0, yend = pitch_width), color = "grey60") +
  annotate("path",
           x = pitch_length/2 + (centre_circle_diameter/2) * cos(seq(0, 2*pi, length.out = 100)),
           y = pitch_width/2 + (centre_circle_diameter/2) * sin(seq(0, 2*pi, length.out = 100)),
           color = "grey60") +
  
  # Målbokse og 5-meter
  geom_rect(aes(xmin = 0, xmax = penalty_box_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - penalty_box_length, xmax = pitch_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = 0, xmax = six_yard_box_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - six_yard_box_length, xmax = pitch_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  
  # Spillere
  geom_point(data = frame_data, aes(x = x_pitch, y = y_pitch, color = club), size = 4) +
  geom_text(data = frame_data, aes(x = x_pitch, y = y_pitch, label = name), vjust = -1, size = 3) +
  
  # Bold
  geom_point(data = frame_data, aes(x = ball_x_pitch, y = ball_y_pitch), color = "black", size = 3) +
  
  # Afleveringspil
  geom_segment(
    data = selected_pass_offset,
    aes(x = passer_x, y = passer_y, xend = receiver_x, yend = receiver_y),
    color = "blue", arrow = arrow(length = unit(0.2, "cm"))
  ) +
  
  # Layout
  coord_fixed(xlim = c(0, pitch_length), ylim = c(0, pitch_width)) +
  theme_minimal() +
  labs(
    title = paste("Freeze Frame – Aflevering fra", selected_pass$passer_name, "til", selected_pass$receiver_name),
    subtitle = paste("Frame:", selected_pass$position_frame, "– visualiseret", offset, "frames før"),
    x = NULL, y = NULL
  ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())


#### Med udgangspunkt i tidliger billede vil jeg nu lave mulige afleværinger (alle spillere på holdet)####

# Find afsenderens klub og holdside i frame_data
passer_info <- frame_data %>%
  filter(id == selected_pass$passer_id)

# Mulige medspillere i samme frame
potential_receivers <- frame_data %>%
  filter(club == passer_info$club, id != selected_pass$passer_id)  # samme klub, ikke passer

# Vis dem
print(potential_receivers %>% select(name, x_pitch, y_pitch))

# Tegn banen og aflevering
ggplot() +
  # Baggrund – bane
  geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_width),
            fill = "white", color = "grey80") +
  
  # Midterlinje og cirkel
  geom_segment(aes(x = pitch_length/2, xend = pitch_length/2, y = 0, yend = pitch_width), color = "grey60") +
  annotate("path",
           x = pitch_length/2 + (centre_circle_diameter/2) * cos(seq(0, 2*pi, length.out = 100)),
           y = pitch_width/2 + (centre_circle_diameter/2) * sin(seq(0, 2*pi, length.out = 100)),
           color = "grey60") +
  
  # Målbokse og 5-meter
  geom_rect(aes(xmin = 0, xmax = penalty_box_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - penalty_box_length, xmax = pitch_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = 0, xmax = six_yard_box_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - six_yard_box_length, xmax = pitch_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  
  # Pile ud til alle spillere fra afsender
  geom_segment(
    data = potential_receivers,
    aes(x = passer_info$x_pitch, y = passer_info$y_pitch,
        xend = x_pitch, yend = y_pitch),
    color = "lightblue", arrow = arrow(length = unit(0.2, "cm")), size = 1
  ) +
  
  # Spillere
  geom_point(data = frame_data, aes(x = x_pitch, y = y_pitch, color = club), size = 4) +
  geom_text(data = frame_data, aes(x = x_pitch, y = y_pitch, label = name), vjust = -1, size = 3) +
  
  # Bold
  geom_point(data = frame_data, aes(x = ball_x_pitch, y = ball_y_pitch), color = "black", size = 3) +
  
  # Afleveringspil
  geom_segment(
    data = selected_pass_offset,
    aes(x = passer_x, y = passer_y, xend = receiver_x, yend = receiver_y),
    color = "blue", arrow = arrow(length = unit(0.2, "cm"))
  ) +
  
  # Layout
  coord_fixed(xlim = c(0, pitch_length), ylim = c(0, pitch_width)) +
  theme_minimal() +
  labs(
    title = paste("Freeze Frame – Aflevering fra", selected_pass$passer_name, "til", selected_pass$receiver_name),
    subtitle = paste("Frame:", selected_pass$position_frame, "– visualiseret", offset, "frames før"),
    x = NULL, y = NULL
  ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

#### Blokering af modstandere, på den reele afleværing, matematisk måde ####

# 1. Find modstandere i frame
opponents <- frame_data %>%
  filter(club != passer_info$club)

# 2. Afsender og modtager position
x1 <- selected_pass_offset$passer_x
y1 <- selected_pass_offset$passer_y
x2 <- selected_pass_offset$receiver_x
y2 <- selected_pass_offset$receiver_y

# 3. Udregn afstand for hver modstander til linjen
opponents <- opponents %>%
  mutate(
    distance_to_line = abs((x2 - x1)*(y1 - y) - (x1 - x)*(y2 - y1)) /
      sqrt((x2 - x1)^2 + (y2 - y1)^2)
  )

# 4. Vis dem sorteret efter afstand
opponents %>% select(name, distance_to_line) %>% arrange(distance_to_line)


# Definér tærskelafstand, hvor meget en modstander kan nå at dække i meter (kan justeres)
covering_threshold <- 1.5

# Tjek om nogen modstandere dækker afleveringen
is_blocked <- any(opponents$distance_to_line < covering_threshold)

# Resultat
if (is_blocked) {
  message("Afleveringen er dækket af en modstander (covering shadow)")
} else {
  message("Afleveringen er fri – ingen modstandere dækker linjen")
}

#### Visuel kejle fra målmand til reel afleværing ####

# Vektor fra afsender til modtager
vec_x <- selected_pass_offset$receiver_x - selected_pass_offset$passer_x
vec_y <- selected_pass_offset$receiver_y - selected_pass_offset$passer_y

# Retning (vinkel) på afleveringen i radianer
pass_angle <- atan2(vec_y, vec_x)

# Keglens bredde i grader (fx 10 grader)
cone_angle <- 10 * pi / 180  # omregnet til radianer

# Afstand til modtager
dist <- sqrt(vec_x^2 + vec_y^2)

# Vinkler til venstre og højre kegleside
angle_left  <- pass_angle - cone_angle / 2
angle_right <- pass_angle + cone_angle / 2

# Koordinater for de to "hjørner" af keglen
cone_x1 <- selected_pass_offset$passer_x + dist * cos(angle_left)
cone_y1 <- selected_pass_offset$passer_y + dist * sin(angle_left)

cone_x2 <- selected_pass_offset$passer_x + dist * cos(angle_right)
cone_y2 <- selected_pass_offset$passer_y + dist * sin(angle_right)

cone_df <- data.frame(
  x = c(selected_pass_offset$passer_x, cone_x1, cone_x2),
  y = c(selected_pass_offset$passer_y, cone_y1, cone_y2)
)

# Plot med kegle
# Beregn covering shadow vinkel
vec_x <- selected_pass_offset$receiver_x - selected_pass_offset$passer_x
vec_y <- selected_pass_offset$receiver_y - selected_pass_offset$passer_y
pass_angle <- atan2(vec_y, vec_x)
cone_angle <- 10 * pi / 180  # fx 10 grader bred vinkel
dist <- sqrt(vec_x^2 + vec_y^2)

# Koordinater til keglesider
cone_x1 <- selected_pass_offset$passer_x + dist * cos(pass_angle - cone_angle / 2)
cone_y1 <- selected_pass_offset$passer_y + dist * sin(pass_angle - cone_angle / 2)
cone_x2 <- selected_pass_offset$passer_x + dist * cos(pass_angle + cone_angle / 2)
cone_y2 <- selected_pass_offset$passer_y + dist * sin(pass_angle + cone_angle / 2)

cone_df <- data.frame(
  x = c(selected_pass_offset$passer_x, cone_x1, cone_x2),
  y = c(selected_pass_offset$passer_y, cone_y1, cone_y2)
)

# Tegn banen og aflevering + kegle
ggplot() +
  # Baggrund – bane
  geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_width),
            fill = "white", color = "grey80") +
  
  # Midterlinje og cirkel
  geom_segment(aes(x = pitch_length/2, xend = pitch_length/2, y = 0, yend = pitch_width), color = "grey60") +
  annotate("path",
           x = pitch_length/2 + (centre_circle_diameter/2) * cos(seq(0, 2*pi, length.out = 100)),
           y = pitch_width/2 + (centre_circle_diameter/2) * sin(seq(0, 2*pi, length.out = 100)),
           color = "grey60") +
  
  # Målbokse og 5-meter
  geom_rect(aes(xmin = 0, xmax = penalty_box_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - penalty_box_length, xmax = pitch_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = 0, xmax = six_yard_box_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - six_yard_box_length, xmax = pitch_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  
  # Covering shadow cone (kegle)
  geom_polygon(data = cone_df, aes(x = x, y = y), fill = "grey50", alpha = 0.3) +
  
  # Pile ud til alle spillere fra afsender
  geom_segment(
    data = potential_receivers,
    aes(x = passer_info$x_pitch, y = passer_info$y_pitch,
        xend = x_pitch, yend = y_pitch),
    color = "lightblue", arrow = arrow(length = unit(0.2, "cm")), size = 1
  ) +
  
  # Spillere
  geom_point(data = frame_data, aes(x = x_pitch, y = y_pitch, color = club), size = 4) +
  geom_text(data = frame_data, aes(x = x_pitch, y = y_pitch, label = name), vjust = -1, size = 3) +
  
  # Bold
  geom_point(data = frame_data, aes(x = ball_x_pitch, y = ball_y_pitch), color = "black", size = 3) +
  
  # Afleveringspil
  geom_segment(
    data = selected_pass_offset,
    aes(x = passer_x, y = passer_y, xend = receiver_x, yend = receiver_y),
    color = "blue", arrow = arrow(length = unit(0.2, "cm"))
  ) +
  
  # Layout
  coord_fixed(xlim = c(0, pitch_length), ylim = c(0, pitch_width)) +
  theme_minimal() +
  labs(
    title = paste("Freeze Frame – Aflevering fra", selected_pass$passer_name, "til", selected_pass$receiver_name),
    subtitle = paste("Frame:", selected_pass$position_frame, "– visualiseret", offset, "frames før"),
    x = NULL, y = NULL
  ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

#### Kejle til alle medspillere ####

# Funktion til at generere cone-koordinater
generate_cone <- function(x1, y1, x2, y2, cone_deg = 10) {
  angle <- atan2(y2 - y1, x2 - x1)
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  cone_rad <- cone_deg * pi / 180
  
  angle_left  <- angle - cone_rad / 2
  angle_right <- angle + cone_rad / 2
  
  x_left  <- x1 + dist * cos(angle_left)
  y_left  <- y1 + dist * sin(angle_left)
  
  x_right <- x1 + dist * cos(angle_right)
  y_right <- y1 + dist * sin(angle_right)
  
  data.frame(
    x = c(x1, x_left, x_right),
    y = c(y1, y_left, y_right),
    group = paste0(round(x2, 2), "_", round(y2, 2))  # unik gruppe pr modtager
  )
}

# Generér cones for alle potentielle modtagere
cones_df <- bind_rows(lapply(1:nrow(potential_receivers), function(i) {
  generate_cone(
    x1 = passer_info$x_pitch,
    y1 = passer_info$y_pitch,
    x2 = potential_receivers$x_pitch[i],
    y2 = potential_receivers$y_pitch[i]
  )
}))

# Generér cones for alle potentielle modtagere
cones_df <- bind_rows(lapply(1:nrow(potential_receivers), function(i) {
  generate_cone(
    x1 = passer_info$x_pitch,
    y1 = passer_info$y_pitch,
    x2 = potential_receivers$x_pitch[i],
    y2 = potential_receivers$y_pitch[i]
  )
}))

#### Plot med alle cones ####

ggplot() +
  # Baggrund – bane
  geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_width),
            fill = "white", color = "grey80") +
  
  # Midterlinje og cirkel
  geom_segment(aes(x = pitch_length/2, xend = pitch_length/2, y = 0, yend = pitch_width), color = "grey60") +
  annotate("path",
           x = pitch_length/2 + (centre_circle_diameter/2) * cos(seq(0, 2*pi, length.out = 100)),
           y = pitch_width/2 + (centre_circle_diameter/2) * sin(seq(0, 2*pi, length.out = 100)),
           color = "grey60") +
  
  # Målbokse og 5-meter
  geom_rect(aes(xmin = 0, xmax = penalty_box_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - penalty_box_length, xmax = pitch_length,
                ymin = (pitch_width - penalty_box_width)/2,
                ymax = (pitch_width + penalty_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = 0, xmax = six_yard_box_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  geom_rect(aes(xmin = pitch_length - six_yard_box_length, xmax = pitch_length,
                ymin = (pitch_width - six_yard_box_width)/2,
                ymax = (pitch_width + six_yard_box_width)/2),
            color = "grey60", fill = NA) +
  
  # Covering cones – én farve per modtager
  geom_polygon(data = cones_df, aes(x = x, y = y, group = group, fill = group),
               alpha = 0.3, color = NA) +
  scale_fill_viridis_d(option = "D", guide = "none") +  # flot og automatisk farveskema
  
  
  # Pile til potentielle modtagere
  geom_segment(
    data = potential_receivers,
    aes(x = passer_info$x_pitch, y = passer_info$y_pitch,
        xend = x_pitch, yend = y_pitch),
    color = "lightblue", arrow = arrow(length = unit(0.2, "cm")), size = 1
  ) +
  
  # Reelle aflevering
  geom_segment(
    data = selected_pass_offset,
    aes(x = passer_x, y = passer_y, xend = receiver_x, yend = receiver_y),
    color = "blue", arrow = arrow(length = unit(0.2, "cm")), size = 1.2
  ) +
  
  # Spillere og bold
  geom_point(data = frame_data, aes(x = x_pitch, y = y_pitch, color = club), size = 4) +
  geom_text(data = frame_data, aes(x = x_pitch, y = y_pitch, label = name), vjust = -1, size = 3) +
  geom_point(data = frame_data, aes(x = ball_x_pitch, y = ball_y_pitch), color = "black", size = 3) +
  
  # Layout
  coord_fixed(xlim = c(0, pitch_length), ylim = c(0, pitch_width)) +
  theme_minimal() +
  labs(
    title = paste("Freeze Frame – Afleveringsmuligheder og covering cones"),
    subtitle = paste("Frame:", selected_pass$position_frame, "– visualiseret", offset, "frames før"),
    x = NULL, y = NULL
  ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())



#### shiny opgave 3 og 4 ####

ui <- fluidPage(
  titlePanel("Analyse af afleveringer i Superligaen"),
  
  tabsetPanel(

    tabPanel("Opgave 4",
             tabsetPanel(
               tabPanel("4.2 Kampvisning",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("kampnavn", "Vælg kamp:",
                                        choices = kampe$kampnavn),
                            selectInput("halvleg", "Vælg halvleg:",
                                        choices = c("1. Halvleg" = "1H", "2. Halvleg" = "2H")),
                            sliderInput("minute", "Vælg minut (5 minutters intervaller):",
                                        min = 0, max = 120, value = 0, step = 5),
                            selectInput("holdvalg", "Vælg holdvisning:", choices = NULL)
                          ),
                          mainPanel(
                            plotOutput("passPlot", height = "700px"),
                            br(),
                            htmlOutput("cluster_forklaring"),
                            br(),
                            htmlOutput("cluster_count_interval"),
                            br(),
                            htmlOutput("cluster_count_kamp")
                          )
                        )
               ),
               tabPanel("4.1 Clustering",
                        tabsetPanel(
                          tabPanel("Fordeling pr. cluster",
                                   plotOutput("barPlot_cluster", height = "600px"),
                                   br(),
                                   htmlOutput("fordelings_beskrivelse")
                                   
                          ),
                          tabPanel("Visualisering af clustering",
                                   plotOutput("cluster_scatter", height = "600px"),
                                   br(),
                                   htmlOutput("cluster_beskrivelse")
                          )
                        )
               )
             )
    ),
    
    tabPanel("Opgave 3.2",
             tabsetPanel(
               tabPanel("Målforløb",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("frame", "Vælg frame:",
                                        min = min(Visuelisering_af_første_mål),
                                        max = max(Visuelisering_af_første_mål),
                                        value = min(Visuelisering_af_første_mål),
                                        step = 1,
                                        animate = animationOptions(interval = 100, loop = FALSE))
                          ),
                          mainPanel(
                            plotOutput("banekort", height = "600px")
                          )
                        )
               ),
               tabPanel("Covershadows",
                        mainPanel(
                          plotOutput("covering_plot", height = "700px")
                        )
               )
             )
    )
  )
)

server <- function(input, output, session) {

  selected_match_id <- reactive({
    kampe %>% filter(kampnavn == input$kampnavn) %>% pull(MATCH_WYID)
  })
  
  Hjemmehold_WYID <- reactive({
    afleværing_samlet %>%
      filter(MATCH_WYID == selected_match_id()) %>%
      pull(TEAM_WYID) %>% .[1]
  })
  
  Udehold_WYID <- reactive({
    afleværing_samlet %>%
      filter(MATCH_WYID == selected_match_id()) %>%
      pull(OPPONENTTEAM_WYID) %>% .[1]
  })
  
  observe({
    kampinfo <- kampe %>% filter(kampnavn == input$kampnavn)
    hjemmelabel <- kampinfo$Hjemmehold[1]
    udelabel <- kampinfo$Udehold[1]
    
    updateSelectInput(
      inputId = "holdvalg",
      choices = c("Begge hold", hjemmelabel, udelabel),
      selected = "Begge hold"
    )
  })
  
  filtered_data <- reactive({
    data <- afleværing_samlet %>%
      filter(MATCH_WYID == selected_match_id(),
             MATCHPERIOD == input$halvleg,
             MINUTE >= input$minute & MINUTE < input$minute + 5)
    
    kampinfo <- kampe %>% filter(kampnavn == input$kampnavn)
    
    hjemmelabel <- kampinfo$Hjemmehold[1]
    udelabel <- kampinfo$Udehold[1]
    
    if (input$holdvalg == hjemmelabel) {
      data <- data %>% filter(TEAM_WYID == Hjemmehold_WYID())
    } else if (input$holdvalg == udelabel) {
      data <- data %>% filter(TEAM_WYID == Udehold_WYID())
    }
    
    return(data)
  })
  
  output$cluster_count_interval <- renderUI({
    data <- filtered_data()
    
    if (nrow(data) == 0) return(HTML("<p><em>Ingen afleveringer i dette interval.</em></p>"))
    
    cluster_tælling <- data %>%
      count(cluster) %>%
      arrange(cluster)
    
    tekst <- paste0(
      "<h5>Afleveringer i intervallet ", input$minute, "-", input$minute + 5, " min:</h5>",
      "<ul>",
      paste0("<li>Cluster ", cluster_tælling$cluster, ": ", cluster_tælling$n, "</li>", collapse = ""),
      "</ul>"
    )
    
    HTML(tekst)
  })
  
  # Antal pr. cluster i hele kampen
  output$cluster_count_kamp <- renderUI({
    kamp_data <- afleværing_samlet %>%
      filter(MATCH_WYID == selected_match_id())
    
    cluster_tælling <- kamp_data %>%
      count(cluster) %>%
      arrange(cluster)
    
    tekst <- paste0(
      "<h5>Afleveringer i hele kampen:</h5>",
      "<ul>",
      paste0("<li>Cluster ", cluster_tælling$cluster, ": ", cluster_tælling$n, "</li>", collapse = ""),
      "</ul>"
    )
    
    HTML(tekst)
  })
  
  output$passPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data) +
      annotate_pitch(dimensions = pitch_opta, colour = "grey60", fill = "white") +
      theme_pitch() +
      geom_point(aes(x = LOCATIONX, y = LOCATIONY, color = cluster),
                 alpha = 0.6, size = 2) +
      scale_color_brewer(palette = "Set1") +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      labs(
        title = paste0("Afleveringsstartpunkter (", input$minute, "-", input$minute + 5, " min) – ", input$halvleg),
        subtitle = paste("Kamp:", input$kampnavn),
        color = "Afleveringstype (cluster)"
      )
  })
  
  output$barPlot_cluster <- renderPlot({
    cluster_tælling <- afleværing_samlet %>%
      count(cluster) %>%
      mutate(cluster = factor(cluster))
    
    ggplot(cluster_tælling, aes(x = cluster, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      labs(
        title = "Fordeling af afleveringer pr. cluster",
        x = "Cluster",
        y = "Antal afleveringer"
      ) +
      theme_minimal()
  })
  
  output$cluster_beskrivelse <- renderUI({
    tekst <- "<h4>Beskrivelse af clustering</h4><ul>"
    
    for (i in 1:nrow(centroids)) {
      cl <- i
      len <- round(centroids$LENGTH[i], 1)
      ang <- round(centroids$ANGLE[i], 1)
      dir <- round(centroids$pass_direction[i], 2)
      acc <- round(centroids$ACCURATE[i], 2)
      
      beskrivelse <- switch(as.character(cl),
                            "1" = "Lange afleveringer centralt med moderat præcision",
                            "2" = "Korte præcise afleveringer mod højre fløj",
                            "3" = "Korte præcise afleveringer mod venstre fløj",
                            "4" = "Korte centrale afleveringer med god præcision",
                            "Beskrivelse mangler")
      
      tekst <- paste0(tekst,
                      "<li><strong>Cluster ", cl, ":</strong> ", beskrivelse,
                      "<br/><em>(Længde: ", len,
                      ", Vinkel: ", ang,
                      ", Retning: ", dir,
                      ", Præcision: ", acc, ")</em></li>")
    }
    
    tekst <- paste0(tekst, "</ul>")
    HTML(tekst)
  })
  
  
  output$fordelings_beskrivelse <- renderUI({
    HTML("
    <h4>Konklusion på fordeling</h4>
    <ul>
      <li><strong>Cluster 2 og 3:</strong> De mest udbredte afleveringer.</li>
      <li><strong>Cluster 4:</strong> Understøtter en boldbesiddende spillestil.</li>
      <li><strong>Cluster 1:</strong> Lange afleveringer bruges sjældent – typisk i omstillinger.</li>
    </ul>
    ")
  })
  
  output$cluster_scatter <- renderPlot({
    ggplot(afleværing_samlet, aes(x = LENGTH, y = ANGLE, color = factor(cluster))) +
      geom_point(alpha = 0.4) +
      geom_point(data = centroids, aes(x = LENGTH, y = ANGLE),
                 color = "black", shape = 8, size = 4, inherit.aes = FALSE) +
      labs(
        title = "Clustering af afleveringer i Superligaen",
        x = "Afleveringslængde",
        y = "Afleveringsvinkel",
        color = "Cluster"
      ) +
      theme_minimal()
  })
  
  output$cluster_forklaring <- renderUI({
    HTML("
    <h4>Forklaring på clusterfarver</h4>
    <ul>
      <li><strong>Cluster 1:</strong> Lange afleveringer centralt med moderat præcision</li>
      <li><strong>Cluster 2:</strong> Korte præcise afleveringer mod højre fløj</li>
      <li><strong>Cluster 3:</strong> Korte præcise afleveringer mod venstre fløj</li>
      <li><strong>Cluster 4:</strong> Korte centrale afleveringer med god præcision</li>
    </ul>
  ")
  })
  
  # --- Opgave 3.2 ---
  
  output$banekort <- renderPlot({
    frame_data <- målforløb_data %>%
      filter(frameIdx == input$frame)
    
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_width),
                fill = "white", color = "grey80") +
      geom_segment(aes(x = pitch_length / 2, xend = pitch_length / 2, y = 0, yend = pitch_width), color = "grey60") +
      annotate("path",
               x = pitch_length / 2 + (centre_circle_diameter / 2) * cos(seq(0, 2 * pi, length.out = 100)),
               y = pitch_width / 2 + (centre_circle_diameter / 2) * sin(seq(0, 2 * pi, length.out = 100)),
               color = "grey60") +
      geom_rect(aes(xmin = 0, xmax = penalty_box_length,
                    ymin = (pitch_width - penalty_box_width)/2,
                    ymax = (pitch_width + penalty_box_width)/2),
                color = "grey60", fill = NA) +
      geom_rect(aes(xmin = pitch_length - penalty_box_length, xmax = pitch_length,
                    ymin = (pitch_width - penalty_box_width)/2,
                    ymax = (pitch_width + penalty_box_width)/2),
                color = "grey60", fill = NA) +
      geom_rect(aes(xmin = 0, xmax = six_yard_box_length,
                    ymin = (pitch_width - six_yard_box_width)/2,
                    ymax = (pitch_width + six_yard_box_width)/2),
                color = "grey60", fill = NA) +
      geom_rect(aes(xmin = pitch_length - six_yard_box_length, xmax = pitch_length,
                    ymin = (pitch_width - six_yard_box_width)/2,
                    ymax = (pitch_width + six_yard_box_width)/2),
                color = "grey60", fill = NA) +
      geom_point(data = frame_data, aes(x = x_pitch, y = y_pitch, color = club), size = 4) +
      geom_text(data = frame_data, aes(x = x_pitch, y = y_pitch, label = name), vjust = -1, size = 3) +
      geom_point(data = frame_data %>% filter(!is.na(ball_x_pitch)),
                 aes(x = ball_x_pitch, y = ball_y_pitch), color = "black", size = 3) +
      coord_fixed(xlim = c(0, pitch_length), ylim = c(0, pitch_width)) +
      theme_minimal() +
      labs(title = paste("Frame", input$frame), x = NULL, y = NULL) +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  
  output$covering_plot <- renderPlot({
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = pitch_width),
                fill = "white", color = "grey80") +
      geom_segment(aes(x = pitch_length / 2, xend = pitch_length / 2, y = 0, yend = pitch_width), color = "grey60") +
      annotate("path",
               x = pitch_length / 2 + (centre_circle_diameter / 2) * cos(seq(0, 2 * pi, length.out = 100)),
               y = pitch_width / 2 + (centre_circle_diameter / 2) * sin(seq(0, 2 * pi, length.out = 100)),
               color = "grey60") +
      geom_rect(aes(xmin = 0, xmax = penalty_box_length,
                    ymin = (pitch_width - penalty_box_width)/2,
                    ymax = (pitch_width + penalty_box_width)/2),
                color = "grey60", fill = NA) +
      geom_rect(aes(xmin = pitch_length - penalty_box_length, xmax = pitch_length,
                    ymin = (pitch_width - penalty_box_width)/2,
                    ymax = (pitch_width + penalty_box_width)/2),
                color = "grey60", fill = NA) +
      geom_rect(aes(xmin = 0, xmax = six_yard_box_length,
                    ymin = (pitch_width - six_yard_box_width)/2,
                    ymax = (pitch_width + six_yard_box_width)/2),
                color = "grey60", fill = NA) +
      geom_rect(aes(xmin = pitch_length - six_yard_box_length, xmax = pitch_length,
                    ymin = (pitch_width - six_yard_box_width)/2,
                    ymax = (pitch_width + six_yard_box_width)/2),
                color = "grey60", fill = NA) +
      geom_polygon(data = cones_df, aes(x = x, y = y, group = group, fill = group),
                   alpha = 0.3, color = NA) +
      scale_fill_viridis_d(option = "D", guide = "none") +
      geom_segment(data = potential_receivers,
                   aes(x = passer_info$x_pitch, y = passer_info$y_pitch,
                       xend = x_pitch, yend = y_pitch),
                   color = "lightblue", arrow = arrow(length = unit(0.2, "cm")), size = 1) +
      geom_segment(data = selected_pass_offset,
                   aes(x = passer_x, y = passer_y, xend = receiver_x, yend = receiver_y),
                   color = "blue", arrow = arrow(length = unit(0.2, "cm")), size = 1.2) +
      geom_point(data = frame_data, aes(x = x_pitch, y = y_pitch, color = club), size = 4) +
      geom_text(data = frame_data, aes(x = x_pitch, y = y_pitch, label = name), vjust = -1, size = 3) +
      geom_point(data = frame_data, aes(x = ball_x_pitch, y = ball_y_pitch), color = "black", size = 3) +
      coord_fixed(xlim = c(0, pitch_length), ylim = c(0, pitch_width)) +
      theme_minimal() +
      labs(
        title = paste("Freeze Frame – Afleveringsmuligheder og covering cones"),
        subtitle = paste("Frame:", selected_pass$position_frame, "– visualiseret", offset, "frames før"),
        x = NULL, y = NULL
      ) +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
}

shinyApp(ui = ui, server = server)
