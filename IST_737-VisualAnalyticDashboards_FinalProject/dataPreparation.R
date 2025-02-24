library(tidyverse)

world_cup_data <- data.frame(
  year = c(1930, 1934, 1938, 1950, 1954, 1958, 1962, 1966, 1970, 1974,
           1978, 1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022),
  hosts = c("Uruguay", "Italy", "France", "Brazil", "Switzerland", "Sweden", 
            "Chile", "England", "Mexico", "Germany", "Argentina", "Spain", 
            "Mexico", "Italy", "United States", "France", "Korea and Japan", 
            "Germany", "South Africa", "Brazil", "Russia", "Qatar"),
  champion = c("Uruguay", "Italy", "Italy", "Uruguay", "Germany", "Brazil", 
               "Brazil", "England", "Brazil", "Germany", "Argentina", "Italy", 
               "Argentina", "Brazil", "France", "Brazil", "France", 
               "Italy", "Spain", "Germany", "France", "Argentina"),
  num_Goals = c(70, 70, 84, 88, 140, 126, 89, 89, 95, 97, 
                102, 146, 132, 115, 141, 171, 161, 147, 145, 171, 
                169, 172),
  total_Attendance = c(590549, 363000, 375700, 1045246, 768607, 819810, 
                       893172, 1563135, 1603975, 1865753, 1545791, 
                       2109723, 2394031, 2516215, 3587538, 
                       2785100, 2705197, 3359439, 3178856, 3429873, 
                       3031768, 3404252),
  num_Matches = c(18, 17, 18, 22, 26, 35, 32, 32, 32, 38, 38, 52, 
                  52, 52, 52, 64, 64, 64, 64, 64, 64, 64),
  average_Attendance = c(32808, 21353, 20872, 47511, 29562, 23423, 
                         27912, 48848, 50124, 49099, 40679, 40572, 
                         46039, 48389, 68991, 43517, 42269, 52491, 
                         49670, 53592, 47371, 53191),
  highest_Attendance = c(93000, 55000, 58455, 173850, 63000, 50928, 
                         68679, 98270, 108192, 83168, 71712, 95500, 
                         114600, 74765, 94194, 80000, 69029, 72000, 
                         84490, 74738, 78011, 88966),
  num_Venues = c(3, 8, 10, 6, 6, 12, 4, 8, 5, 9, 6, 17, 12, 12, 9, 
                 10, 20, 12, 10, 12, 12, 8),
  num_Cities = c(1, 8, 9, 6, 6, 12, 4, 7, 5, 9, 5, 14, 11, 12, 9, 
                 10, 20, 12, 9, 12, 11, 5),
  num_Teams = c(13,16,16,13,16,16,16,16,16,16,16,24,24,24,24,32,32,32,32,32,32,32),
  prize_money = c(0,0,0,0,0,0,0,0,0,0,0,20,26,54,71,103,156.6,266,420,564,791,1000)
) %>%
  mutate(
    host_1 = ifelse(year == 2002, "Korea", hosts),
    host_2 = ifelse(year == 2002, "Japan", ""),
    gender = "Men"
  ) %>%
  select(-hosts)

world_cup_data <- world_cup_data %>%
  arrange(year)

world_cup_data <- world_cup_data %>%
  mutate(World_Cup_Number = row_number())

women_world_cup_data <- data.frame(
  year = c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  hosts = c("China", "Sweden", "United States", "United States", "China", 
            "Germany", "Canada", "France", "Australia and New Zealand"),
  num_Venues = c(6, 5, 8, 6, 5, 9, 6, 9, 10),
  num_Cities = c(4, 5, 8, 6, 5, 9, 6, 9, 9),
  num_Goals = c(99, 99, 123, 107, 111, 86, 146, 146, 164),
  total_Attendance = c(510000, 112213, 1214209, 679664, 1190971, 
                       845751, 1353506, 1131312, 1978274),
  num_Matches = c(26, 26, 32, 32, 32, 32, 52, 52, 64),
  average_Attendance = c(18344, 4316, 37944, 21240, 37218, 
                         26430, 26029, 21756, 30911),
  highest_Attendance = c(65000, 17158, 90185, 34144, 55832, 
                         73680, 54027, 57900, 75784),
  champion = c("United States", "Norway", "United States", "Germany", 
               "Germany", "Japan", "United States", "United States", 
               "Spain"),
  num_Teams = c(12,12,16,16,16,16,24,24,32),
  prize_money = c(0,0,0,0,5.8,7.5,15,30,110)
) %>%
  mutate(
    host_1 = ifelse(year == 2023, "Australia", hosts),
    host_2 = ifelse(year == 2023, "New Zealand", ""),
    gender = "Women"
  ) %>%
  select(-hosts)

women_world_cup_data <- women_world_cup_data %>%
  arrange(year)

women_world_cup_data <- women_world_cup_data %>%
  mutate(World_Cup_Number = row_number())



combined_world_cup_data <- bind_rows(world_cup_data, women_world_cup_data) %>%
  arrange(year)



write_csv(combined_world_cup_data,"WorldCupData.csv")




library(StatsBombR)

allComps <- FreeCompetitions()
intComp <- allComps[allComps$competition_international==TRUE,]
intCompF <- intComp[intComp$competition_gender=="female",]
intCompFInt <- intCompF[intCompF$country_name=="International",]
matchesFInt <- FreeMatches(intCompFInt)

home_managers_unnested <- matchesFInt %>%
  select(match_id, home_team.managers) %>%
  unnest_wider(home_team.managers, names_sep = "_") %>%
  select(-home_team.managers_nickname)

away_managers_unnested <- matchesFInt %>%
  select(match_id, away_team.managers) %>%
  unnest_wider(away_team.managers, names_sep = "_") %>%
  select(-away_team.managers_nickname)

matchesFInt <- matchesFInt %>%
  select(-home_team.managers, -away_team.managers) %>%
  left_join(home_managers_unnested, by = "match_id") %>%
  left_join(away_managers_unnested, by = "match_id")

matchesFInt$match_date <- as.Date(matchesFInt$match_date)
matchesFInt$home_team.managers_dob <- as.Date(matchesFInt$home_team.managers_dob)
matchesFInt$away_team.managers_dob <- as.Date(matchesFInt$away_team.managers_dob)

na_counts <- colSums(is.na(matchesFInt))
na_counts <- na_counts[na_counts > 0]
print(na_counts)

matchesFInt %>%
  filter(is.na(home_team.managers_dob)) %>%
  select(home_team.managers_name)

matchesFInt %>%
  filter(is.na(away_team.managers_dob)) %>%
  select(away_team.managers_name)


matchesFInt <- matchesFInt %>%
  mutate(
    home_team.managers_dob = if_else(home_team.managers_name == "Bruce Mwape", as.Date("1959-10-26"), home_team.managers_dob),
    away_team.managers_dob = if_else(away_team.managers_name == "Bruce Mwape", as.Date("1959-10-26"), away_team.managers_dob)
  )

matchesFInt <- matchesFInt %>%
  mutate(
    home_team.managers_dob = if_else(home_team.managers_name == "Angelo Marsiglia", as.Date("1985-12-20"), home_team.managers_dob),
    away_team.managers_dob = if_else(away_team.managers_name == "Angelo Marsiglia", as.Date("1985-12-20"), away_team.managers_dob)
  )


missRef <-matchesFInt %>%
  filter(is.na(referee.id)) %>%
  select(match_id,match_date, home_team.home_team_name,home_score, away_team.away_team_name,away_score, season.season_name, competition_stage.name)

lookup_table_name <- matchesFInt %>%
  distinct(referee.id, referee.name)

referee_data <- data.frame(
  match_id = c(3893797, 22930, 22942, 22943, 22948, 22961, 22962, 22963, 22964, 22966, 22973, 22983, 68343, 68355, 68362, 69191, 69258),
  referee_id = c(374, 2476, 2426, 2601,NA ,NA ,NA , 2637, 2388, 2546, 2508, 2426, 2637, 2546,NA , 2426, 2426),
  referee_name = c(NA, NA, NA, NA, "Bibiana Steinhaus", "Lidya Tafesse Abebe", "Liang Qin", NA, NA, NA, NA, NA, NA, NA, "Liang Qin", NA, NA),
  referee_country_name = c(NA, NA, NA, NA, "Brazil", "Ethiopia", "China", NA, NA, NA, NA, NA, NA, NA, "China", NA, NA)
)


next_referee_id <- max(as.numeric(as.character(matchesFInt$referee.id)), na.rm = TRUE) + 1
ref_names <- c()
ref_id <- c()

for (i in 1:nrow(referee_data)) {
  names <- c(names, referee_data$referee_name[i])
  if (is.na(referee_data$referee_id[i]) && !is.na(referee_data$referee_name[i])) {
    if (referee_data$referee_name[i] %in% names[-i]) {
      name_index <- which(names == referee_data$referee_name[i])[1]
      referee_data$referee_id[i] <- id[name_index]
    }
    else {
      referee_data$referee_id[i] <- next_referee_id
      next_referee_id <- next_referee_id + 1
    }
  }
  
  id <- c(id, referee_data$referee_id[i])
}

referee_data$referee_id <- as.numeric(as.character(referee_data$referee_id))

matchesFInt <- merge(matchesFInt, referee_data, by.x = "match_id", by.y = "match_id", all.x = TRUE)


matchesFInt <- matchesFInt %>%
  mutate(referee.id = ifelse(is.na(referee.id), referee_id, referee.id))

matchesFInt <- matchesFInt %>%
  mutate(referee.name = ifelse(is.na(referee.name), referee_name, referee.name))

matchesFInt <- matchesFInt %>%
  mutate(referee.country.name = ifelse(is.na(referee.country.name), referee_country_name, referee.country.name))



matchesFInt <- matchesFInt %>%
  left_join(lookup_table_name, by = "referee.id") %>%
  select(-referee.name.x) %>%
  rename(referee.name = referee.name.y) %>%
  mutate(referee.name = coalesce(referee.name, referee_name)) %>%
  select(-referee_name,-referee_id, -referee_country_name)

lookup_table_id_ref <- matchesFInt %>%
  distinct(referee.id, referee.country.id,referee.country.name) %>%
  filter(!is.na(referee.country.id))


matchesFInt <- matchesFInt %>%
  left_join(lookup_table_id_ref, by = "referee.id") %>%
  select(-referee.country.id.x) %>%
  rename(referee.country.name = referee.country.name.y) %>%
  rename(referee.country.id = referee.country.id.y)

matchesFInt <- matchesFInt %>%
  mutate(referee.country.name = ifelse(is.na(referee.country.name), referee.country.name.x, referee.country.name)) %>%
  select(-referee.country.name.x)

lookup_table_id <- matchesFInt %>%
  distinct(referee.country.id,referee.country.name)

country_name <- c()
country_id <- c()

next_country_id <- max(as.numeric(as.character(matchesFInt$referee.country.id)), na.rm = TRUE) + 1


for(i in 1:nrow(lookup_table_id)){
  country_name <- c(country_name,lookup_table_id$referee.country.name[i])
  if(is.na(lookup_table_id$referee.country.id[i])){
    if(lookup_table_id$referee.country.name[i] %in% country_name[-i]){
      country_name_index <- which(names == lookup_table_id$referee.country.name[i])[1]
      lookup_table_id$referee.country.name[i] <- id[country_name_index]
    }
    else{
      lookup_table_id$referee.country.id[i] <- next_country_id
      next_country_id <- next_country_id + 1
    }
  }
}


lookup_table_id <- na.omit(lookup_table_id)

lookup_table_id$referee.country.name <- unlist(lookup_table_id$referee.country.name)

matchesFInt <- matchesFInt %>%
  left_join(lookup_table_id, by = "referee.country.name") %>%
  select(-referee.country.id.x)%>%
  rename(referee.country.id = referee.country.id.y)

for(i in 1:nrow(matchesFInt)){
  if(is.na(matchesFInt$referee.id[i])){
    matchesFInt$referee.id[i] <- next_referee_id
    next_referee_id <- next_referee_id +1
  }
}


matchesFInt <- matchesFInt %>%
  select(
    match_id,
    match_date,
    kick_off,
    home_score,
    away_score,
    match_week,
    season.season_name,
    home_team.home_team_id,
    home_team.home_team_name,
    starts_with("home_team.managers"),
    home_team.country.id,
    home_team.country.name,
    away_team.away_team_id,
    away_team.away_team_name,
    starts_with("away_team.managers"),
    away_team.country.id,
    away_team.country.name,
    competition_stage.id,
    competition_stage.name,
    stadium.id,
    stadium.name,
    stadium.country.id,
    stadium.country.name,
    referee.id,
    referee.name,
    referee.country.id,
    referee.country.name,
    away_team.away_team_gender,
    home_team.home_team_gender
  )

matchesFInt2019 <- matchesFInt[matchesFInt$season.season_name==2019,]
matchesFInt2023 <- matchesFInt[matchesFInt$season.season_name==2023,]


qf <- data.frame(
  match_id = c(69199, 69202, 69205, 69208),
  home_team.country.name = c("Norway", "France", "Italy", "Germany"),
  away_team.country.name = c("England", "United States of America", "Netherlands", "Sweden")
)


home_team_info_2023 <- matchesFInt2023 %>%
  select(team_id = home_team.home_team_id, team_name = home_team.home_team_name, 
         country_id = home_team.country.id, country_name = home_team.country.name)

home_team_info_2019 <- matchesFInt2019 %>%
  select(team_id = home_team.home_team_id, team_name = home_team.home_team_name, 
         country_id = home_team.country.id, country_name = home_team.country.name)

away_team_info_2023 <- matchesFInt2023 %>%
  select(team_id = away_team.away_team_id, team_name = away_team.away_team_name, 
         country_id = away_team.country.id, country_name = away_team.country.name)

away_team_info_2019 <- matchesFInt2019 %>%
  select(team_id = away_team.away_team_id, team_name = away_team.away_team_name, 
         country_id = away_team.country.id, country_name = away_team.country.name)

team_info_2023 <- bind_rows(home_team_info_2023, away_team_info_2023)

team_info_2019 <- bind_rows(home_team_info_2019, away_team_info_2019)

team_lookup_2023 <- team_info_2023 %>%
  distinct(team_id, team_name, country_id, country_name)

team_lookup_2019 <- team_info_2019 %>%
  distinct(team_id, team_name, country_id, country_name)

group_mapping_2023 <- data.frame(
  country_id = c(221, 171, 163, 180, 14, 166, 40, 109, 
                 114, 214, 253, 54, 68, 61, 46, 98, 
                 160, 241, 183, 247, 78, 113, 31, 176, 
                 220, 211, 112, 11, 49, 154, 85, 121),
  group = c("Group A", "Group A", "Group A", "Group A",
            "Group B", "Group B", "Group B", "Group B",
            "Group C", "Group C", "Group C", "Group C",
            "Group D", "Group D", "Group D", "Group D",
            "Group E", "Group E", "Group E", "Group E",
            "Group F", "Group F", "Group F", "Group F",
            "Group G", "Group G", "Group G", "Group G",
            "Group H", "Group H", "Group H", "Group H")
)

group_mapping_2019 <- data.frame(
  country_id = c(78, 171, 166, 121, 85, 214, 46, 211, 112, 14, 31, 113, 68, 114, 11, 201, 160, 40, 39, 163, 241, 220, 45, 226),
  group = c("Group A", "Group A", "Group A", "Group A",
            "Group B", "Group B", "Group B", "Group B",
            "Group C", "Group C", "Group C", "Group C",
            "Group D", "Group D", "Group D", "Group D",
            "Group E", "Group E", "Group E", "Group E",
            "Group F", "Group F", "Group F", "Group F")
)

team_lookup_2023 <- team_lookup_2023 %>%
  left_join(group_mapping_2023, by = "country_id")

team_lookup_2019 <- team_lookup_2019 %>%
  left_join(group_mapping_2019, by = "country_id")

matches_with_home_group_2023 <- matchesFInt2023 %>%
  left_join(team_lookup_2023 %>% select(country_id, group) %>% rename(home_team.country.id = country_id, home_team.group = group), 
            by = "home_team.country.id")

matches_with_home_group_2019 <- matchesFInt2019 %>%
  left_join(team_lookup_2019 %>% select(country_id, group) %>% rename(home_team.country.id = country_id, home_team.group = group), 
            by = "home_team.country.id")

matchesFInt2023 <- matches_with_home_group_2023 %>%
  left_join(team_lookup_2023 %>% select(country_id, group) %>% rename(away_team.country.id = country_id, away_team.group = group), 
            by = "away_team.country.id")

matchesFInt2019 <- matches_with_home_group_2019 %>%
  left_join(team_lookup_2019 %>% select(country_id, group) %>% rename(away_team.country.id = country_id, away_team.group = group), 
            by = "away_team.country.id")

calculate_goals <- function(matchesFInt, year) {
  total_goals_scored <- matchesFInt %>%
    pivot_longer(cols = c(home_score, away_score), 
                 names_to = "location", 
                 values_to = "score") %>%
    mutate(Country.Id = ifelse(location == "home_score", home_team.country.id, away_team.country.id),
           Country.Name = ifelse(location == "home_score", home_team.country.name, away_team.country.name),
           Season.Season_Name = as.character(year)) %>%
    group_by(Country.Id, Country.Name, Season.Season_Name) %>%
    summarise(total_goals_scored = sum(score, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(total_goals_scored))
  
  total_goals_conceded <- matchesFInt %>%
    pivot_longer(cols = c(home_score, away_score), 
                 names_to = "location", 
                 values_to = "score") %>%
    mutate(Country.Id = ifelse(location == "home_score", away_team.country.id, home_team.country.id),
           Country.Name = ifelse(location == "home_score", away_team.country.name, home_team.country.name),
           Season.Season_Name = as.character(year)) %>%
    group_by(Country.Id, Country.Name, Season.Season_Name) %>%
    summarise(total_goals_conceded = sum(score, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(total_goals_conceded))
  
  total_goals <- total_goals_scored %>%
    full_join(total_goals_conceded, by = c("Country.Id", "Country.Name", "Season.Season_Name"))
  
  return(total_goals)
}


total_goals_2023 <- calculate_goals(matchesFInt2023, 2023)
total_goals_2019 <- calculate_goals(matchesFInt2019, 2019)

matchesFInt <-rbind(matchesFInt2019,matchesFInt2023)

lineupsFInt2019 <- data.frame()

for(i in 1:nrow(matchesFInt2019)){
  lineup <- cleanlineups(get.lineupsFree(matchesFInt2019[i,]))
  lineupsFInt2019 <- rbind(lineupsFInt2019,lineup)
}

lineupsFInt2023 <- data.frame()

for(i in 1:nrow(matchesFInt2023)){
  lineup <- cleanlineups(get.lineupsFree(matchesFInt2023[i,]))
  lineupsFInt2023 <- rbind(lineupsFInt2023,lineup)
}

eventsFInt2019 <- allclean(free_allevents(MatchesDF = matchesFInt2019, Parallel = T))
eventsFInt2023 <- allclean(free_allevents(MatchesDF = matchesFInt2023, Parallel = T))

eventsFInt2019 <- subset(eventsFInt2019, select = -c(tactics.lineup))
eventsFInt2023 <- subset(eventsFInt2023, select = -c(tactics.lineup))

list_columns_2019 <- sapply(eventsFInt2019, is.list) 
list_column_names_2019 <- names(eventsFInt2019)[list_columns_2019]

eventsFInt2019 <- eventsFInt2019 %>%
  select(-all_of(list_column_names_2019))

list_columns_2023 <- sapply(eventsFInt2019, is.list) 
list_column_names_2023 <- names(eventsFInt2019)[list_columns_2023]

eventsFInt2023 <- eventsFInt2023 %>%
  select(-all_of(list_column_names_2023))


lineupsFInt2019 <- lineupsFInt2019 %>%
  unnest_wider(positions, names_sep = "_")%>%
  unnest_wider(cards, names_sep = "_")

lineupsFInt2023 <- lineupsFInt2023 %>%
  unnest_wider(positions, names_sep = "_")%>%
  unnest_wider(cards, names_sep = "_")

diff_cols <- setdiff(colnames(eventsFInt2023), colnames(eventsFInt2019))
print(diff_cols)
eventsFInt2019 <- eventsFInt2019[, setdiff(colnames(eventsFInt2019), diff_cols)]
eventsFInt2023 <- eventsFInt2023[, setdiff(colnames(eventsFInt2023), diff_cols)]

diff_cols <- setdiff(colnames(eventsFInt2019), colnames(eventsFInt2023))
print(diff_cols)
eventsFInt2019 <- eventsFInt2019[, setdiff(colnames(eventsFInt2019), diff_cols)]
eventsFInt2023 <- eventsFInt2023[, setdiff(colnames(eventsFInt2023), diff_cols)]

eventsFInt <- rbind(eventsFInt2019,eventsFInt2023)
lineupsFInt <- rbind(lineupsFInt2019, lineupsFInt2023)

eventsFInt <- eventsFInt%>%
  mutate(card = case_when(
    foul_committed.card.name == "Yellow Card" | foul_committed.card.name == "Second Yellow" |
      bad_behaviour.card.name == "Yellow Card"  ~ "Yellow Card",
    foul_committed.card.name == "Red Card" | bad_behaviour.card.name == "Red Card" ~ "Red Card",
    is.na(foul_committed.card.name) & is.na(bad_behaviour.card.name) ~ NA_character_,
    TRUE ~ NA_character_
  ))

numeric_columns <- sapply(eventsFInt, is.numeric)
eventsFInt[numeric_columns] <- lapply(eventsFInt[numeric_columns], function(x) ifelse(is.na(x), 0, x))

team_lookup <- matchesFInt %>%
  select(team_name = home_team.home_team_name, country_name = home_team.country.name) %>%
  bind_rows(matchesFInt %>% select(team_name = away_team.away_team_name, country_name = away_team.country.name)) %>%
  distinct()

eventsFInt <- eventsFInt %>%
  left_join(team_lookup, by = c("team.name" = "team_name"))

determine_winner <- function(home_score, away_score, home_team.country.name, away_team.country.name) {
  if (home_score > away_score) {
    return(home_team.country.name)
  } else if (away_score > home_score) {
    return(away_team.country.name)
  } else {
    return("Draw")
  }
}

matchesFInt <- matchesFInt %>%
  rowwise() %>%
  mutate(winner = determine_winner(home_score, away_score, home_team.country.name, away_team.country.name)) %>%
  ungroup() %>%
  left_join(
    eventsFInt %>%
      filter(period == 5, type.name == "Shot", shot.outcome.name == "Goal") %>%
      group_by(match_id) %>%
      summarise(goals = n(), country_name = first(country_name), .groups = 'drop') %>%
      group_by(match_id) %>%
      filter(goals == max(goals)) %>%
      slice(1) %>%
      ungroup(),
    by = c("match_id" = "match_id")
  ) %>%
  mutate(winner = ifelse(!is.na(country_name), country_name, winner)) %>%
  select(-country_name, -goals)


matchesFInt <- matchesFInt %>%
  mutate(competition_stage.name = ifelse(competition_stage.name == "Regular Season", "Group Stage", competition_stage.name))

totalGoals <- rbind(total_goals_2019,total_goals_2023)


matchesFInt <- matchesFInt %>%
  mutate(
    competition_stage.id = ifelse(
      match_id %in% qf$match_id &
        home_team.country.name %in% qf$home_team.country.name &
        away_team.country.name %in% qf$away_team.country.name,
      11, competition_stage.id
    ),
    competition_stage.name = ifelse(
      match_id %in% qf$match_id &
        home_team.country.name %in% qf$home_team.country.name &
        away_team.country.name %in% qf$away_team.country.name,
      "Quarter-finals", competition_stage.name
    )
  )

eventsFInt$shot.statsbomb_xg <- eventsFInt$shot.statsbomb_xg <- round(eventsFInt$shot.statsbomb_xg, 4)


data <- data.frame(
  Rank = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36),
  Country = c("Spain", "United States of America", "France", "Norway", "Germany", "Australia", "Brazil", "England", "Canada", "Argentina", "New Zealand", "Nigeria",
              "South Africa", "Japan", "Jamaica", "Netherlands", "Sweden", "Korea (South)", "China", "Italy", "Chile", "Scotland", "Thailand", "Cameroon",
              "Philippines", "Zambia", "Denmark", "Colombia", "Switzerland", "Portugal", "Panama", "Costa Rica", "Ireland", "Haiti", "Vietnam", "Morocco"),
  X = c(rep(1, 12), rep(2, 12), rep(3, 12)),
  Y = c(rep(1, 12), rep(2, 12), rep(3, 12))
)

# Reorder the ranks within each group
data <- data %>%
  arrange(Y, X) %>%
  group_by(Y) %>%
  mutate(X = row_number()) %>%
  select(Country, X, Y) %>%
  ungroup()




write.csv(data, file = "world_cup_teams.csv", row.names = FALSE)

#write_csv(team_lookup_2019, file = "team_lookup_2019.csv")
#write_csv(team_lookup_2023, file = "team_lookup_2023.csv")
#write_csv(total_goals_2019, file = "total_goals_2019.csv")
#write_csv(total_goals_2023, file = "total_goals_2023.csv")
#write_csv(matchesFInt2019, file = "matchesFInt2019.csv")
#write_csv(matchesFInt2023, file = "matchesFInt2023.csv")
write_csv(matchesFInt, file = "matchesFInt.csv")
write_csv(totalGoals, file = "totalGoals.csv")
#write_csv(eventsFInt2019, file = "eventsFInt2019.csv")
#write_csv(eventsFInt2023, file = "eventsFInt2023.csv")
#write_csv(lineupsFInt2019, file = "lineupsFInt2019.csv")
#write_csv(lineupsFInt2023, file = "lineupsFInt2023.csv")
write_csv(eventsFInt, file = "eventsFInt.csv")
write_csv(lineupsFInt, file = "lineupsFInt.csv")

