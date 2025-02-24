install.packages("devtools")
install.packages("remotes")
remotes::install_version("SDMTools", "1.1-221")
devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")

library(StatsBombR)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(SBpitch)
library(RColorBrewer)
library(grDevices)

allComps <- FreeCompetitions()
matches <- FreeMatches(allComps)

barcaID <- matches[matches$home_team.home_team_name == "Barcelona" | matches$away_team.away_team_name == "Barcelona"  ,]
matchID <- c()
for (i in 1:nrow(barcaID)){
  match <- barcaID[i,]
  count <- 0
  
  lineups <- cleanlineups(get.lineupsFree(match))
  if(any(lineups$player_name == "Lionel Andrés Messi Cuccittini")){
    matchID <- c(matchID, match$match_id)
    messiID <- lineups[lineups$player_name == "Lionel Andrés Messi Cuccittini","player_id"]
  }
  else{
    
  }
}

messiMatches <- barcaID[barcaID$match_id %in% matchID,]

messiMatches <- messiMatches[messiMatches$competition.competition_id == 11,]

events <- allclean(free_allevents(MatchesDF = messiMatches, Parallel = T))

events <- events[events$player.id == messiID,]

events <- subset(events, select = -c(tactics.lineup))

colnames1 <- names(event)
colnames2 <- names(events)

uniqueColnames <- setdiff(colnames2, colnames1)
print(uniqueColnames)

list_columns <- sapply(events, is.list)
list_column_names <- names(events)[list_columns]

events <- events %>%
  select(-all_of(list_column_names))


list_columnsM <- sapply(messiMatches, is.list)
list_column_namesM <- names(messiMatches)[list_columnsM]

messiMatches <- messiMatches %>%
  select(-all_of(list_column_namesM))

messiMatches$opposition <- ifelse(messiMatches$home_team.home_team_name == "Barcelona", 
                                  messiMatches$away_team.away_team_name, 
                                  messiMatches$home_team.home_team_name)

write.csv(messiMatches, file = "matches.csv", row.names = FALSE)

write.csv(events, file = "events.csv", row.names = FALSE)

####################################################################################


events <- read.csv("events.csv")
mMatches <- read.csv("matches.csv")

events <- events[!events$competition_id != 11,]
events <- events[!is.na(events$competition_id),]

soccer_teams <- list(
  "Málaga" = "Málaga",
  "Osasuna" = "Pamplona",
  "Albacete" = "Albacete",
  "Levante UD" = "Valencia",
  "Getafe" = "Getafe",
  "Espanyol" = "Barcelona",
  "Deportivo Alavés" = "Vitoria-Gasteiz",
  "Villarreal" = "Villarreal",
  "Celta Vigo" = "Vigo",
  "Real Betis" = "Sevilla",
  "Racing Santander" = "Santander",
  "Real Zaragoza" = "Zaragoza",
  "Atlético Madrid" = "Madrid",
  "Mallorca" = "Palma de Mallorca",
  "Sevilla" = "Sevilla",
  "Real Madrid" = "Madrid",
  "Athletic Club" = "Bilbao",
  "Cádiz" = "Cádiz",
  "Gimnàstic Tarragona" = "Tarragona",
  "Valencia" = "Valencia",
  "Recreativo Huelva" = "Huelva",
  "RC Deportivo La Coruña" = "La Coruña",
  "Real Sociedad" = "San Sebastián",
  "Real Murcia CF" = "Murcia",
  "Almería" = "Almería",
  "Real Valladolid" = "Valladolid",
  "Sporting Gijón" = "Gijón",
  "CD Numancia de Soria" = "Soria",
  "Tenerife" = "Santa Cruz de Tenerife",
  "Xerez" = "Xerez",
  "Hércules" = "Alicante",
  "Rayo Vallecano" = "Madrid",
  "Granada" = "Granada",
  "Elche" = "Elche",
  "Eibar" = "Eibar",
  "Córdoba CF" = "Córdoba",
  "Las Palmas" = "Las Palmas",
  "Leganés" = "Leganés",
  "Girona" = "Girona",
  "Huesca" = "Huesca",
  "Barcelona" = "Barcelona"
)


map_cities <- function(location) {
  if (location %in% names(soccer_teams)) {
    return(soccer_teams[[location]])
  } else {
    return(NA_character_)
  }
}


mMatches <- transform(mMatches, cities = sapply(mMatches$home_team.home_team_name, map_cities))

assists_per_match <- events %>%
  filter(pass.goal_assist == TRUE, na.rm = TRUE) %>%
  group_by(match_id) %>%
  summarise(messi_assists = n())


goals_per_match <- events %>%
  filter(shot.outcome.name == "Goal", na.rm = TRUE) %>%
  group_by(match_id) %>%
  summarise(messi_goals = n())

shots_per_match <- events %>%
  filter(type.name == "Shot", na.rm = TRUE) %>%
  group_by(match_id) %>%
  summarise(messi_shots = n())


mMatches <- merge(mMatches, goals_per_match, by = "match_id", all.x = TRUE)
mMatches <- merge(mMatches, assists_per_match, by = "match_id", all.x = TRUE)
mMatches <- merge(mMatches, shots_per_match, by = "match_id", all.x = TRUE)

mMatches$messi_goals[is.na(mMatches$messi_goals)] <- 0
mMatches$messi_assists[is.na(mMatches$messi_assists)] <- 0
mMatches$messi_shots[is.na(mMatches$messi_shots)] <- 0

mMatches$totalGoalCon <- mMatches$messi_goals + mMatches$messi_assists

city_province_mapping <- data.frame(
  City = c("Sevilla", "Vitoria-Gasteiz", "Barcelona", "Getafe", "Eibar", "Girona",
           "Las Palmas", "Madrid", "Málaga", "Bilbao", "Leganés", "Valencia",
           "Vigo", "Villarreal", "La Coruña", "San Sebastián", "Valladolid",
           "Huesca", "Pamplona", "Albacete", "Zaragoza", "Cádiz", "Palma de Mallorca",
           "Tarragona", "Santander", "Huelva", "Gijón", "Soria", "Almería",
           "Murcia", "Xerez", "Santa Cruz de Tenerife", "Alicante", "Granada",
           "Córdoba", "Elche"),
  Province = c("Sevilla", "Álava", "Barcelona", "Madrid", "Gipuzkoa", "Gerona",
               "Las Palmas", "Madrid", "Málaga", "Bizkaia", "Madrid", "Valencia",
               "Pontevedra", "Castellón", "La Coruña", "Gipuzkoa", "Valladolid",
               "Huesca", "Navarra", "Albacete", "Zaragoza", "Cádiz", "Baleares",
               "Tarragona", "Cantabria", "Huelva", "Asturias", "Soria", "Almería",
               "Murcia", "Cádiz", "Santa Cruz de Tenerife", "Alicante", "Granada",
               "Córdoba", "Alicante")
)

mMatches <- mMatches %>%
  left_join(city_province_mapping, by = c("cities" = "City")) %>%
  rename(province = Province)


province_total <- mMatches %>%
  group_by(province) %>%
  summarise(total = sum(totalGoalCon, na.rm = TRUE),
            num_matches = n(),
            average = round(total / num_matches, 2))



spain_map <- ne_states(country = "Spain", returnclass = "sf")

mapInvolvement <- spain_map %>%
  left_join(province_total, by = c("name" = "province"))


palette <- colorRampPalette(c("#edbb00", "#a50044"))(9)

n_bins <- 150

mapInvolvement$total_discrete <- cut(mapInvolvement$total, breaks = n_bins)


#Plot 1
ggplot(mapInvolvement) +
  geom_sf(aes(fill = total_discrete)) +
  scale_fill_manual(values = palette) +
  theme_void() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
    axis.text.y = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"), 
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "#004d98")
  ) +
  labs(title = "Messi Goal Involvement by Province in Spain", x = "", y = "")



mMatches_count <- mMatches %>%
  group_by(season.season_name) %>%
  summarise(total_matches = n())

mMatches_sum <- mMatches %>%
  group_by(season.season_name) %>%
  summarise(messi_goals = sum(messi_goals),
            messi_assists = sum(messi_assists),
            messi_shots = sum(messi_shots),
            total = sum(messi_goals, messi_assists))

mMatchesFinal <- left_join(mMatches_sum, mMatches_count, by = "season.season_name")

mMatches_final <- mMatchesFinal %>%
  mutate(averageTotal = total / total_matches,
         averageGoal = messi_goals / total_matches,
         averageAssists = messi_assists / total_matches,
         shootingPercentage = round(((messi_goals / messi_shots)*100),2))





stacked_data <- mMatches_final %>%
  select(season.season_name, averageAssists, averageGoal) %>%
  gather(key = "statistic", value = "value", -season.season_name) %>%
  mutate(statistic = if_else(statistic == "averageAssists", "Assists", "Goals"))

stacked_data <- stacked_data %>%
  mutate(statistic = factor(statistic, levels = c("Goals", "Assists")))


#Plot 2

ggplot(stacked_data, aes(x = season.season_name, y = value, group = statistic, fill = statistic)) +
  geom_area(alpha = 1, position = "identity")  +
  labs(title = "Average Goals and Assists by Season",
       x = "Season",
       y = "Average",
       fill = "Statistic") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
    axis.text.y = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"), 
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "#004d98")
  ) +
  scale_fill_manual(values = c("Goals" = "#edbb00", "Assists" = "#a50044"))






#Plot 3
ggplot(mMatches_final, aes(x = season.season_name, group = 1)) +
  geom_line(aes(y = messi_goals, color = "Messi Goals")) +
  geom_line(aes(y = shootingPercentage, color = "Shooting Percentage")) +
  labs(x = "Season", y = "Average Shooting Percentage", title = "Average Shooting Percentage by Season") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
    axis.text.y = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "#004d98") 
  ) +
  scale_color_manual(values = c("Messi Goals" = "#edbb00", "Shooting Percentage" = "#a50044"))


top_left <- c(120, 18)
bottom_right <- c(120 - 18, 18 + 45)



goals <-  events %>%
  filter(shot.outcome.name == "Goal", na.rm = TRUE)

assists <-  events %>%
  filter(pass.goal_assist == TRUE, na.rm = TRUE)

x_80th_percentile <- quantile(assists$location.x, probs = 0.8, na.rm = TRUE)
y_80th_percentile <- quantile(assists$location.y, probs = 0.8, na.rm = TRUE)


goals <- goals %>%
  mutate(location = ifelse(
    location.x < top_left[1] & location.x > bottom_right[1] &
      location.y > top_left[2] & location.y < bottom_right[2],
    "Inside_Box",
    "Outside_Box"
  ))

goals_by_season <- goals %>%
  group_by(season_id, location) %>%
  summarise(count = n())%>%
  spread(location, count)


season_unique <- mMatches %>%
  distinct(season.season_id, .keep_all = TRUE)

goals_by_season <- goals_by_season %>%
  left_join(season_unique %>% select(season.season_id, season.season_name), by = c("season_id" = "season.season_id"))

goals_by_season <- goals_by_season %>%
  arrange(season.season_name)

long_goals <- goals_by_season %>%
  pivot_longer(cols = c(Inside_Box, Outside_Box), names_to = "Location", values_to = "Count")

# Plot4
ggplot(long_goals, aes(x = season.season_name, y = Count, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Inside_Box" = "#edbb00", "Outside_Box" = "#a50044")) +
  theme_minimal() +
  labs(title = "Goals Inside and Outside Box by Season",
       x = "Season",
       y = "Count",
       fill = "Location") +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
    axis.text.y = element_text(color = "white"), 
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"), 
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "#004d98")
  )







create_Pitch(grass_colour = "darkgreen", background_colour = "darkgreen", line_colour = "white") + 
  geom_density_2d_filled(data = goals, aes(x = location.x, y = location.y, fill = ..level..,), alpha = 0.7, 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "darkgreen", fill = "darkgreen"),
        plot.title = element_text(color = "white", hjust = .5, size = 22, vjust = -1),
        plot.subtitle = element_text(color = "white", hjust = .5, size = 10, vjust = -4),
        plot.caption = element_text(color = "white", hjust = .5, size = 10, vjust = 4)) +
  labs(title = "Heat Map Of Where Messi Scores From",
       subtitle = "LaLiga 2004-2021",
       ) 


create_Pitch(grass_colour = "darkgreen", background_colour = "darkgreen", line_colour = "white") + 
  geom_density_2d_filled(data = assists, aes(x = location.x, y = location.y, fill = ..level..,), alpha = .7, 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "darkgreen", fill = "darkgreen"),
        plot.title = element_text(color = "white", hjust = .5, size = 22, vjust = -1),
        plot.subtitle = element_text(color = "white", hjust = .5, size = 10, vjust = -4),
        plot.caption = element_text(color = "white", hjust = .5, size = 10, vjust = 4)) +
  labs(title = "Heat Map Of Where Messi Assists From",
       subtitle = "LaLiga 2004-2021",
  )

goals_vs_teams <- merge(goals_per_match, mMatches, by = "match_id", all.x = TRUE)


opposition_goals <- goals_vs_teams %>%
  group_by(opposition) %>%
  summarise(Total_Goals = sum(messi_goals.x, na.rm = TRUE))

ggplot(opposition_goals, aes(x = reorder(opposition, Total_Goals), y = Total_Goals, fill = "#a50044")) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
    axis.text.y = element_text(color = "white"), 
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"), 
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "#004d98")
  )+
  labs(title = "Total Goals Scored Against Each Opposition",
       x = "Opposition",
       y = "Total Goals")


