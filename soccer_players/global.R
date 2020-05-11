library(shiny)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(lubridate)
library(ggplot2)
library(DT)
library(maps)
library(viridis)
library(leaflet)
library(sf)
library(shinyWidgets)
library(kableExtra)
library(shinyjs)
library(fmsb)
library(plotly)
theme_set(
  theme_void()
)

fm_df = read.csv('/home/dantole/Desktop/NYCDSA/Project_2/soccer/players_fm.csv', header = TRUE, stringsAsFactors = FALSE)

values_df = read.csv('/home/dantole/Desktop/NYCDSA/Project_2/soccer/European_Rosters.csv', header = TRUE, stringsAsFactors = FALSE)

fm_df = fm_df %>%
  select(., -Age, -IntCaps, -IntGoals, Birth.Date = Born)

values_df = values_df %>%
  select(., Name = PlayerName, 
         Club = Affiliation, 
         League, 
         Birth.Date, 
         Foot, 
         Agent, 
         Contract.Expiration.Date = ContractExpiration, 
         Youth.Club.1, 
         Games.Played, 
         Market.Value..Euros., 
         Accumulated.Transfer.Sums..Euros., 
         Highest.Market.Value..Euros., 
         Highest.Market.Value.Date)

# Pre-processing of both tables

# Process dates
# -----------------------------------------------------
clean_date_str <- function(date)
{
  return(lubridate::parse_date_time(date, orders = c("dmy", "mdy", "ymd", "ydm")))
  # return(lubridate::dmy(date))
}

fmt_df_dates <- function(df)
{
  df %>%
    mutate_at(., vars(contains(".Date")), .funs = clean_date_str)
}

fm_df = fmt_df_dates(fm_df)
values_df = fmt_df_dates(values_df)

# -----------------------------------------------------

# Pricess names players
clean_name <- function(name)
{
  name = tolower(name)
  name = iconv(name, to="ASCII//TRANSLIT")
  gsub("[^(a-z | |-|')]", "", name)
}

fm_df = fm_df %>%
  mutate(., Name = tolower(clean_name(Name)))
values_df = values_df %>%
  mutate(., Name = clean_name(Name))

# -----------------------------------------------------

# Joint tables
df = fm_df %>% 
  left_join(., values_df, by = c("Name", "Birth.Date"))
# -----------------------------------------------------

# Nation ID's
countries = read.delim('/home/dantole/Desktop/NYCDSA/Project_2/soccer/countries.txt', header = FALSE, col.names = c('NationID', 'Country'), sep = ',', stringsAsFactors = FALSE)

# Tranform nationID format to couontry name
df = df %>% left_join(., countries, by="NationID")

df = df %>% select(., -NationID)

# Adding age (as of 01/01/2017) feature
date_ref = dmy("01/01/2017")

df = df %>% mutate(., Age = floor(as.numeric(as.duration(interval(start = Birth.Date, end = date_ref)))/(3600*24*365)))

# Not so lazy categorization of attributes (by hand)
Physical = c('AerialAbility', 'Heading', 'Acceleration', 'Jumping', 'NaturalFitness', 'Pace', 'Stamina', 'Strength', 'InjuryProness')
Technical = c('Corners', 'Crossing', 'Passing', 'Balance', 'Dribbling', 'FirstTouch', 'Technique', 'Agility')
Shooting = c('Finishing', 'LongShots', 'PenaltyTaking', 'Freekicks')
Other = c('Longthrows', 'RightFoot', 'LeftFoot', 'Dirtiness')
Defense = c('Marking', 'Tackling', 'Aggression', 'Anticipation')
Mental = c('Bravery', 'Composure', 'Concentration', 'Determination', 'Flair', 'Leadership', 'ImportantMatches', 'Consistency')
Tactical = c('Vision', 'Decisions', 'CommandOfArea', 'OffTheBall', 'Positioning', 'Teamwork', 'Workrate')
Personality = c('Versatility', 'Adaptability', 'Ambition', 'Loyalty', 'Pressure', 'Professional', 'Sportsmanship', 'Temperament', 'Controversy')
Goal = c('Communication', 'Eccentricity', 'Handling', 'Kicking', 'OneOnOnes', 'Reflexes', 'RushingOut', 'TendencyToPunch', 'Throwing')
Positioning = c('Goalkeeper', 'Sweeper', 'Striker', 'AttackingMidCentral', 'AttackingMidLeft', 'AttackingMidRight', 'DefenderCentral', 'DefenderLeft', 'DefenderRight', 'DefensiveMidfielder', 'MidfielderCentral', 'MidfielderLeft', 'MidfielderRight', 'WingBackLeft', 'WingBackRight')

Categories = list(Physical, Technical, Shooting, Other, Defense, Mental, Tactical, Personality, Goal)

names(Categories) = c('Physical', 'Technical', 'Shooting', 'Other', 'Defense', 'Mental', 'Tactical', 'Personality', 'Goal')

df = df %>% mutate(., Physical_agg = floor(rowMeans(df[Physical])),
                   Technical_agg = floor(rowMeans(df[Technical])),
                   Shooting_agg = floor(rowMeans(df[Shooting])),
                   Defense_agg = floor(rowMeans(df[Defense])),
                   Mental_agg = floor(rowMeans(df[Mental])),
                   Tactical_agg = floor(rowMeans(df[Tactical])),
                   Personality_agg = floor(rowMeans(df[Personality])),
                   Other_agg = floor(rowMeans(df[Other])),
                   Goal_agg = floor(rowMeans(df[Goal])))

# -----------------------------------------------------

# Predicting prices
data_pred = df %>% 
  mutate(., log_price = log(Market.Value..Euros.)) %>%
  select(., Physical, Technical, Shooting, Defense, Mental, Tactical, Personality, Positioning, Other, Goal, Age, Height, Weight, log_price)

# data_train = data_pred %>%
#   filter(., !is.na(log_price))

# smp_size <- floor(0.75 * nrow(data_train))
# 
# set.seed(42)
# train_ind <- sample(seq_len(nrow(data_train)), size = smp_size)
# 
# data_train <- data_train[train_ind, ]
# data_test <- data_train[-train_ind, ]
# 
# 
# X_train = data_train %>%
#   select(., -Market.Value..Euros.)
# Y_train = data_train %>%
#   mutate(., log_price = log(Market.Value..Euros.)) %>%
#   select(., Market.Value..Euros.)
# X_test = data_test %>%
#   select(., -Market.Value..Euros.)
# Y_test = data_test %>%
#   mutate(., log_price = log(Market.Value..Euros.)) %>%
#   select(., Market.Value..Euros.)
features = data_pred %>% select(., -log_price)

model_= paste("data_pred$", names(features), sep = "", collapse=" + ")

text = paste("data_pred$log_price ~" , model_)

# data_train = data_pred

Prediction_list = predict(lm(text, method = "qr"), data_pred)
Prediction_list = exp(Prediction_list)

Prediction_list[[1]]

df = df %>%
  mutate(., Prediction = round(Prediction_list))

density1 = density(Prediction_list)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Prediction', fill = 'tozeroy',
               fillcolor = 'rgba(168, 216, 234, 0.5)',
               line = list(width = 0.5))
fig

density(df$Height)
# -----------------------------------------------------

# # Retrievethe map data
#  some.eu.maps <- map_data("world", region = countries$country)
# 
# # Compute the centroid as the mean longitude and lattitude
# # Used as label coordinate for country's names
# 
# region.lab.data <- some.eu.maps %>%
#    group_by(region) %>%
#    summarise(long = mean(long), lat = mean(lat))
# 
# ggplot(some.eu.maps, aes(x = long, y = lat)) +
#    geom_polygon(aes( group = group, fill = region))+
#    geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#    scale_fill_viridis_d()+
#    theme_void()+
#    theme(legend.position = "none")

# -----------------------------------------------------

# df %>% filter(., !is.na(Market.Value..Euros.))
 
# notInfm = values_df %>% 
#   left_join(., fm_df, by = c("Name", "Birth.Date")) %>% 
#   filter(., is.na(CommandOfArea))


