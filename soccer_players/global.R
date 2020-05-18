library(shiny)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(lubridate)
library(ggplot2)
library(DT)
library(caret)
library(maps)
library(viridis)
library(leaflet)
library(sf)
library(shinyWidgets)
library(kableExtra)
library(shinyjs)
library(fmsb)
library(plotly)
library(ggbiplot)
theme_set(
  theme_void()
)

fm_df = read.csv('./data/players_fm.csv', header = TRUE, stringsAsFactors = FALSE)

values_df = read.csv('./data/European_Rosters.csv', header = TRUE, stringsAsFactors = FALSE)

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
countries = read.delim('./data/countries.txt',
                       header = FALSE, 
                       col.names = c('NationID', 'Country'), 
                       sep = ',', stringsAsFactors = FALSE)

# Tranform nationID format to couontry name
df = df %>% left_join(., countries, by="NationID")

df = df %>% select(., -NationID)

# Adding age (as of 01/01/2017) feature
date_ref = dmy("01/01/2017")

df = df %>% mutate(., Age = floor(as.numeric(as.duration(interval(start = Birth.Date, end = date_ref)))/(3600*24*365)))

# Position feature
clean_position <- function(position)
{
  sapply(strsplit(position, " "), "[", 1)
}

df = df %>%
  mutate(., Position = clean_position(PositionsDesc))

list_position = c("DM", "GK", "S", "M", "D", "AM", "WB", "C")

# Not so lazy categorization of attributes (by hand)
Physical = c('Heading', 'Acceleration', 'Jumping', 'NaturalFitness', 'Pace', 'Stamina', 'Strength', 'InjuryProness')
Technical = c('Corners', 'Crossing', 'Passing', 'Balance', 'Dribbling', 'FirstTouch', 'Technique', 'Agility')
Shooting = c('Finishing', 'LongShots', 'PenaltyTaking', 'Freekicks')
Other = c('Longthrows', 'RightFoot', 'LeftFoot', 'Dirtiness')
Defense = c('Marking', 'Tackling', 'Aggression', 'Anticipation')
Mental = c('Bravery', 'Composure', 'Concentration', 'Determination', 'Flair', 'Leadership', 'ImportantMatches', 'Consistency')
Tactical = c('Vision', 'Decisions', 'CommandOfArea', 'OffTheBall', 'Positioning', 'Teamwork', 'Workrate')
Personality = c('Versatility', 'Adaptability', 'Ambition', 'Loyalty', 'Pressure', 'Professional', 'Sportsmanship', 'Temperament', 'Controversy')
Goal = c('Communication', 'Eccentricity', 'Handling', 'Kicking', 'OneOnOnes', 'Reflexes', 'RushingOut', 'TendencyToPunch', 'Throwing')
Positioning = c('AerialAbility', 'Goalkeeper', 'Sweeper', 'Striker', 'AttackingMidCentral', 'AttackingMidLeft', 'AttackingMidRight', 'DefenderCentral', 
                'DefenderLeft', 'DefenderRight', 'DefensiveMidfielder', 'MidfielderCentral', 'MidfielderLeft', 'MidfielderRight', 'WingBackLeft', 'WingBackRight')

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

features = data_pred %>% select(., -log_price)

model_= paste("data_pred$", names(features), sep = "", collapse=" + ")

text = paste("data_pred$log_price ~" , model_)

# data_train = data_pred

lin = lm(text)

Prediction_list = predict(lin, data_pred)
Prediction_list = exp(Prediction_list)

Prediction_list[[1]]

df = df %>%
  mutate(., Prediction = round(Prediction_list))
