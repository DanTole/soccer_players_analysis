library(shiny)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(DT)

fm_df = read.csv('/home/dantole/Desktop/NYCDSA/Project_2/soccer/players_fm.csv', header = TRUE, stringsAsFactors = FALSE)

values_df = read.csv('/home/dantole/Desktop/NYCDSA/Project_2/soccer/European_Rosters.csv', header = TRUE, stringsAsFactors = FALSE)

fm_df = fm_df %>%
  select(., -Age, -IntCaps, -IntGoals, Birth.Date = Born)

values_df = values_df %>%
  select(., Name = PlayerName, Club = Affiliation, League, Birth.Date, Foot, Agent, Contract.Expiration.Date = ContractExpiration, Youth.Club.1, Games.Played, Market.Value..Euros., Accumulated.Transfer.Sums..Euros., Highest.Market.Value..Euros., Highest.Market.Value.Date)

# Pre-processing of both tables

# Process dates
# -----------------------------------------------------
clean_date_str <- function(date)
{
  return(lubridate::parse_date_time(date, orders = c("dmy", "mdy")))
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
  gsub("[^a-z | |^-|']", "", name)
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

# df %>% filter(., !is.na(Market.Value..Euros.))
 
# notInfm = values_df %>% 
#   left_join(., fm_df, by = c("Name", "Birth.Date")) %>% 
#   filter(., is.na(CommandOfArea))







