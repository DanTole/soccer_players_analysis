library(shiny)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(DT)
require(maps)
require(viridis)
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
fmt_df_country <- function(nationId)
{
  countries[countries$key == nationId, 2]
}

df = df %>% left_join(., countries, by="NationID")

df = df %>% select(., -NationID)

# -----------------------------------------------------

# Retrievethe map data
# some.eu.maps <- map_data("world", region = countries$country)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names

# region.lab.data <- some.eu.maps %>%
#   group_by(region) %>%
#   summarise(long = mean(long), lat = mean(lat))
# 
# ggplot(some.eu.maps, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region))+
#   geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")
# -----------------------------------------------------

# df %>% filter(., !is.na(Market.Value..Euros.))
 
# notInfm = values_df %>% 
#   left_join(., fm_df, by = c("Name", "Birth.Date")) %>% 
#   filter(., is.na(CommandOfArea))


