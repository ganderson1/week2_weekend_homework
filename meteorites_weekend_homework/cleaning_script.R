library(tidyverse)
library(janitor)
library(assertr)
#load data file
meteorite_raw <- read_csv("meteorite_landings.csv") %>% 
  #check column headers are as expected
  verify(has_all_names("id", "name", "mass (g)", "fall", "year", "GeoLocation")) %>% 
  #clean names as per tidyr
  clean_names()

#create new subset of cleaned data
meteorite_clean <- meteorite_raw %>%
  #split geolocation into latitude and longitude
  mutate(geo_location = gsub('[()°]', '', geo_location)) %>% 
  separate(col = geo_location, into = c("latitude", "longitude"), sep = '\\,', convert = TRUE) %>% 
  #replace NA values in lat and long zith zeroes
  replace_na(list(latitude = 0, longitude = 0)) %>% 
  #remove mass obs of less than 1000g
  filter(mass_g >= 1000) %>%
  #check lat values are valid
  assert(within_bounds(-90,90), latitude) %>% 
  #check long values are valid
  assert(within_bounds(-180,180), longitude) %>% 
  #arrange values by year of discovery
  arrange(year) %>% 
  #write new csv file with cleaned and arranged data
  write_csv( file = "meteorite_landings_clean.csv")
