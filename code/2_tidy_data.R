######################################
# Data wrangling
######################################

library(dplyr)        # Flexible tools for data manipulation
library(ggplot2)      # Powerful graphing package (incl. map)
library(maps)         # Provides functions that let us plot the maps
library(mapdata)      # Contains the hi-resolution points that mark out the countries

### Raw data locations
un_sdg_file = "./raw_data/bigquery_un_sdg.rda"
gapminder_file = "./raw_data/gapminder_country_list.rda"
world_bank_file = "./raw_data/world_bank_population_data.rda"

### Tidy data destinations
tidy_file = "./rdas/tidy_dat.rda"
world_map_file = "./rdas/world_map.rda"

### Attach raw data to namespace
if(!exists("un_sdg_data")) load(un_sdg_file); rm(un_sdg_file)
if(!exists("gapminder_country_list")) load(gapminder_file); rm(gapminder_file)
if(!exists("wb_pop")) load(world_bank_file); rm(world_bank_file)

### Join UN SDG and Gapminder dataset such that:
# > Drop from UN SDG all countries not in Gapminder
# > Append to UN SDG the region column from Gapminder
dat <- inner_join(un_sdg_data, gapminder_country_list)

### Append World Bank population data, by country and year
# Rename and select World Bank columns to match UN SDG dataset
wb_pop <- wb_pop %>%
  rename(geo = iso3c, timeperiod = date, population = value) %>%
  select(geo, timeperiod, population)

# Append population column to UN SDG dataset
dat <- left_join(dat, wb_pop)

### Indicators to keep
indicators <- c("6.1.1", "7.1.1", "7.2.1", "8.1.1", "9.4.1",
                "12.2.1", "12.2.2", "17.6.2", "17.8.1")

### Description of indicators
  # 6.1.1. Pop.prop using safe drinking water services
  # 7.1.1. Pop.prop w/ access to electricity
  # 7.2.1. Renewable energy (share of total consumption)
  # 8.1.1. Annual growth rate real GDP per capita
  # 9.4.1. CO2 emission per unit of value added
  # 12.2.1. Material footprint (3x groups)
  # 12.2.2. Domestic material consumption (3x groups)
  # 17.6.2. Pop.prop w/ broadband subscriptions
  # 17.8.1. Pop.prop using the internet
# Source: https://unstats.un.org/sdgs/indicators/indicators-list/

### Text description of targets for indicators (https://undocs.org/A/RES/71/313)
target_description <-
  c('By 2030, achieve universal and equitable access to safe and affordable drinking water for all',
    'By 2030, ensure universal access to affordable, reliable and modern energy services',
    'By 2030, increase substantially the share of renewable energy in the global energy mix',
    'Sustain per capita economic growth in accordance with national circumstances and, in particular, at least 7 per cent gross domestic product growth per annum in the least developed countries',
    'By 2030, upgrade infrastructure and retrofit industries to make them sustainable, with increased resource-use efficiency and greater adoption of clean and environmentally sound technologies and industrial processes, with all countries taking action in accordance with their respective capabilities',
    'By 2030, achieve the sustainable management and efficient use of natural resources',
    'By 2030, achieve the sustainable management and efficient use of natural resources',
    'Enhance North-South, South-South and triangular regional and international cooperation on and access to science, technology and innovation and enhance knowledge-sharing on mutually agreed terms, including through improved coordination among existing mechanisms, in particular at the United Nations level, and through a global technology facilitation mechanism',
    'Fully operationalize the technology bank and science, technology and innovation capacity-building mechanism for least developed countries by 2017 and enhance the use of enabling technology, in particular information and communications technology')

# Data frame of indicators and text descriptions (to be merged after dplyr manipulation of dat)
descriptions_dat <- data.frame(indicators, target_description) %>%
  rename(indicator = indicators)

### Manipulate columns with dplyr (see indented comments for details)
dat <- dat %>%
  
  # Filter for chosen indicators
  filter(indicator %in% indicators) %>%
  
  # Remove seriescode, geoareacode, time_detail, freq
  select(-c('goal', 'seriescode', 'geoareacode', 'time_detail', 'freq')) %>%
  
  # Rename geoareaname to country, timeperiod to year
  rename(country = geoareaname, year = timeperiod) %>%
  
  # Capitalize region column
  mutate(region = sub("(.)", "\\U\\1", region, perl=TRUE)) %>%
  
  # Remove empty columns
  select_if(~sum(!is.na(.)) > 0) %>%
  
  # Coerce the type of each column
  mutate(target = as.factor(target),
         indicator = as.factor(indicator),
         year = as.integer(year),
         value = as.numeric(value),
         nature = as.factor(nature),
         location = as.factor(location),
         type_of_product = as.factor(type_of_product),
         type_of_speed = as.factor(type_of_speed),
         units = as.factor(units),
         region = as.factor(region))

# Use value and population to create column containing the absolute number of people
dat <- dat %>%
  mutate(nr_people = as.integer(floor(population*value/100)))

# Append target descriptions to dat
dat <- left_join(dat, descriptions_dat)

### Save map coordinates in dataframe
map_world <- map_data('world')

# Rename region as country to harmonise with 'dat'
map_world <- map_world %>%
  rename(country = region)

# List all mismatches in country names between dat and map_world
# dat %>%
#   anti_join(map_world, by = 'country') %>%
#   select(country) %>%
#   unique()

# Rename countries in map_world to match those of dat
dat <- dat %>%
  mutate(country = recode(country,
                          'Antigua and Barbuda' = 'Antigua',
                          'Holy See' = 'Vatican',
                          'Trinidad and Tobago' = 'Trinidad')) %>%
  
  # Remove rows with country Tuvalu
  filter(country != 'Tuvalu')

### Save data frame to file
save(dat, file = tidy_file)
save(map_world, file = world_map_file)

### Clear namespace except for dat
rm(list = setdiff(ls(), c('dat', 'map_world')))