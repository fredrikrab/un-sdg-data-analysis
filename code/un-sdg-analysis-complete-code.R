######################################
# Download raw data and save locally
######################################

# Note: Add your own Google Cloud project ID in line 20.
# Note: googlesheets requires Google account authentication (browser opens automatically)

library(bigrquery)        # Query Google's BigQuery database
library(DBI)              # Interface for communication with relational database systems
library(googlesheets)     # Google spreadsheet API
library(wbstats)          # World Bank API
library(dplyr)            # Flexible tools for data manipulation

### Local destination for downloaded raw data
un_sdg_file = "./raw_data/bigquery_un_sdg.rda"
gapminder_file = "./raw_data/gapminder_country_list.rda"
wb_pop_file = "./raw_data/world_bank_population_data.rda"

### Download and save UN SDG data
if(!file.exists(un_sdg_file)){
  
  # Google Cloud project ID required for BigQuery interactions
  # Create your own project ID at https://console.cloud.google.com/
  google_cloud_project_id <- "edx-ds-capstone-2019"
  
  # BigQuery connection interface
  dbi_connection <- dbConnect(
    bigrquery::bigquery(),
    project = "publicdata",
    dataset = "un_sdg",
    billing = google_cloud_project_id
  )
  
  # SQL query for entire dataset (~300 MB)
  sql_query <- "SELECT * FROM `bigquery-public-data.un_sdg.indicators`"
  
  # Download dataset
  un_sdg_data <- dbGetQuery(dbi_connection, sql_query)
  
  # Save dataframe to file
  save(un_sdg_data, file = un_sdg_file)
  
}

### Download and save Gapminder country list
if(!file.exists(gapminder_file)){
  
  # Connect to spreadsheet (URL from https://www.gapminder.org/data/geo/)
  gapminder_spreadsheet <- gs_url("https://docs.google.com/spreadsheets/d/1qHalit8sXC0R8oVXibc2wa2gY7bkwGzOybEMTWp-08o/")
  
  # Download list-of-countries worksheet
  gapminder_country_list <- gapminder_spreadsheet %>%
    gs_read(ws = "list-of-countries-etc")
  
  # Keep only country* and region.
  #> Mutate colname to 'geoareaname' to match UN SDG data colname
  #> Make geo uppercase to match World Bank data
  gapminder_country_list <- gapminder_country_list %>%
    mutate(geoareaname = name, region = four_regions) %>%
    select(geo, geoareaname, region) %>%
    mutate(geo = toupper(geo))
  
  # Save dataframe to file
  save(gapminder_country_list, file = gapminder_file)
  
}

### Download and save World Bank population data
if(!file.exists(wb_pop_file)) {
  
  # Create list of ISO 3 country codes from Gapminder which will be used to query World Bank's data
  if(!exists('gapminder_country_list')) load(gapminder_file)
  iso3c_codes <- gapminder_country_list$geo
  
  # Remove HOS (Holy See) which isn't in World Bank data
  # https://datahelpdesk.worldbank.org/knowledgebase/articles/898590-country-api-queries
  iso3c_codes <- iso3c_codes[-which(iso3c_codes == 'HOS', )]
  
  # Get year interval for which there is UN SDG data
  if(!exists("un_sdg_data")) load(un_sdg_file);
  first_year <- min(un_sdg_data$timeperiod)
  last_year <- max(un_sdg_data$timeperiod)
  
  # Download population size from the World Bank
  wb_pop <- wb(country = iso3c_codes,
               indicator = 'SP.POP.TOTL',
               startdate = first_year, enddate = last_year)
  
  # Save dataframe to file
  save(wb_pop, file = wb_pop_file)
  
}

### Clear namespace except for dataframes
rm(list = setdiff(ls(), c("un_sdg_data", "gapminder_country_list", "wb_pop")))


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
tidy_file = "./tidy_data/tidy_dat.rda"
world_map_file = "./tidy_data/world_map.rda"

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

######################################
# Data exploration
######################################

library(dplyr)        # Flexible tools for data manipulation
library(ggplot2)      # Powerful graphing package
library(gridExtra)    # Arrange grid graphics
library(cowplot)      # Potential replacement of gridExtra
library(purrr)        # Functional programming toolkit - interested in map()
library(gifski)       # Convert image frames to GIF animations

### Tidy data locations
tidy_file = "./tidy_data/tidy_dat.rda"
world_map_file = "./tidy_data/world_map.rda"

### Attach tidy data to namespace
if(!exists("dat")) load(tidy_file); rm(tidy_file)
if(!exists("map_world")) load(world_map_file); rm(world_map_file)

######################################
# Functions
######################################


############   FILTERING   ############


### Filter for given indicator; delete empty columns
select_ind <- function(data, ind) {
  
  data %>%
    filter(indicator == ind) %>%
    select_if(~sum(!is.na(.)) > 0)
  
}


### Filter for given indicator and year; delete empty columns
select_ind_year <- function(data, ind, yr) {
  
  data %>%
    filter(indicator == ind, year %in% yr) %>%
    select_if(~sum(!is.na(.)) > 0)
  
}


### Filter for given indicator, year and loc; delete empty columns
select_ind_year_loc <- function(data, ind, yr, loc) {
  
  data %>%
    filter(indicator == ind, year %in% yr, location == loc) %>%
    select_if(~sum(!is.na(.)) > 0)
  
}


############   PLOTTING   ############


### Create world heatmap
plot_world_heatmap <- function(data, ind, year) {
  
  # Create filtered dataset
  data_filtered <- select_ind_year(data, ind, year)
  
  # Join filtered dataset with world map
  map_data <- left_join(map_world, data_filtered, by = 'country')
  
  # Text for title and legend
  title <- map_data$seriesdescription %>% na.omit() %>% unique()
  unit <- map_data$units %>% na.omit() %>% unique() %>% as.character()
  
  # Plot graph
  ggplot(map_data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = value)) +
    
    # Titles and axis
    labs(fill = paste(unit),
         title = paste(title),
         subtitle = paste('Year', year),
         x = NULL, y = NULL) +
    
    # Legend
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = 'lightgrey') +
    
    # Theme adjustments
    theme(plot.title = element_text(size = 9, face = 'bold'),
          plot.subtitle = element_text(size = 7, hjust = 0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.key.size = unit(6, 'pt'),
          legend.title = element_text(size = 3),
          legend.text = element_text(size = 4),
          panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA))
  
}


save_last_plot <- function(filename, w = 1280, h = 720, dpi = 300) {
  
  # Convert width and heigh from pixels into cm because ggsave() doesn't accept pixels as a unit
  w_mm <- w/dpi
  h_mm <- h/dpi
  
  ggsave(filename = paste(filename, '.png', sep = ''),
         plot = last_plot(),     #.Last.value
         path = './figs/',
         device = 'png',
         dpi = dpi,
         width = w_mm, height = h_mm, units = 'in')
  
}


############ CREATE GIF ############


### Create GIF of plot_world_heatmap series
gif_world_heatmap <- function(data, ind, start, end, filename = "animation", w = 1280, h = 720, d = 0.5) {
  
  # Create ggplots for interval given in arguments
  for (yr in start:end) {
    # Set PNG file specifications
    png(paste('./figs/', as.character(filename), '_', yr, '.png', sep= ''), width = w, height = h)
    # Print map to file
    print(plot_world_heatmap(data, ind, yr))
    # Close file
    dev.off()
  }
  
  # Create list of PNG filenames
  filenames <- as.character(seq(start, end))
  filenames <- paste('./figs/', filename, '_', filenames, '.png', sep= '')
  
  # Create GIF filename
  gif_filename <- paste('./figs/', filename, '.gif', sep = '')
  
  # Create GIF from PNG images
  gifski(filenames, delay = d, width = w, height = h,
         gif_file = gif_filename)
  
  # Delete PNG images
  file.remove(filenames)
  
  # Open GIF with default local application
  utils::browseURL(gif_filename)
  
}

### Create world heatmap, rural vs. urban 1x2 grid
facet_heatmap <- function(data, ind, yr) {
  
  # Create filtered datasets for each location
  data_urban <- select_ind_year_loc(data, ind, yr, "URBAN")
  data_rural <- select_ind_year_loc(data, ind, yr, "RURAL")
  
  # Joined filtered data with world map
  map_urban <- left_join(map_world, data_urban, by = 'country')
  map_rural <- left_join(map_world, data_rural, by = 'country')
  
  # Bind all data together
  map <- rbind(map_urban, map_rural)
  
  # Create data frame with facet property
  map <- data.frame(map, Facet = rep(c("map_urban","map_rural"),
                                     times=c(nrow(map_urban),nrow(map_rural))))
  
  # Text for plot title
  title <- map$seriesdescription %>% na.omit() %>% unique()
  
  # Plot graph
  ggplot(map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = value)) +
    
    # Labels and legend
    labs(title = "Population with access to electricity, by rural/urban",
         subtitle = paste('Year', yr),
         fill = '  %',
         x = NULL, y = NULL) +
    
    
    
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = 'lightgrey',
                        breaks = seq(0, 100, 20)) +
    
    # Theme adjustments
    theme(plot.title = element_text(size = 18, face = 'bold'),
          plot.subtitle = element_text(size = 16, hjust = 0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.key.size = unit(25, 'pt'),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA),
          plot.margin = unit(c(10,10,0,23),"mm")) + # top, right, bottom, left
    
    # Faceting
    facet_wrap(.~Facet)
  
}

### Create scatterplot, rural vs. urban 1x2 grid
facet_scatterplot <- function(data, ind, start, end, x_max = end) {
  
  # Create function that filters data used for plotting
  filter_data <- function(data, ind, start, end, loc) {
    
    dat %>%
      
      # Filter for indicator, given year interval and location
      filter(indicator == ind, year %in% c(start:end), location == loc) %>%
      
      # Add column for nr. of people covered (in millions) for all countries
      group_by(year) %>%
      mutate(total_people_million = sum(nr_people/10^6, na.rm = TRUE))
    
  }
  
  # Create filtered datasets for each location
  rural <- filter_data(data, ind, start, end, "RURAL")
  urban <- filter_data(data, ind, start, end, "URBAN")
  
  # Joined filtered data with world map
  plot <- rbind(rural, urban)
  
  # Create data frame with facet property
  plot <- data.frame(plot, Facet = rep(c("rural","urban"),
                                       times=c(nrow(rural),nrow(urban))))
  
  # Specify limits for plot
  x_min <- start
  #x_max <- end   note: x_max = end is default value of the function (can be overridden)
  y_min <- 3400
  y_max <- 6250
  
  # Plot graph
  plot %>%
    ggplot(aes(year, total_people_million)) +
    geom_point(size = 2) +
    geom_line(size = 0.1) +
    
    # Set limits
    scale_x_continuous("Year", breaks = seq(2000, 2015, by = 5), limits = c(x_min, x_max)) +
    scale_y_continuous("Million people", breaks = seq(3500, 6500, by = 500), limits = c(3400, 6750)) +
    
    # Labels
    labs(x = "Year") +
    
    # Theme adjustments
    theme(panel.background = element_rect(linetype = 5),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA),
          plot.margin = unit(c(0,34,10,6),"mm"),  # top, right, bottom, left
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12)) +
    
    # Faceting
    facet_grid(~Facet)
  
}

### Create arranged GIF of heatmap and scatterplot
gif_grid_electricity <- function(data, ind, start, end, d = 0.5, filename = "animation", w = 1440, h = 720) {
  
  # Accumulator variable for the GIF frame
  frame <- start
  
  for (i in start:end) {
    
    # Set PNG file specifications
    png(paste('./figs/', filename, '_', frame, '.png', sep = ''), width = w, height = h)
    
    # Create plots
    heatmap <- facet_heatmap(data, ind, frame)
    scatterplot <- facet_scatterplot(data, ind, start, frame, end)
    
    # Print arranged plots
    grid.arrange(heatmap, scatterplot, nrow = 2, heights = c(7/10, 3/10))
    
    # Close PNG file
    dev.off()
    
    # Move on to next frame in GIF
    frame <- frame + 1
  }
  
  # Set GIF filename
  gif_filename <- paste('./figs/', filename, '.gif', sep = '')
  
  # Create list of PNG filenames
  filenames <- as.character(seq(start, end))
  filenames <- paste('./figs/', filename, '_', filenames, '.png', sep= '')
  
  # Create GIF from PNG images
  gifski(filenames, delay = d, width = w, height = h,
         gif_file = gif_filename)
  
  # Delete PNG images
  file.remove(filenames)
  
  # Open GIF with default local application
  utils::browseURL(gif_filename)
  
}

### Create descending bar chart of countries according to reported performance
country_barchart <- function(data, ind, yr, continent) {
  
  # Filter data accoring to input arguments and select relevant columns
  filtered_data <- filter(data, year == yr, indicator == ind, region == continent) %>%
    select(c(indicator, seriesdescription, country, year, value, units, location, region))
  
  # Select only ALLAREA so as to not add up RURAL and URBAN for indicators 6.1.1 and 7.1.1.
  if (ind %in% c("6.1.1", "7.1.1")) {
    filtered_data <- filtered_data %>% filter(location == "ALLAREA")
  }
  
  # Get bar chart title and unit
  title <- filtered_data$seriesdescription %>% na.omit() %>% unique()
  unit <- filtered_data$units %>% na.omit() %>% unique()
  
  # Plot graph
  filtered_data %>%
    ggplot(aes(x = reorder(country, -value), y = value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste(title),
         subtitle = paste(continent, '- Year', yr),
         x = "Country",
         y = paste(unit))
  
}

### Create long form time series table of reported progress
long_form_table <- function(data, ind, continent) {
  
  # Filter data accoring to input arguments and select relevant columns
  filtered_data <- data %>%
    select(indicator, seriesdescription, country, year, value, units, location, region) %>%
    filter(indicator == ind, region == continent)
  
  # Select only ALLAREA so as to not add up RURAL and URBAN for indicators 6.1.1 and 7.1.1.
  if (ind %in% c("6.1.1", "7.1.1")) {
    filtered_data <- filtered_data %>% filter(location == "ALLAREA")
  }
  
  # Cast data frame to wide form
  dcast(filtered_data, country ~ year)
  
}

#############################
# Example visualizations
#############################

### World heatmap plots
# Annual RGDP growth rate per capita, year 2000
plot_world_heatmap(dat, "8.1.1", 2000)

# Renewable energy share of total energy consumption, year 2015
plot_world_heatmap(dat, "7.2.1", 2010)

# CO2 emissions per unit of value added, year 2010
plot_world_heatmap(dat, "9.4.1", 2015)


### Create and save world heatmap GIFs
# Annual RGDP growth rate per capita, year 2000-2015
gif_world_heatmap(dat, "8.1.1", 2000, 2015, "gif_annual_rgdp_growth")

# Renewable energy share of total energy consumption, year 2000-2015
gif_world_heatmap(dat, "7.2.1", 2000, 2015, "gif_renewable_energy_share")

# CO2 emissions per unit of value added, year 2000-2015
gif_world_heatmap(dat, "9.4.1", 2000, 2015, "gif_co2_per_unit_of_value")

# Internet users per 100 inhabitants, year 2000-2015
gif_world_heatmap(dat, "17.8.1", 2000, 2015, "gif_internet_users")


### Arranged GIF: heatmap and scatterplot of electricity access, urban vs. rural
gif_grid_electricity(dat, "7.1.1", 2000, 2016, 0.5, "electricity_access_urban_vs_rural")


### Create country bar chart of annual RGDP growth for Africa, year 2000
country_barchart(dat, "8.1.1", 2000, "Africa")


### Print time series table of pop.prop. using internet in the Americas
long_form_table(dat, "17.8.1", "Americas")


##############################
# Simple forecasting example
##############################

### Create linear model for Afghanistan, indicator 7.1.1
lm_711_afg <- dat %>%
  filter(country == "Afghanistan", indicator == "7.1.1", location == "ALLAREA") %>%
  select(country, indicator, year, value)

fit <- lm(value ~ year, data = lm_711_afg)

summary(fit)$coeff

### Plot of the above model
ggplot(lm_711_afg, aes(x = year, y = value)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")