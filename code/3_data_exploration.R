######################################
# Data exploration
######################################

library(dplyr)        # Flexible tools for data manipulation
library(ggplot2)      # Powerful graphing package
library(gridExtra)    # Tools to work with grid graphics
library(gifski)       # Convert image frames to GIF animations

### Tidy data locations
tidy_file = "./rdas/tidy_dat.rda"
world_map_file = "./rdas/world_map.rda"

### Attach tidy data to namespace
if(!exists("dat")) load(tidy_file); rm(tidy_file)
if(!exists("map_world")) load(world_map_file); rm(world_map_file)

### Attach functions to namespace
source("./code/functions.R")
source("./code/facet_heatmap_scatterplot.R")


### Create and save world heatmap plots
# Annual RGDP growth rate per capita, year 2000
plot_world_heatmap(dat, "8.1.1", 2000)
save_last_plot("pic_annual_rgdp_growth")

# Renewable energy share of total energy consumption, year 2015
plot_world_heatmap(dat, "7.2.1", 2010)
save_last_plot("pic_renewable_evergy_share")

# CO2 emissions per unit of value added, year 2010
plot_world_heatmap(dat, "9.4.1", 2015)
save_last_plot("pic_co2_per_unit_of_value")


### Create and save world heatmap GIFs
# Annual RGDP growth rate per capita, year 2000-2015
gif_world_heatmap(dat, "8.1.1", 2000, 2015, "gif_annual_rgdp_growth")

# Renewable energy share of total energy consumption, year 2000-2015
gif_world_heatmap(dat, "7.2.1", 2000, 2015, "gif_renewable_energy_share")

# CO2 emissions per unit of value added, year 2000-2015
gif_world_heatmap(dat, "9.4.1", 2010, 2015, "gif_co2_per_unit_of_value")

# Internet users per 100 inhabitants, year 2000-2015
gif_world_heatmap(dat, "17.8.1", 2000, 2015, "gif_internet_users")


### Arranged GIF: heatmap and scatterplot of electricity access, urban vs. rural
gif_grid_electricity(dat, "7.1.1", 2000, 2016, 0.5, "electricity_access_urban_vs_rural")
