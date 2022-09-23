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
