library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(bigrquery)
library(tidyverse)
library(glue)
library(countrycode)
library(stringi)

options(shiny.maxRequestSize = 20*1024*1024^2)

project <- "global-fishing-watch"
dataset <- "global_footprint_of_fisheries"
billing <- "fish-r-man" # your billing account name

BQ_connection <-  dbConnect(bigquery(), 
                            project = project,
                            dataset = dataset, 
                            billing = billing, 
                            use_legacy_sql = FALSE) # specify we are using Standard SQL

tables_list <- dbListTables(BQ_connection)

list_togglable_ids <- list(
  "date",
  "lat_bin",
  "lon_bin",
  "vessel_hours",
  "fishing_hours",
  "mmsi_present",
  "mmsi",
  "flag",
  "geartype"
)

tables_list_ui <- c(
  "Fishing effort at 100th degree", 
  "Fishing effort at 10th degree"
)

column_list_fe100_ui <- c(
  "Date", 
  "Latitude", 
  "Longitude",
  "Flag",
  "Geartype",
  "Vessel hours",
  "Fishing hours",
  "MMSI present"
)

column_list_fe10_ui <- c(
  "Date", 
  "Latitude", 
  "Longitude",
  "MMSI",
  "Fishing hours"
)

tables_columns_list_ui <- list(
  column_list_fe100_ui,
  column_list_fe10_ui
)

geartype_elements <- c(
  "drifting_longlines",
  "fixed_gear",
  "purse_seines",
  "squid_jigger",
  "trawlers",
  "other_fishing"
)

geartype_names <- c(
  "Drifting longlines",
  "Fixed gear",
  "Purse seines",
  "Squid jigger",
  "Trawlers",
  "Other fishing gear"
)

names(geartype_elements) <- geartype_names

column_100th <- c(
  "date",
  "lat_bin",
  "lon_bin",
  "flag",
  "geartype",
  "vessel_hours",
  "fishing_hours",
  "mmsi_present"
  )

column_10th <- c(
  "date",
  "lat_bin",
  "lon_bin",
  "mmsi",
  "fishing_hours"
  )

available_analyses_10th <- c("Descriptive statistics 10th degree")

available_analyses_100th <- c("Descriptive statistics 100th degree")