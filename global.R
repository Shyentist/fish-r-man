library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(bigrquery)
library(tidyverse)
library(countrycode)
library(stringi)
library(sf)
library(maps)
library(ggplot2)
library(viridis)
library(shinyBS)

source("fun/sql.construct.R")
source("fun/count.sql.R")
source("fun/length.until.R")
source("fun/intro.message.R")

options(scipen = 999)

options(shiny.maxRequestSize = 150 * 1024^2) # maximum upload size is 150 MB, edit if you need more than that

project <- "global-fishing-watch"
dataset <- "gfw_public_data"

# uncomment line below for "online distro"
bq_auth(email = "fishrman-user@fish-r-man.iam.gserviceaccount.com", path = "www/appDir/fish-r-man-ec45cfe426c8.json") 

# uncomment line below for "offline distro"
#bq_auth(email=FALSE, scopes = "https://www.googleapis.com/auth/bigquery") 


# these are the IDs for the filter checkboxes, so that later
# functions can iterate through it
list_togglable_ids <- list(
  "date",
  "cell_ll_lat",
  "cell_ll_lon",
  "hours",
  "fishing_hours",
  "mmsi_present",
  "mmsi",
  "flag",
  "geartype"
)

# front-end names of the tables
tables_list_ui <- c(
  "AIS data at 10th degree",
  "AIS data at 100th degree"
)

#back-end names of the tables
tables_list_names <- c(
  "fishing_effort_byvessel_v2", 
  "fishing_effort_v2")

names(tables_list_names) <- tables_list_ui

# front-end names of the columns for the 100th degree table
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

# front-end names of the columns for the 10th degree table
column_list_fe10_ui <- c(
  "Date",
  "Latitude",
  "Longitude",
  "MMSI",
  "Vessel hours",
  "Fishing hours"
)

# list of vectors with the column names for both tables to later
# iterate through/match index in order to have the right list
tables_columns_list_ui <- list(
  column_list_fe10_ui,
  column_list_fe100_ui
)

# back-end names of the geartypes
geartype_elements <- c(
  "dredge_fishing",
  "drifting_longlines",
  "fixed_gear",
  "pole_and_line",
  "pots_and_traps",
  "purse_seines",
  "seiners",
  "set_gillnets",
  "set_longlines",
  "squid_jigger",
  "trawlers",
  "trollers",
  "tuna_purse_seines",
  "other_purse_seines",
  "other_seines",
  "fishing"
)

# front-end names of the geartypes
geartype_names <- c(
  "Dredge fishing",
  "Drifting longlines",
  "Fixed gear",
  "Pole and line",
  "Pots and traps",
  "Purse seines",
  "Seiners",
  "Set gillnets",
  "Set longlines",
  "Squid jigger",
  "Trawlers",
  "Trollers",
  "Tuna purse seins",
  "Other purse seins",
  "Other seines",
  "Fishing"
)

# to have a cleaner UI, the geartype names are noew named, so a user selecting
# a front-end name, gets automatically processed as having selected the back-end one
names(geartype_elements) <- geartype_names

# the col names are here so I can check against them for validity of uploaded files
column_100th <- c(
  "date",
  "cell_ll_lat",
  "cell_ll_lon",
  "flag",
  "geartype",
  "hours",
  "fishing_hours",
  "mmsi_present"
)

column_10th <- c(
  "date",
  "cell_ll_lat",
  "cell_ll_lon",
  "mmsi",
  "hours",
  "fishing_hours"
)

month_year_vector <- c("month", "year") # to append to colnames, to have a cleaner UI and two more options for summaries

available_summaries_10th <- append(column_10th, month_year_vector, after = 1)

available_summaries_100th <- append(column_100th, month_year_vector, after = 1)

sf_column_100th <- column_100th[!column_100th %in% c("cell_ll_lat", "cell_ll_lon")] %>% # to later check the validity of spatial data uploaded (they must have same colnames as these vectors)
  append("geom")

sf_column_10th <- column_10th[!column_10th %in% c("cell_ll_lat", "cell_ll_lon")] %>%
  append("geom")

add_layer_choices_backend <- c("eez_boundaries_v11.gpkg", "eez_24nm_v3_boundaries.gpkg", "eez_12nm_v3_boundaries.gpkg")

add_layer_choices_ui <- c("EEZ", "24 Nautical Miles Zones", "12 Nautical Miles Zones")

names(add_layer_choices_backend) <- add_layer_choices_ui

