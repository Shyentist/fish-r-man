library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(bigrquery)
library(tidyverse)
library(glue)
library(countrycode)
library(stringi)
library(sf)
library(maps)
library(ggplot2)
library(viridis)
library(shinyBS)

options(shiny.maxRequestSize = 20*1024*1024^2) #this should take care of the majority of the gpkg sizes

project <- "global-fishing-watch"
dataset <- "gfw_public_data"
billing <- "fish-r-man" # your billing account name

bq_auth(email = "fishrman-user@fish-r-man.iam.gserviceaccount.com", #comment these out
        path = "www/appDir/fish-r-man-ec45cfe426c8.json") #to access your own account

BQ_connection <-  dbConnect(bigquery(), 
                            project = project,
                            dataset = dataset, 
                            billing = billing, 
                            use_legacy_sql = FALSE) # specify we are using Standard SQL

tables_list <- dbListTables(BQ_connection)

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

tables_list_ui <- c(
  "Fishing effort at 10th degree", 
  "Fishing effort at 100th degree"
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
  "Vessel hours",
  "Fishing hours"
)

tables_columns_list_ui <- list(
  column_list_fe10_ui,
  column_list_fe100_ui
)

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

names(geartype_elements) <- geartype_names #to have a cleaner UI, will change with newer version of the tables

column_100th <- c( #the col names are here so I can check against them for validity of uploaded files
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

month_year_vector <- c("month","year") #to append to colnames, to have a cleaner UI and two more options for summaries

available_summaries_10th <- append(column_10th,month_year_vector, after = 1)

available_summaries_100th <- append(column_100th,month_year_vector, after = 1)

sf_column_100th <- column_100th[! column_100th %in% c("cell_ll_lat", "cell_ll_lon")] %>% #to later check the validity of spatial data uploaded (they must have same colnames as these vectors)
                      append("geom")
  
sf_column_10th <- column_10th[! column_10th %in% c("cell_ll_lat", "cell_ll_lon")] %>%
  append("geom")