library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(bigrquery)
library(tidyverse)

project <- "global-fishing-watch"
dataset <- "global_footprint_of_fisheries"
billing <- "fish-r-man" # my billing account name, but I would like the users to 
#be able to insert their own

BQ_connection <-  dbConnect(bigquery(), 
                            project = project,
                            dataset = dataset, 
                            billing = billing, 
                            use_legacy_sql = FALSE)

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
#next is a simple ui, server and session set-up, with the fields for the query
ui <- fluidPage(
  
  useShinyjs(),
  
  selectInput(
    inputId = "table_name_ui",
    label = "Query table",
    choices = tables_list_ui[1:2]
    
  ),
  
  checkboxGroupInput(
    inputId = "filter_columns_ui",
    label = "Filter by",
    choices = NULL
  ),
  
  disabled(
    dateRangeInput(
      inputId = "date", 
      label = "Date range",
      start = "2012-01-01",
      end = "2012-01-02",
      min = "2012-01-01",
      max = "2017-01-01",
      separator = " to ",
      autoclose = FALSE
    )),
  
  disabled(
    numericRangeInput(
      inputId = "lat_bin",
      label = "Latitude range:",
      value = c(-90, 90)
    )),
  
  disabled(
    numericRangeInput(
      inputId = "lon_bin",
      label = "Longitude range:",
      value = c(-180, 180)
    )),
  
  disabled(
    numericRangeInput(
      inputId = "vessel_hours",
      label = "Vessel hours range:",
      value = c(0, 2000)
    )),
  
  disabled(
    numericRangeInput(
      inputId = "fishing_hours",
      label = "Fishing hours range:",
      value = c(0, 200)
    )),
  
  disabled(
    numericRangeInput(
      inputId = "mmsi_present",
      label = "MMSI present range:",
      value = c(0, 600)
    )),
  
  disabled(
    numericRangeInput(
      inputId = "mmsi",
      label = "MMSI range:",
      value = c(0, 1111111111)
    )),
  
  disabled(
    selectInput(
      inputId = "flag",
      label = "Flag",
      choices = NULL,
      multiple = TRUE
    )),
  
  disabled(
    selectInput(
      inputId = "geartype",
      label = "Geartype",
      choices = NULL,
      multiple = TRUE
    )),
  
  actionButton(
    inputId = "filter_button",
    label = "Filter"
  ),
  
  dataTableOutput(
    outputId = "queried_table"
  )
  
)

server <- function(input,output,session) {
  
  observeEvent(input$table_name_ui,{
    
    table_name_ui <- input$table_name_ui
    
    fields_list <- dbListFields(
      BQ_connection, 
      tables_list[match(
        table_name_ui,
        tables_list_ui
      )])
    
    fields_list_names <- tables_columns_list_ui[[match(
      table_name_ui, 
      tables_list_ui
    )]]
    
    names(fields_list) <- fields_list_names
    
    updateCheckboxGroupInput(
      session,
      inputId = 'filter_columns_ui',
      choices = fields_list
    )}) 
  
  observeEvent(input$filter_columns_ui,{
    
    for (field in list_togglable_ids) {
      
      if (field %in% input$filter_columns_ui) {
        
        enable(id = field)
        
      } else {
        
        disable (id = field)
        
      }
    }
  },ignoreNULL = FALSE)
  
}
  shinyApp(ui = ui, server = server)
  
#I am working on a script for interactive SQL query building, it will be ready shortly