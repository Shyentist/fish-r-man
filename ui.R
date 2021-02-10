ui <- fluidPage(
  
  theme = "style.css",
  
  useShinyjs(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Query",
  
  fluidRow(
    
    column(3,
           tags$div(class = "sidenav",
                    tags$div(class = "sidebar",
                             
                             selectInput(
                               inputId = "table_name_ui",
                               label = "Query table",
                               choices = tables_list_ui[1:2]
                             ),
                             
                             prettyCheckboxGroup(
                               inputId = "filter_columns_ui",
                               label = "Filter by",
                               choices = NULL,
                               shape = "curve",
                               animation = "pulse"
                             )),
                    
                    tags$div(class = "sidebar",
                             tags$div(class="doubleNumber",
                                      disabled(
                                        dateRangeInput(
                                          inputId = "date",
                                          label = "Date range:",
                                          start = "2012-01-01",
                                          end = "2012-01-02",
                                          min = "2012-01-01",
                                          max = "2017-01-01",
                                          autoclose = FALSE
                                        )
                                      ),
                                      
                                      disabled(
                                        numericRangeInput(
                                          inputId = "lat_bin",
                                          label = "Latitude range:",
                                          value = c(-90, 90)
                                        )
                                      ),
                                      
                                      disabled(
                                        numericRangeInput(
                                          inputId = "lon_bin",
                                          label = "Longitude range:",
                                          value = c(-180, 180)
                                        )
                                      ),
                                      
                                      disabled(
                                        numericRangeInput(
                                          inputId = "vessel_hours",
                                          label = "Vessel hours range:",
                                          value = c(0, 2000)
                                        )
                                      ),
                                      
                                      disabled(
                                        numericRangeInput(
                                          inputId = "fishing_hours",
                                          label = "Fishing hours range:",
                                          value = c(0, 200)
                                        )
                                      ),
                                      
                                      disabled(
                                        numericRangeInput(
                                          inputId = "mmsi_present",
                                          label = "MMSI present range:",
                                          value = c(0, 600)
                                        )
                                      ),
                                      
                                      disabled(
                                        numericRangeInput(
                                          inputId = "mmsi",
                                          label = "MMSI range:",
                                          value = c(0, 1111111111)
                                        )
                                      )
                             )
                    ),
                    
                    tags$div(class = "sidebar",
                             disabled(
                               selectInput(
                                 inputId = "flag",
                                 label = "Flag",
                                 choices = na.omit(codelist$iso3c),
                                 multiple = TRUE
                               )
                             ),
                             
                             disabled(
                               selectInput(
                                 inputId = "geartype",
                                 label = "Geartype",
                                 choices = geartype_elements,
                                 multiple = TRUE
                               )
                             )
                    ),
                    
                    tags$div(class = "sidebar",
                             actionButton(
                               inputId = "filter_button",
                               label = "Filter"
                             )
                    ),
                    
                    tags$div(class = "sidebar",
                             disabled(
                               downloadButton(
                                 outputId = "download_button",
                                 label = "Download"
                               )
                             )
                    )
           )
    ),
    
    column(7,
           tags$div(class = "queried_table",
                    withSpinner(
                      dataTableOutput(
                      outputId = "queried_table"
                      ), 
                      type = 1
                      )
           )
    ),
    
    column(2,
           tags$div(class = "sidenav",
           tags$div(class = "sidebar",
                    img(
                      src = "img/fishrman_logo.png",
                      height = 'auto',
                      width = '100%'
                    ),
                    href="https://github.com/Shyentist/fish-r-man"
           ),
           
           tags$div(class = "sidebar",
                    tags$a
                    (img
                      (
                        src = "img/github_logo.png",
                        height = 'auto',
                        width = '100%'
                      ),
                      href="https://github.com/Shyentist/fish-r-man"
                    )
           )
    )
  )
)),

tabPanel("Analysis", 
         fluidRow(
           column(3,
                  tags$div(class = "sidenav",
                           tags$div(class = "sidebar",
                                    tabsetPanel(type = "tabs",
                                                tabPanel("CSV",
                                                         fileInput("uploaded_csv", "Choose CSV File",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv"))
                                    ),tabPanel("GPKG",
                                               fileInput("uploaded_gpkg", "Choose GPKG File",
                                                         multiple = FALSE,
                                                         accept = c("gpkg")),
                                               "Or use the current data",
                                               disabled(actionButton(
                                                 inputId = "convert_to_spatial_button",
                                                 label = "Convert"
                                               )),
                                               disabled(
                                                 downloadButton(
                                                   outputId = "download_gpkg_button",
                                                   label = "Download .gpkg"
                                                 )
                                               )
                                               
                                    )
                                    )),
                           
                           tags$div(class = "sidebar",
                                    "Available analyses",
                                    prettyCheckboxGroup(
                                      inputId = "summaries",
                                      label = "Summarize by",
                                      choices = NULL,
                                      shape = "curve",
                                      animation = "pulse"
                                      ),
                                    actionButton(
                                      inputId = "summarize_button",
                                      label = "Summarize"
                                    )
                                    ),
                           
                           tags$div(class = "sidebar",
                                    disabled(
                                      downloadButton(
                                        outputId = "download_analyses_button",
                                        label = "Download"
                                      )
                                    )
                           )
                           )
                  ),
           
           column(7,
                  tags$div(class = "queried_table", 
                           tableOutput("uploaded_csv_viz"))
           ),
           
           column(2,
                  tags$div(class = "sidenav",
                           tags$div(class = "sidebar",
                                    img(
                                      src = "img/fishrman_logo.png",
                                      height = 'auto',
                                      width = '100%'
                                    ),
                                    href="https://github.com/Shyentist/fish-r-man"
                           ),
                           
                           tags$div(class = "sidebar",
                                    tags$a
                                    (img
                                      (
                                        src = "img/github_logo.png",
                                        height = 'auto',
                                        width = '100%'
                                      ),
                                      href="https://github.com/Shyentist/fish-r-man"
                                    )
                           )
                  )
           )
         )
)

))