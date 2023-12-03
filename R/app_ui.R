#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stats na.omit
#' @import countrycode
#' @importFrom shinyjs hidden disabled useShinyjs
#' @importFrom shinyBS tipify bsTooltip
#' @importFrom shinyWidgets prettyCheckbox prettyCheckboxGroup numericRangeInput
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = "style.css",
      shinyjs::useShinyjs(),
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel(
          "Query",
          fluidRow(
            column(
              3,
              tags$div(
                class = "sidenav",
                tags$div(
                  class = "sidebar",
                  selectInput(
                    inputId = "table_name_ui",
                    label = "Query table",
                    choices = c(
                      "AIS data at 10th degree" = "fishing_effort_byvessel_v2",
                      "AIS data at 100th degree" = "fishing_effort_v2")
                  ),
                  shinyWidgets::prettyCheckboxGroup(
                    inputId = "filter_columns_ui",
                    label = "Filter by",
                    choices = NULL
                  )
                ),
                tags$div(
                  class = "sidebar",
                  tags$div(
                    class = "doubleNumber",
                    shinyjs::disabled(
                      dateRangeInput(
                        inputId = "dated",
                        label = "Date range:",
                        start = "2012-01-01",
                        end = "2012-01-02",
                        min = "2012-01-01",
                        max = "2021-01-01",
                        autoclose = FALSE
                      )
                    ),
                    shinyjs::disabled(
                      shinyWidgets::numericRangeInput(
                        inputId = "lat",
                        label = "Latitude range:",
                        value = c(-90, 90)
                      )
                    ),
                    shinyjs::disabled(
                      shinyWidgets::numericRangeInput(
                        inputId = "lon",
                        label = "Longitude range:",
                        value = c(-180, 180)
                      )
                    ),
                    shinyjs::disabled(
                      shinyWidgets::numericRangeInput(
                        inputId = "hours",
                        label = "Vessel hours range:",
                        value = c(0, 100000)
                      )
                    ),
                    shinyjs::disabled(
                      shinyWidgets::numericRangeInput(
                        inputId = "fishing_hours",
                        label = "Fishing hours range:",
                        value = c(0, 100000)
                      )
                    ),
                    shinyjs::disabled(
                      shinyWidgets::numericRangeInput(
                        inputId = "mmsi_present",
                        label = "MMSI present range:",
                        value = c(0, 10000)
                      )
                    ),
                    shinyBS::bsTooltip(
                      id = list(
                        "mmsi_present",
                        "fishing_hours",
                        "hours",
                        "cell_ll_lon",
                        "cell_ll_lat",
                        "date"
                      ),
                      title = "Minimum is included.<br>Maximum is not.<br>Minimum must be less than maximum.",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  )
                ),
                tags$div(
                  class = "sidebar",
                  shinyjs::disabled(
                    selectInput(
                      inputId = "flag",
                      label = tags$p("Flag", tags$a("(List of ISO Alpha-3 Country Codes)",
                                                    target = "_blank",
                                                    rel = "noreferrer noopener",
                                                    href = "https://www.nationsonline.org/oneworld/country_code_list.htm",
                                                    style = "color:gray; text-decoration: underline; font-size: 0.7vw;"
                      )),
                      choices = sort(na.omit(countrycode::codelist$iso3c)),
                      multiple = TRUE
                    )
                  ),
                  shinyjs::disabled(
                    selectInput(
                      inputId = "geartype",
                      label = "Geartype",
                      choices = c(
                        "Dredge fishing" = "dredge_fishing",
                        "Drifting longlines" = "drifting_longlines",
                        "Fixed gear" = "fixed_gear",
                        "Pole and line" = "pole_and_line",
                        "Pots and traps" = "pots_and_traps",
                        "Purse seines" = "purse_seines",
                        "Seiners" = "seiners",
                        "Set gillnets" = "set_gillnets",
                        "Set longlines" = "set_longlines",
                        "Squid jigger" = "squid_jigger",
                        "Trawlers" = "trawlers",
                        "Trollers" = "trollers",
                        "Tuna purse seines" = "tuna_purse_seines",
                        "Other purse seines" = "other_purse_seines",
                        "Other seines" = "other_seines",
                        "Fishing" = "fishing"
                      ),
                      multiple = TRUE
                    )
                  ),
                  shinyjs::disabled(
                    textInput(
                      inputId = "mmsi",
                      label = "MMSI"
                    ),
                    shinyBS::bsTooltip(
                      id = "mmsi",
                      title = "Type 123 to search for MMSI number 123, exactly.<br>Type %123 to search for MMSIs ending with 123.<br>Type 123% to search for MMSIs starting with 123.<br>Type %123% to search for MMSIs containing 123.",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  )
                ),
                tags$div(
                  class = "sidebar",
                  "SQL Query:",
                  tags$code(textOutput(
                    outputId = "sql_query"
                  ))
                ),
                tags$div(
                  class = "sidebar hide-overflow",

                  textInput(
                    inputId = "billing",
                    label = "Billing project",
                    placeholder="fish-r-man"
                  ),
                  tags$div(
                    class = "button-container",
                    actionButton(
                      inputId = "filter_button",
                      label = "Filter"
                    ),
                    shinyjs::disabled(
                      downloadButton(
                        outputId = "download_button_csv",
                        label = ".csv"
                      )
                    ),
                    shinyjs::disabled(
                      downloadButton(
                        outputId = "download_button_gpkg",
                        label = ".gpkg"
                      )
                    ))
                )
              )
            ),
            column(
              9,
              tags$div(
                class = "queried_table",
                dataTableOutput(
                  outputId = "queried_table"
                )
              )
            )
          ),
          footer()
        ),
        tabPanel(
          "Analysis",
          fluidRow(
            column(
              3,
              tags$div(
                class = "sidenav",
                tags$div(
                  class = "sidebar",
                  "Data to analyse",
                  shinyBS::tipify(
                    fileInput("uploaded_csv", "Choose CSV File",
                              accept = ".csv"
                    ),
                    title = "Only upload CSV files downloaded from fishRman. Maximum 150 Mb.",
                    placement = "bottom",
                    trigger = "hover",
                    options = list(container = "body")
                  )
                ),
                tags$div(
                  class = "sidebar hide-overflow",
                  "Area of interest",
                  shinyjs::disabled(
                    shinyBS::tipify(
                      fileInput("area_of_interest", "Choose GPKG File",
                                multiple = FALSE,
                                accept = ".gpkg"
                      ),
                      title = "File must have same CRS as GFW data (EPSG: 4326). Maximum 150 Mb.",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  ),
                  shinyjs::disabled(
                    selectInput(
                      inputId = "area_of_interest_layer",
                      label = "Layer",
                      choices = NULL,
                      multiple = FALSE
                    )
                  ),
                  shinyjs::disabled(
                    shinyBS::tipify(
                      shinyWidgets::prettyCheckbox(
                        inputId = "clip",
                        label = "Use only data contained in the area?",
                        value = FALSE
                      ),
                      title = "When ticked, analyses are performed only in the area of interest",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  ),
                  tags$div(
                    class = "button-container",
                    shinyjs::disabled(
                      downloadButton(
                        outputId = "download_button_csv_clipped",
                        label = ".csv"
                      )
                    ),
                    shinyjs::disabled(
                      downloadButton(
                        outputId = "download_button_gpkg_clipped",
                        label = ".gpkg"
                      )
                    ))
                )
              ),
              tags$div(
                class = "sidebar",
                "Available analyses",
                tabsetPanel(
                  id = "analyses_types_tabs",
                  type = "tabs",
                  tabPanel(
                    "Descriptive",
                    shinyWidgets::prettyCheckboxGroup(
                      inputId = "summaries",
                      label = "Summarize by (max 7)",
                      choices = NULL
                    ),
                    shinyjs::disabled(
                      actionButton(
                        inputId = "summarize_button",
                        label = "Summarize"
                      )
                    ),
                    shinyjs::disabled(
                      downloadButton(
                        outputId = "download_analyses_button",
                        label = "Download"
                      )
                    )
                  ),
                  tabPanel(
                    "Spatial",
                    shinyWidgets::prettyCheckboxGroup(
                      inputId = "spatial_analyses",
                      label = "Visualization options",
                      choices = NULL
                    ),
                    shinyjs::disabled(
                      actionButton(
                        inputId = "visualize_button",
                        label = "Visualize"
                      )
                    )
                  )
                )
              )
            ),
            column(
              9,
              tags$div(
                class = "queried_table",
                id = "summary-table",
                dataTableOutput("summary_preview")
              ),
              shinyjs::hidden(tags$div(
                class = "sidebar",
                id = "map",
                fluidRow(
                  column(
                    9,
                    plotOutput("viz_map")
                  ),
                  column(
                    3,
                    shinyWidgets::prettyCheckboxGroup(
                      inputId = "add_layer",
                      label = "Also show global",
                      choices = c(
                        "EEZ" = "eez",
                        "24 Nautical Miles Zones" = "eez_24nm",
                        "12 Nautical Miles Zones" = "eez_12nm")
                    ),
                    selectInput(
                      inputId = "mapped_column",
                      label = "Map by",
                      choices = NULL
                    ),
                    shinyBS::tipify(
                      sliderInput(
                        inputId = "cumul_distr_percent",
                        label = "Top % of dataframe",
                        min = 1,
                        max = 100,
                        value = 100
                      ),
                      title = "Show the top X% of the dataframe",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(container = "body")
                    ),
                    shinyWidgets::numericRangeInput(
                      inputId = "yrange",
                      label = "Latitude range:",
                      value = c(-90, 90)
                    ),
                    shinyWidgets::numericRangeInput(
                      inputId = "xrange",
                      label = "Longitude range:",
                      value = c(-180, 180)
                    ),
                    shinyBS::tipify(
                      numericInput(
                        inputId = "map_rez",
                        label = "Map resolution",
                        value = NULL
                      ),
                      title = "Length of the side of each grid cell, in degrees of latitude and longitude. Maximum 2.",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(container = "body")
                    ),
                    actionButton(
                      inputId = "re_visualize_button",
                      label = "Re-Visualize"
                    ),
                    downloadButton(
                      outputId = "download_map_button",
                      label = "Download"
                    )
                  )
                )
              ))
            )),
          footer()
        ),
        tabPanel(
          "Handbook",
          fluidRow(
            column(
              12,
              tags$div(
                class = "queried-table",
                id = "documentation",
                uiOutput("pdfview")
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fishRman"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
