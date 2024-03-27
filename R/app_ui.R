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
