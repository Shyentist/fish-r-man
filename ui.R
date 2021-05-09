ui <- fluidPage(
  theme = "style.css",
  useShinyjs(),
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
                choices = tables_list_ui[1:2]
              ),
              prettyCheckboxGroup(
                inputId = "filter_columns_ui",
                label = "Filter by",
                choices = NULL,
                shape = "curve",
                animation = "pulse"
              )
            ),
            tags$div(
              class = "sidebar",
              tags$div(
                class = "doubleNumber",
                disabled(
                  dateRangeInput(
                    inputId = "date",
                    label = "Date range:",
                    start = "2012-01-01",
                    end = "2012-01-02",
                    min = "2012-01-01",
                    max = "2021-01-01",
                    autoclose = FALSE
                  )
                ),
                disabled(
                  numericRangeInput(
                    inputId = "cell_ll_lat",
                    label = "Latitude range:",
                    value = c(-90, 90)
                  )
                ),
                disabled(
                  numericRangeInput(
                    inputId = "cell_ll_lon",
                    label = "Longitude range:",
                    value = c(-180, 180)
                  )
                ),
                disabled(
                  numericRangeInput(
                    inputId = "hours",
                    label = "Vessel hours range:",
                    value = c(0, 100000)
                  )
                ),
                disabled(
                  numericRangeInput(
                    inputId = "fishing_hours",
                    label = "Fishing hours range:",
                    value = c(0, 100000)
                  )
                ),
                disabled(
                  numericRangeInput(
                    inputId = "mmsi_present",
                    label = "MMSI present range:",
                    value = c(0, 10000)
                  )
                ),
                bsTooltip(
                  id = list(
                    "mmsi_present",
                    "fishing_hours",
                    "hours",
                    "cell_ll_lon",
                    "cell_ll_lat",
                    "date"
                  ),
                  title = "Minimum (left) is included.<br>Maximum (right) is not.",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")
                )
              )
            ),
            tags$div(
              class = "sidebar",
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
              ),
              disabled(
                textInput(
                  inputId = "mmsi",
                  label = "MMSI"
                ),
                bsTooltip(
                  id = "mmsi",
                  title = "Type 123 to search for MMSI number 123, exactly.<br>Type %123 to search for MMSIs ending with 123.<br>Type 123% to search for MMSIs starting with 123.<br>Type %123% to search for MMSIs containing 123.", placement = "right", trigger = "hover",
                  options = list(container = "body")
                )
              )
            ),
            tags$div(
              class = "sidebar",
              actionButton(
                inputId = "filter_button",
                label = "Filter"
              ),
              disabled(
                downloadButton(
                  outputId = "download_button",
                  label = "Download"
                )
              )
            ),
            tags$div(
              class = "sidebar",
              "SQL Query:",
              tags$code(textOutput(
                outputId = "sql_query"
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
      )
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
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "CSV",
                  tipify(
                    fileInput("uploaded_csv", "Choose CSV File",
                      accept = ".csv"
                    ),
                    title = "Only upload CSV files downloaded from fishRman. Maximum 150 Mb.",
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
                  ),
                ), tabPanel(
                  "GPKG",
                  tipify(
                    fileInput("uploaded_gpkg", "Choose GPKG File",
                      multiple = FALSE,
                      accept = ".gpkg"
                    ),
                    title = "Only upload GPKG files downloaded from fishRman (EPSG: 4326). Maximum 150 Mb.",
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
                  ),
                  "Or use the current data",
                  disabled(actionButton(
                    inputId = "convert_to_spatial_button",
                    label = "Convert"
                  )),
                  disabled(
                    tipify(
                      downloadButton(
                        outputId = "download_gpkg_button",
                        label = "Download .gpkg"
                      ),
                      title = "Download may take a couple of minutes for larger dataframes, please wait.",
                      placement = "right",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  )
                )
              ),
              tags$div(
                class = "sidebar",
                id = "area_of_interest",
                "Area of interest",
                disabled(
                  tipify(
                    fileInput("second_uploaded_gpkg", "Choose GPKG File",
                      multiple = FALSE,
                      accept = ".gpkg"
                    ),
                    title = "File must have same CRS as GFW data (EPSG: 4326). Maximum 150 Mb.",
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
                  )
                ),
                disabled(
                  selectInput(
                    inputId = "second_gpkg_layer",
                    label = "Layer",
                    choices = NULL,
                    multiple = FALSE
                  )
                ),
                disabled(
                  tipify(
                    prettyCheckbox(
                      inputId = "clip",
                      label = "Use only data contained in the area?",
                      value = FALSE
                    ),
                    title = "When ticked, analyses are performed only in the area of interest",
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
                  )
                )
              )
            ),
            tags$div(
              class = "sidebar",
              "Available analyses",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Descriptive",
                  prettyCheckboxGroup(
                    inputId = "summaries",
                    label = "Summarize by (max 7)",
                    choices = NULL,
                    shape = "curve",
                    animation = "pulse"
                  ),
                  disabled(
                    actionButton(
                      inputId = "summarize_button",
                      label = "Summarize"
                    )
                  ),
                  disabled(
                    downloadButton(
                      outputId = "download_analyses_button",
                      label = "Download"
                    )
                  )
                ),
                tabPanel(
                  "Spatial",
                  prettyCheckboxGroup(
                    inputId = "spatial_analyses",
                    label = "Visualization options",
                    choices = NULL,
                    shape = "curve",
                    animation = "pulse"
                  ),
                  disabled(
                    actionButton(
                      inputId = "visualize_button",
                      label = "Visualize"
                    )
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
            id = "preview-table",
            tableOutput("uploaded_csv_viz")
          ),
          tags$div(
            class = "queried_table",
            id = "summary-table",
            dataTableOutput("summary_preview")
          ),
          hidden(tags$div(
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
                tipify(
                  sliderInput(
                    inputId = "cumul_distr_percent",
                    label = "% of cumulative distribution",
                    min = 1,
                    max = 100,
                    value = 100
                  ),
                  title = "Show the top X% of the distribution",
                  placement = "left",
                  trigger = "hover",
                  options = list(container = "body")
                ),
                numericRangeInput(
                  inputId = "xrange",
                  label = "Longitude range:",
                  value = c(-180, 180)
                ),
                numericRangeInput(
                  inputId = "yrange",
                  label = "Latitude range:",
                  value = c(-90, 90)
                ),
                tipify(
                  numericInput(
                    inputId = "map_rez",
                    label = "Map resolution",
                    value = NULL
                  ),
                  title = "Length of the side of each grid cell, in degrees of latitude and longitude",
                  placement = "left",
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
        )
      )
    )
  ),
  fluidRow(
    column(
      12,
      tags$div(
        class = "footer",
        column(
          2,
          tags$a(
            target = "_blank",
            rel = "noreferrer noopener",
            img(
              src = "img/fishrman_banner.png",
              height = "auto",
              width = "100%"
            ),
            href = "https://github.com/Shyentist/fish-r-man" # this is temporary, it will eventually link to fishRman website or similar hub
          )
        ),
        column(
          5,
          tags$p("References"),
          "Software by 'Buonomo Pasquale. [2021].",
          tags$a("https://github.com/Shyentist/fish-r-man'",
            target = "_blank",
            rel = "noreferrer noopener",
            href = "https://github.com/Shyentist/fish-r-man",
            style = "color:#000000"
          ),
          tags$br(),
          "Data by 'Global Fishing Watch. [2021].",
          tags$a("https://globalfishingwatch.org/'",
            target = "_blank",
            rel = "noreferrer noopener",
            href = "https://globalfishingwatch.org/",
            style = "color:#000000"
          )
        ),
        column(
          3,
          tags$p("Contacts"),
          "E-mail:",
          tags$a("pasqualebuonomo@hotmail.it",
            target = "_blank",
            rel = "noreferrer noopener",
            href = "mailto:pasqualebuonomo@hotmail.it",
            style = "color:#000000"
          )
        ),
        column(
          2,
          tags$a(
            target = "_blank",
            rel = "noreferrer noopener",
            img(
              src = "img/github_logo.png",
              height = "auto",
              width = "100%"
            ),
            href = "https://github.com/Shyentist/fish-r-man"
          )
        )
      )
    )
  )
)