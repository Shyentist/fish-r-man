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
                choices = tables_list_names[1:2]
              ),
              prettyCheckboxGroup(
                inputId = "filter_columns_ui",
                label = "Filter by",
                choices = NULL
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
                  title = "Minimum is included.<br>Maximum is not.<br>Minimum must be less than maximum.",
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
                  label = tags$p("Flag", tags$a("(List of ISO Alpha-3 Country Codes)",
                    target = "_blank",
                    rel = "noreferrer noopener",
                    href = "https://www.nationsonline.org/oneworld/country_code_list.htm",
                    style = "color:gray; text-decoration: underline; font-size: 0.7vw;"
                  )),
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
              
              textInput(
                inputId = "billing",
                label = "Billing project",
                placeholder="fish-r-man"
              ),
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
              tabsetPanel(
                id = "filetype_tabs",
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
                  )
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
                id = "analyses_types_tabs",
                type = "tabs",
                tabPanel(
                  "Descriptive",
                  prettyCheckboxGroup(
                    inputId = "summaries",
                    label = "Summarize by (max 7)",
                    choices = NULL
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
                    choices = NULL
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
                prettyCheckboxGroup(
                  inputId = "add_layer",
                  label = "Also show global",
                  choices = add_layer_choices_backend
                ),
                selectInput(
                  inputId = "mapped_column",
                  label = "Map by",
                  choices = NULL
                ),
                tipify(
                  sliderInput(
                    inputId = "cumul_distr_percent",
                    label = "Top % of dataframe",
                    min = 1,
                    max = 100,
                    value = 100
                  ),
                  title = "Show the top X% of the dataframe",
                  placement = "left",
                  trigger = "hover",
                  options = list(container = "body")
                ),
                numericRangeInput(
                  inputId = "yrange",
                  label = "Latitude range:",
                  value = c(-90, 90)
                ),
                numericRangeInput(
                  inputId = "xrange",
                  label = "Longitude range:",
                  value = c(-180, 180)
                ),
                tipify(
                  numericInput(
                    inputId = "map_rez",
                    label = "Map resolution",
                    value = NULL
                  ),
                  title = "Length of the side of each grid cell, in degrees of latitude and longitude. Maximum 2.",
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
  ),
  fluidRow(
    column(
      12,
      tags$div(
        class = "footer",
        column(
          3,
          tags$p("Software"),
          tags$a(
            target = "_blank",
            rel = "noreferrer noopener",
            img(
              src = "img/fishrman_banner.png",
              height = "auto",
              width = "100%"
            ),
            href = "https://github.com/Shyentist/fish-r-man"
          )
        ),
        column(
          6,
          tags$p("References"),
          "Software: Buonomo P. [2021]. fishRman: A Shiny R Dashboard improving Global Fishing Watch data availability. Journal of Open Source Software.",
          tags$a("https://doi.org/10.21105/joss.03467",
            target = "_blank",
            rel = "noreferrer noopener",
            href = "https://doi.org/10.21105/joss.03467",
            style = "color:#000000"
          ),
          tags$br(),
          tags$br(),
          "Data: Global Fishing Watch. [2021].",
          tags$a("https://globalfishingwatch.org/",
            target = "_blank",
            rel = "noreferrer noopener",
            href = "https://globalfishingwatch.org/",
            style = "color:#000000"
          ),
          tags$br(),
          tags$br(),
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
          3,
          tags$p("Sponsor"),
          tags$a(
            target = "_blank",
            rel = "noreferrer noopener",
            img(
              src = "img/OSMOS_logo.png",
              height = "auto",
              width = "100%"
            ),
            href = "https://osmos.xyz/"
          )
        )
      )
    )
  )
)