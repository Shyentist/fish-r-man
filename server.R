server <- function(input, output, session) {
  observeEvent(input$table_name_ui, {
    table_name_ui <- input$table_name_ui

    fields_list <- dbListFields(
      BQ_connection,
      tables_list[match(
        table_name_ui,
        tables_list_ui
      )]
    )

    fields_list_names <- tables_columns_list_ui[[match(
      table_name_ui,
      tables_list_ui
    )]]

    names(fields_list) <- fields_list_names

    updatePrettyCheckboxGroup( # this way, the checkboxes are always named after the table fields
      session,
      inputId = "filter_columns_ui",
      choices = fields_list
    )
  })

  # what follow enables and disables the input boxes from the "filter sidebar"
  # I do this so that I can later check on which boxes are ticked, instead of
  # checking directly for the inputs, and I can build the SQL Query based on
  # which inputs are enabled. The logic is simple, for inputs of type numeric range
  # the query adds AND, as in "WHERE cell_ll_lat > 10 AND cell_ll_lat < 20 ", since the
  # two things must be true at the same time. For categories, such as flag and
  # geartype, I add OR, as in " flag = 'ITA' OR flag = 'FRA' ", since we are
  # looking for entries that match any of these categories

  # for MMSI, the SQL created is a simple 'like', but one can use the '%'
  # character to search things starting/ending/containing the input

  # The query starts without a WHERE, only AND, then, I substitute the first AND
  # of the query with a WHERE. I am sure someone else can come up with something
  # more elegant, but I figured this was the best option at the time of coding

  observeEvent(input$filter_columns_ui,
    {
      for (field in list_togglable_ids) {
        if (field %in% input$filter_columns_ui) {
          enable(id = field)
        } else {
          disable(id = field)
        }
      }
    },
    ignoreNULL = FALSE
  )

  # what is happening down here, with which_event, you will see it often in this
  # code. It is basically a switch, that I can later check in reactive expressions
  # that must differentiate among the various type of inputs that they can receive
  # for instance, both when uploading a csv or querying data, I need the final
  # result to be assigned to 'my_data', so that later on I can just refer to this
  # value, which is qualitatively the same for both sources

  which_event <- reactiveValues(
    query = FALSE,
    csv = FALSE,
    gpkg = FALSE
  )

  observeEvent(input$filter_button, {
    which_event$query <- TRUE
    which_event$csv <- FALSE
    which_event$gpkg <- FALSE
  })

  observeEvent(input$uploaded_csv, {
    which_event$query <- FALSE
    which_event$csv <- TRUE
    which_event$gpkg <- FALSE
  })

  # the switch for which_event$gpkg is in sf_data(), when the spatial dataframe
  # passes the checks and is successfully loaded

  possibleInputs <- reactive({
    list(
      input$filter_button,
      input$uploaded_csv,
      which_event$gpkg
    )
  })

  my_data <- eventReactive(possibleInputs(), {
    tryCatch(
      {
        df <- NULL

        updatePrettyCheckbox( # uploading a new csv or running a new query "overwrites"
          session = session, # the "clip" mode, so that the summaries and analyses shown
          inputId = "clip", # are always related to the latest loaded data
          value = FALSE
        )

        if (which_event$query) {
          showModal(
            modalDialog(
              "Constructing SQL Query...",
              footer = NULL
            )
          )

          output$queried_table <- renderDataTable({})

          table_name_ui <- input$table_name_ui

          table_full_name <- paste(
            project,
            dataset,
            tables_list[match(
              table_name_ui,
              tables_list_ui
            )],
            sep = "."
          )

          SQL <- "SELECT * FROM {`table_full_name`}"

          first_date <- min(input$date)
          second_date <- max(input$date)

          checked_boxes <- input$filter_columns_ui

          table_name_ui <- input$table_name_ui

          if ((!is.null(first_date) && !is.na(first_date)) && (!is.null(second_date) && !is.na(second_date))) {
            date_SQL <- "AND date >= {first_date} AND date < {second_date}"

            SQL <- paste(
              SQL,
              date_SQL,
              sep = " "
            )
          }

          for (field in checked_boxes) {
            if (field == "date" || field == "flag" || field == "geartype" || field == "mmsi") {
              next
            } else {
              first_field <- min(input[[field]])
              second_field <- max(input[[field]])

              if (

                (!is.null(first_field) && !is.na(first_field)) &&
                  (!is.null(second_field) && !is.na(second_field))) {
                next_SQL <- sprintf(
                  "AND %s >= {%s} AND %s < {%s}",
                  field,
                  first_field,
                  field,
                  second_field
                )

                SQL <- paste(
                  SQL,
                  next_SQL,
                  sep = " "
                )
              }
            }
          }

          mmsi <- input$mmsi
          
          if (("mmsi" %in% checked_boxes) && (mmsi != "")) {
            mmsi_SQL <- sprintf(
              "AND mmsi LIKE '%s'",
              mmsi
            )

            SQL <- paste(
              SQL,
              mmsi_SQL,
              sep = " "
            )
          }

          flags <- input$flag

          if (!is.null(flags) && !is.na(flags) && ("flag" %in% checked_boxes)) {
            flag_SQL <- "AND ("

            for (isocode in flags) {
              next_flag_SQL <- sprintf(
                "flag = '%s' OR",
                isocode
              )

              flag_SQL <- paste(
                flag_SQL,
                next_flag_SQL,
                sep = " "
              )
            }

            flag_SQL <- stri_replace_last_fixed(
              flag_SQL,
              " OR",
              ")"
            )

            SQL <- paste(
              SQL,
              flag_SQL,
              sep = " "
            )
          }

          gears <- input$geartype

          if (!is.null(gears) && !is.na(gears) && ("geartype" %in% checked_boxes)) {
            geartype_SQL <- "AND ("

            for (gear in gears) {
              next_geartype_SQL <- sprintf(
                "geartype = '%s' OR",
                gear
              )

              geartype_SQL <- paste(
                geartype_SQL,
                next_geartype_SQL,
                sep = " "
              )
            }

            geartype_SQL <- stri_replace_last_fixed(
              geartype_SQL,
              " OR",
              ")"
            )

            SQL <- paste(
              SQL,
              geartype_SQL,
              sep = " "
            )
          }

          SQL <- sub(
            "AND",
            "WHERE",
            SQL
          )

          SQL_count <- sub(
            "SELECT ",
            "SELECT COUNT(",
            SQL
          )

          SQL_count <- sub(
            " FROM",
            ") as count_col FROM",
            SQL_count
          )

          GLUED_SQL <- glue_sql(
            SQL,
            .con = BQ_connection
          )

          GLUED_SQL_count <- glue_sql(
            SQL_count,
            .con = BQ_connection
          )

          output$sql_query <- renderText({
            GLUED_SQL
          })

          count <- dbGetQuery(
            BQ_connection,
            GLUED_SQL_count
          )

          max_rows <- 1000000 # add more 0s if you don't care for speed and need to perform large analyses

          if (count$count_col <= max_rows) {
            showModal(
              modalDialog(
                "Fishing for data...",
                footer = NULL
              )
            )

            df <- dbGetQuery(
              BQ_connection,
              GLUED_SQL
            )

            showModal(
              modalDialog(
                "Building table...",
                footer = NULL
              )
            )

            output$queried_table <- renderDataTable(df)

            enable(id = "download_button")

            output$download_button <- downloadHandler(
              filename = function() {
                paste(
                  "data-",
                  Sys.Date(),
                  ".csv",
                  sep = ""
                )
              },
              content = function(con) {
                write.csv(
                  df,
                  con,
                  row.names = F
                )
              }
            )

            removeModal()
          } else {
            message <- sprintf(
              "Your query would return more than %s rows (%s to be precise), too many for the server. Please, be more specific in your query or run the source code from your own machine.",
              max_rows,
              count$count_col
            )


            showModal(
              modalDialog(
                message
              )
            )
          }
        } else if (which_event$csv) {
          showModal(
            modalDialog(
              "Uploading your data...",
              footer = NULL
            )
          )

          df <- read.csv(input$uploaded_csv$datapath,
            header = TRUE,
            sep = ",",
            quote = '"'
          )

          col_names_csv <- colnames(df)

          if ((isFALSE(all.equal(col_names_csv, column_100th))) && (isFALSE(all.equal(col_names_csv, column_10th)))) {
            df <- NULL
          }

          removeModal()
        } else if (which_event$gpkg) {
          sdf <- sf_data()

          if (!is.null(sdf) && !is.na(sdf)) {
            df <- sdf %>%
              dplyr::mutate(
                cell_ll_lon = sf::st_coordinates(.)[, 1],
                cell_ll_lat = sf::st_coordinates(.)[, 2]
              ) %>%
              as.data.frame()

            df <- select(df, -c(geom))

            if (length(colnames(df)) == 8) { # this is to preserve the same order for

              df <- select(df, column_100th) # colnames, to avoid eventual inconsistencies
            } else if (length(colnames(df)) == 6) {
              df <- select(df, column_10th)
            }
          } else {
            df <- NULL
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while loading the dataframe. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )

    return(df)
  })

  # function to enable/disable/update inputs related to my_data/clipped_data
  observe({
    tryCatch(
      {
        df <- my_data()

        whether_to_clip <- input$clip

        if (whether_to_clip) {
          df <- clipped_data()
        }

        col_names_df <- colnames(df)

        if (isTRUE(all.equal(col_names_df, column_100th))) {
          summaries <- available_summaries_100th

          enable(id = "summarize_button")
          enable(id = "convert_to_spatial_button")
        } else if (isTRUE(all.equal(col_names_df, column_10th))) {
          summaries <- available_summaries_10th

          enable(id = "summarize_button")
          enable(id = "convert_to_spatial_button")
        } else {
          summaries <- NULL

          disable(id = "summarize_button")
          disable(id = "convert_to_spatial_button")
        }

        updatePrettyCheckboxGroup(
          session,
          inputId = "summaries",
          choices = summaries
        )
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while updating the summaries. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  output$uploaded_csv_viz <- renderTable({
    tryCatch(
      { # renderTable must have had an update

        df <- my_data() # that is messing with the date format. In an attempt to

        if (length(df$date) != 0) { # solve that, I also had issues with the header,

          header <- head(my_data()) # so this is my solution for now

          header$date <- as.character(as.Date(header$date, "%Y-%m-%d"))

          return(header)
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while rendering the preview table. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  observeEvent(input$summarize_button, {
    tryCatch(
      {
        showModal(
          modalDialog(
            "Summarizing...",
            footer = NULL
          )
        )

        choice <- input$summaries

        if (length(choice) < 8) {
          whether_to_clip <- input$clip

          if (whether_to_clip) {
            df <- clipped_data()
          } else {
            df <- my_data()
          }

          if (!is.null(choice)) {
            if ("month" %in% choice) { # allow users to summarise by month

              df$month <- substr(
                df$date,
                start = 1,
                stop = 7
              )
            }

            if ("year" %in% choice) { # allow users to summarise by year

              df$year <- substr(
                df$date,
                start = 1,
                stop = 4
              )
            }

            summarized <- group_by_at(df, vars(one_of(choice))) %>%
              summarize(
                "Total fishing" = sum(fishing_hours),
                "Min. fishing" = min(fishing_hours),
                "1st Qu. fishing" = quantile(fishing_hours, 0.25),
                "Median fishing" = median(fishing_hours),
                "Mean fishing" = mean(fishing_hours),
                "3rd Qu. fishing" = quantile(fishing_hours, 0.75),
                "Max. fishing" = max(fishing_hours),
                "Total hours" = sum(hours),
                "Min. hours" = min(hours),
                "1st Qu. hours" = quantile(hours, 0.25),
                "Median hours" = median(hours),
                "Mean hours" = mean(hours),
                "3rd Qu. hours" = quantile(hours, 0.75),
                "Max. hours" = max(hours)
              )
          } else {
            summarized <- summarize(df,
              "Total fishing" = sum(fishing_hours),
              "Min. fishing" = min(fishing_hours),
              "1st Qu. fishing" = quantile(fishing_hours, 0.25),
              "Median fishing" = median(fishing_hours),
              "Mean fishing" = mean(fishing_hours),
              "3rd Qu. fishing" = quantile(fishing_hours, 0.75),
              "Max. fishing" = max(fishing_hours),
              "Total hours" = sum(hours),
              "Min. hours" = min(hours),
              "1st Qu. hours" = quantile(hours, 0.25),
              "Median hours" = median(hours),
              "Mean hours" = mean(hours),
              "3rd Qu. hours" = quantile(hours, 0.75),
              "Max. hours" = max(hours)
            )
          }

          columns_to_append <- c(
            "Total fishing", # this is to limit the amount of fields
            "Mean fishing", # displayed on screen, since one could
            "Total hours", # group by every single field and obtain
            "Mean hours"
          ) # over 20 columns

          columns_to_show <- append(choice, columns_to_append)

          output$summary_preview <- renderDataTable({
            summarized[, columns_to_show]
          })

          output$download_analyses_button <- downloadHandler(
            filename = function() {
              paste(
                "summary-",
                Sys.Date(),
                ".csv",
                sep = ""
              )
            },
            content = function(file) {
              write.csv(summarized,
                file,
                row.names = F
              )
            }
          )

          enable(id = "download_analyses_button")

          removeModal()
        } else {
          showModal(
            modalDialog(
              "Maximum 7 fields"
            )
          )
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while producing the summaries. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  which_sf_event <- reactiveValues(
    gpkg = FALSE,
    converted = FALSE
  )

  observeEvent(input$convert_to_spatial_button, {
    which_sf_event$converted <- TRUE
    which_sf_event$gpkg <- FALSE
  })

  observeEvent(input$uploaded_gpkg, {
    which_sf_event$converted <- FALSE
    which_sf_event$gpkg <- TRUE
  })

  possibleSpatialInputs <- reactive({
    list(
      input$uploaded_gpkg,
      input$convert_to_spatial_button
    )
  })

  sf_data <- eventReactive(possibleSpatialInputs(), {
    sdf <- NULL

    tryCatch(
      {
        shinyjs::hide(id = "map") # to avoid users messing with revisualisation, possibly skipping the checks

        updatePrettyCheckbox( # uploading or converting into a new gpkg "overwrites"
          session = session, # the "clip" mode, so that the checks can be run again
          inputId = "clip",
          value = FALSE
        )

        disable(id = "clip") # to avoid users purposefully loading a gpkg that might
        # break the clip function without first passing the due checks

        if (which_sf_event$converted) {
          showModal(
            modalDialog(
              "Converting data to GeoPackage...",
              footer = NULL
            )
          )

          df <- my_data()

          col_names_conv <- colnames(df)

          sdf <- st_as_sf(df,
            coords = c(
              "cell_ll_lon",
              "cell_ll_lat"
            )
          )

          colnames(sdf)[colnames(sdf) == "geometry"] <- "geom" # this happens because
          # st_write creates a column "geom",
          # but st_as_sf names it "geometry", messing
          # with my system of checking colnames

          st_geometry(sdf) <- "geom" # otherwise the sf object would still look for "geometry" for its geometry

          st_crs(sdf) <- 4326 # make sure to set the CRS, which is a very important
          # check later on

          removeModal()
        } else if (which_sf_event$gpkg) {
          showModal(
            modalDialog(
              "Uploading GeoPackage...",
              footer = NULL
            )
          )

          layers <- st_layers(input$uploaded_gpkg$datapath)

          layers_name <- layers$name

          # this is to check for the presence of "GFW" layer before asking
          # the code to actually read that layer
          if ("GFW" %in% layers_name) {
            sdf <- st_read(input$uploaded_gpkg$datapath,
              layer = "GFW",
              geometry_column = "geom"
            )

            col_names_gpkg <- colnames(sdf)

            removeModal()

            if (((isFALSE(all.equal(col_names_gpkg, sf_column_100th)) && isFALSE(all.equal(col_names_gpkg, sf_column_10th)))) || (st_crs(sdf) != st_crs(4326))) {
              sdf <- NULL

              showModal(
                modalDialog(
                  "Please upload a file originating from fishRman"
                )
              )
            }

            which_event$gpkg <- TRUE # this part of the switch had to be moved here
            which_event$query <- FALSE # to avoid making repetitive checks just for
            which_event$csv <- FALSE # these boolean values
          } else {
            sdf <- NULL

            showModal(
              modalDialog(
                "Please upload a file originating from fishRman"
              )
            )
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while loading the spatial dataframe. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )

    return(sdf)
  })

  # all inputs relying on sdf are enabled/disabled here depending on the fact
  # that sdf exists and has at least one point
  observe({
    tryCatch(
      {
        sdf <- sf_data()

        whether_to_clip <- input$clip

        if (whether_to_clip) {
          sdf <- clipped_sf_data()
        }

        if ((!is.null(sdf)) && (length(sdf$geom) > 0)) {
          enable(id = "download_gpkg_button")
          enable(id = "visualize_button")
          enable(id = "second_uploaded_gpkg")
          enable(id = "re_visualize_button")
        } else {
          disable(id = "download_gpkg_button")
          disable(id = "visualize_button")
          disable(id = "second_uploaded_gpkg")
          disable(id = "re_visualize_button")
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while enabling/disabling spatial inputs. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # to download either clipped or unclipped data
  output$download_gpkg_button <- tryCatch(
    {
      downloadHandler(
        filename = function() {
          paste(
            "spatial-data-",
            Sys.Date(),
            ".gpkg",
            sep = ""
          )
        },
        content = function(file) {
          whether_to_clip <- input$clip

          if (whether_to_clip) {
            sdf <- clipped_sf_data()
          } else {
            sdf <- sf_data()
          }

          world_sf <- sf::st_as_sf(
            maps::map(
              "world",
              plot = FALSE,
              fill = TRUE
            )
          )

          st_write(
            sdf,
            file,
            layer = "GFW",
            driver = "GPKG"
          )

          st_write(
            world_sf,
            file,
            layer = "Land",
            driver = "GPKG",
            append = TRUE
          )
        }
      )
    },
    error = function(err) {
      showModal(
        modalDialog(
          paste("The error '", err, "' arose while downloading spatial data. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
        )
      )
    }
  )

  # function to update list for selection of layer for clipping
  # first of a series of checks to make the process as safe as possible
  observeEvent(input$second_uploaded_gpkg, {
    tryCatch(
      {
        showModal(
          modalDialog(
            "Uploading GeoPackage...",
            footer = NULL
          )
        )

        layers <- st_layers(input$second_uploaded_gpkg$datapath)

        layers_name <- layers$name

        updateSelectInput(
          inputId = "second_gpkg_layer",
          choices = layers_name
        )

        enable(id = "second_gpkg_layer")

        removeModal()
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while checking the layers of the area to clip. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # function to just CHECK whether the chosen layer of the uploaded geopackage is
  # in CRS EPSG 4326 and is a POLYGON/MULTIPOLYGON, thus suitable for st_intersects
  observeEvent(input$second_gpkg_layer, {
    tryCatch(
      {
        chosen_layer <- input$second_gpkg_layer

        dsn <- input$second_uploaded_gpkg$datapath

        sdf <- sf_data

        if ((!is.null(dsn)) && (!is.na(dsn))) {
          area_of_interest <- st_read(
            dsn = dsn,
            layer = chosen_layer
          )

          geom_type <- st_geometry(area_of_interest) %>%
            class()

          if (("sfc_POLYGON" %in% geom_type) || ("sfc_MULTIPOLYGON" %in% geom_type)) {
            if (st_crs(sf_data()) == st_crs(area_of_interest)) {
              enable(id = "clip")
            } else {
              disable(id = "clip")

              showModal(
                modalDialog(
                  "CRS does not match. Please upload a file with CRS 'EPSG 4326'"
                )
              )
            }
          } else {
            disable(id = "clip")

            showModal(
              modalDialog(
                "Geometry type is invalid. Please choose a layer with geometry type 'POLYGON' or 'MULTIPOLYGON'"
              )
            )
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while loading the area to clip. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # function to get the insersection between GFW points and uploaded polygons
  # de facto executing a clip of the points falling in the polygons
  clipped_sf_data <- eventReactive(input$clip, {
    tryCatch(
      {
        whether_to_clip <- input$clip

        if (whether_to_clip) {
          
          showModal(
            modalDialog(
              "Clipping data...",
              footer = NULL
            )
          )
          
          chosen_layer <- input$second_gpkg_layer

          dsn <- input$second_uploaded_gpkg$datapath

          points <- sf_data()

          area_of_interest <- st_read(
            dsn = dsn,
            layer = chosen_layer
          )
          
          area_of_interest <- area_of_interest["geom"]
          
          clipped_points <- points[st_intersects(points, area_of_interest) %>% lengths > 0,]
          
          if (length(clipped_points$geom) != 0) {
            
            removeModal()

            return(clipped_points)
            
          } else {
            updatePrettyCheckbox(
              session = session,
              inputId = "clip",
              value = FALSE
            )

            showModal(
              modalDialog(
                "The two dataframes do not intersect."
              )
            )

            return(NULL)
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while clipping the data. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # function to get csv-like data from clipped spatial data
  clipped_data <- reactive({
    tryCatch(
      {
        df <- NULL

        sdf <- clipped_sf_data()

        if (!is.null(sdf) && !is.na(sdf)) {
          df <- sdf %>%
            dplyr::mutate(
              cell_ll_lon = sf::st_coordinates(.)[, 1],
              cell_ll_lat = sf::st_coordinates(.)[, 2]
            ) %>%
            as.data.frame()

          df <- select(df, -c(geom))

          if (length(colnames(df)) == 8) { # this is to preserve the same order for

            df <- select(df, column_100th) # colnames, to avoid eventual inconsistencies
          } else if (length(colnames(df)) == 6) {
            df <- select(df, column_10th)
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while retrieving a dataframe from clipped data. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )

    return(df)
  })

  which_viz_event <- reactiveValues(
    origin = FALSE,
    reviz = FALSE
  )

  observeEvent(input$visualize_button, {
    which_viz_event$origin <- TRUE
    which_viz_event$reviz <- FALSE
  })

  observeEvent(input$re_visualize_button, {
    which_viz_event$origin <- FALSE
    which_viz_event$reviz <- TRUE
  })

  possibleVizInputs <- reactive({
    list(
      input$visualize_button,
      input$re_visualize_button
    )
  })

  # what happens here with bbox/plot_range is so that the plot can "zoom in" on the area
  # of interest, with a padding of 5% of the x and y range on either side of
  # the range

  plot_range <- reactiveValues()

  observeEvent(possibleVizInputs(), {
    tryCatch(
      {
        whether_to_clip <- input$clip

        if (whether_to_clip) {
          sdf <- clipped_sf_data()
        } else {
          sdf <- sf_data()
        }

        if ((!is.null(sdf)) && (length(sdf$geom) > 0)) {
          bbox <- st_bbox(sdf)

          xmin <- bbox$xmin
          xmax <- bbox$xmax
          ymin <- bbox$ymin
          ymax <- bbox$ymax

          if (((which_viz_event$reviz)) && (!is.na(input$xrange[1])) && (!is.na(input$xrange[2]) && (!is.na(input$yrange[1])) && (!is.na(input$yrange[2])) && (input$xrange[1] != input$xrange[2]) && (input$yrange[1] != input$yrange[2]))) {
            xmin <- min(input$xrange)
            xmax <- max(input$xrange)
            ymin <- min(input$yrange)
            ymax <- max(input$yrange)
            
            
          }

          xbuff <- (xmax - xmin) * 0.05
          ybuff <- (ymax - ymin) * 0.05

          plot_range$lowx <- xmin - xbuff
          plot_range$highx <- xmax + xbuff
          plot_range$lowy <- ymin - ybuff
          plot_range$highy <- ymax + ybuff

          updateNumericRangeInput(session,
            inputId = "xrange",
            value = c(xmin, xmax)
          )

          updateNumericRangeInput(session,
            inputId = "yrange",
            value = c(ymin, ymax)
          )
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while handling the bbox. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  rez <- reactive({ # assign resolution values on clipped and not clipped spatial data

    rez <- 0.1

    tryCatch(
      {
        whether_to_clip <- input$clip

        if (whether_to_clip) {
          sdf <- clipped_sf_data()
        } else {
          sdf <- sf_data()
        }

        col_names_sdf <- colnames(sdf)

        df <- sdf %>%
          dplyr::mutate(
            lon = sf::st_coordinates(.)[, 1],
            lat = sf::st_coordinates(.)[, 2]
          ) # lat and lon are easier to work with than "geom"

        if (which_viz_event$origin) {
          if (isTRUE(all.equal(col_names_sdf, sf_column_100th))) {
            updateNumericInput(session,
              inputId = "map_rez",
              min = 0.01,
              max = 2,
              value = 0.01
            )
            rez <- 0.01
          } else if (isTRUE(all.equal(col_names_sdf, sf_column_10th))) {
            updateNumericInput(session,
              inputId = "map_rez",
              min = 0.1,
              max = 2,
              value = 0.1
            )

            rez <- 0.1
          }
        } else if (which_viz_event$reviz) {
          rez <- input$map_rez
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while handling the resolution of the plot. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )

    return(rez)
  })

  observeEvent(possibleVizInputs(), {
    tryCatch(
      {
        if (which_viz_event$origin || which_viz_event$reviz) {
          showModal(
            modalDialog(
              "Visualizing your data...",
              footer = NULL
            )
          )

          whether_to_clip <- input$clip

          if (whether_to_clip) {
            sdf <- clipped_sf_data()
          } else {
            sdf <- sf_data()
          }

          col_names_sdf <- colnames(sdf)

          df <- sdf %>%
            dplyr::mutate(
              lon = sf::st_coordinates(.)[, 1],
              lat = sf::st_coordinates(.)[, 2]
            ) # lat and lon are easier to work with than "geom"

          rez <- rez()

          # if statements to reaggregate data according to rez
          if (isTRUE(all.equal(col_names_sdf, sf_column_100th))) {
            if (is.numeric(rez) && rez > 0.01 && rez <= 2) {
              df <- df %>%
                mutate(
                  lat = floor(lat / rez) * rez + 0.5 * rez,
                  lon = floor(lon / rez) * rez + 0.5 * rez
                )
            } # this sets new lat and lon for the new aggregation

            grouped_df <- df %>%
              # aggregation starts here

              as.data.frame() %>%
              group_by(lon, lat) %>%
              summarise(
                "Total fishing hours" = sum(fishing_hours),
                "Total hours" = sum(hours),
                "Total MMSI present" = sum(mmsi_present),
                "Mean fishing hours" = mean(fishing_hours),
                "Mean hours" = mean(hours),
                "Mean MMSI present" = mean(mmsi_present)
              )
          } else if (isTRUE(all.equal(col_names_sdf, sf_column_10th))) {
            if (is.numeric(rez) && rez > 0.1 && rez <= 2) {
              df <- df %>%
                mutate(
                  lat = floor(lat / rez) * rez + 0.5 * rez,
                  lon = floor(lon / rez) * rez + 0.5 * rez
                )
            }

            grouped_df <- df %>%
              as.data.frame() %>%
              group_by(lon, lat) %>%
              summarise(
                "Total fishing hours" = sum(fishing_hours),
                "Total hours" = sum(hours),
                "Mean fishing hours" = mean(fishing_hours),
                "Mean hours" = mean(hours)
              )
          }

          # if statement to assign to_fill on origin and on revisualize
          if (which_viz_event$origin) {
            col_names_grouped <- colnames(grouped_df)

            col_names_grouped_no_geo <- col_names_grouped[!col_names_grouped %in% c("lat", "lon")]

            updateSelectInput(session,
              inputId = "mapped_column",
              choices = col_names_grouped_no_geo
            )

            to_fill <- "Total fishing hours"
          } else {
            to_fill <- input$mapped_column
          } # basically, if it's the first plot, it defaults to "Total fishing hours" to fill, otherwise, it is the chosen field

          # sort table according to to_fill, decreasing
          grouped_df <- grouped_df[order(-grouped_df[to_fill]), ]

          cumul_distr_percent <- input$cumul_distr_percent

          # calculate sum of the entire column to_fill
          summed <- sum(grouped_df[to_fill]) * (cumul_distr_percent / 100)

          # vectorize the column, to be able to use the length.until function
          to_fill_column <- grouped_df %>%
            pull(to_fill)

          # how many rows are needed to reach the cumul_distr_percent% of the total for to_fill?
          cumul_distr_length <- length.until(to_fill_column, summed)

          # subsetting grouped_df accordingly to cumul_distr_percent
          grouped_df <- grouped_df[c(1:cumul_distr_length$length), ]

          world_sf <- sf::st_as_sf( # world map to give some reference
            maps::map(
              "world",
              plot = FALSE,
              fill = TRUE
            )
          )
          
          layers_added <- input$add_layer
          
          if ("eez_boundaries_v11.gpkg" %in% layers_added) {
            
            world_eez <- st_read("./www/geo/eez_boundaries_v11.gpkg", 
                                 layer = "eez_boundaries_v11",
                                 geometry_column = "geom"
            )}
          
          if ("eez_24nm_v3_boundaries.gpkg" %in% layers_added) {
            
            world_24nm <- st_read("./www/geo/eez_24nm_v3_boundaries.gpkg", 
                                 layer = "eez_24nm_v3_boundaries",
                                 geometry_column = "geom"
            )}
          
          if ("eez_12nm_v3_boundaries.gpkg" %in% layers_added) {
            
            world_12nm <- st_read("./www/geo/eez_12nm_v3_boundaries.gpkg", 
                                 layer = "eez_12nm_v3_boundaries",
                                 geometry_column = "geom"
            )}

          lowx <- plot_range$lowx
          highx <- plot_range$highx
          lowy <- plot_range$lowy
          highy <- plot_range$highy

          map <- ggplot() +
            geom_tile(data = grouped_df, aes(x = lon, y = lat, fill = .data[[to_fill]])) +
            scale_fill_viridis() +
            geom_sf(
              data = world_sf,
              fill = "#BABABA",
              color = "#0A1738",
              size = 0.1
            ) 
          
          if (exists("world_eez")) {
            map = map + geom_sf(
              data = world_eez,
              fill = "#BABABA",
              color = "#BABABA",
              size = 0.5
            ) }
          
          if (exists("world_24nm")) {
            map = map + geom_sf(
                data = world_24nm,
                fill = "#BABABA",
                color = "#BABABA",
                size = 0.5
              ) }
          
          if (exists("world_12nm")) {
            map = map + geom_sf(
                data = world_12nm,
                fill = "#BABABA",
                color = "#BABABA",
                size = 0.5
            ) }
          
          map = map + coord_sf(
            xlim = c(lowx, highx), # these are the zoom in coordinates
            ylim = c(lowy, highy), # I mentioned earlier
            expand = FALSE
          ) 

          output$viz_map <- renderPlot({
            return(map)
          })

          shinyjs::show("map") # the map output is always present on screen, it is only
          # shown when in use in order to have a cleaner UI

          output$download_map_button <- downloadHandler(
            filename = function() {
              paste(
                to_fill,
                "-",
                Sys.Date(),
                ".png",
                sep = ""
              )
            },
            content = function(file) {
              ggsave(map, filename = file)
            }
          )

          removeModal()
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            paste("The error '", err, "' arose while producing the plot. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })
}
