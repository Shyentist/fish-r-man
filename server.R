server <- function(input, output, session) {

  intro.message()

  #depending on the chosen table, update the input checkboxes
  
  observeEvent(input$table_name_ui, {
    
    table_name_ui <- input$table_name_ui

    # check which table the user decided to query, and presents the checkboxes
    # of the fields (columns) of that table
    
    if (table_name_ui == "fishing_effort_byvessel_v2"){
      
      fields_list <- column_10th
      
      names(fields_list) <- column_list_fe10_ui
      
    } else {
      
      fields_list <- column_100th
      
      names(fields_list) <- column_list_fe100_ui
      
    }
    
    updatePrettyCheckboxGroup( # this way, the checkboxes are always named after the table fields
      session,
      inputId = "filter_columns_ui",
      choices = fields_list
    )
  })

  # what follow enables the input boxes that have had their respective checkboxes
  # ticked, disabling the others. This way I can build the SQL query from the
  # checked boxes, rather than the existing inputs, which could have been input-ed
  # when it was allowed, then disabled by changing table to one that does not allow
  # that input

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
  # that must differentiate among the various types of inputs that they can receive
  # for instance, both when uploading a csv or querying data, I need the final
  # result to be assigned to 'my_data', so that later on I can just refer to this
  # value, which is qualitatively the same for both sources

  which_event <- reactiveValues(
    query = FALSE, # activated when new data is queried
    csv = FALSE, # activated when new data is uploaded via csv
    gpkg = FALSE # activated when new data is uploaded via gpkg
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

  # I used the possibleInputs list to have reactive events start at any
  # change of the list, then check which of the previously mentioned
  # switches was turned on (TRUE)
  
  possibleInputs <- reactive({
    list(
      input$filter_button,
      input$uploaded_csv,
      which_event$gpkg
    )
  })

  # this function takes different paths depending on the input
  # if "Filter" button is pressed, it creates an SQL Query runs it
  # if csv or gpkg files are uploaded, it assign the dataframe to "my_data"
  
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
              size = "l",
              "Constructing SQL Query...",
              footer = NULL
            )
          )

          output$queried_table <- renderDataTable({}) # empty the table before starting the function

          table <- input$table_name_ui

          table_full_name <- paste(
            project,
            dataset,
            table,
            sep = "."
          )

          # Below, I check which filter boxes are checked, and assign the input
          # value to a variable named exactly like the input's id. If the checkbox
          # is unchecked, I assign NULL to the respective variable.
          # I do this because, otherwise, one could i.e. select the 100th degree
          # table, which has a flag and a geartype column, check those boxes,
          # input something and then switch to the 10th degree table, which does
          # not have the columns. The inputs would only be grayed out, not deleted,
          # so the query would have them all the same.

          checked_boxes <- input$filter_columns_ui

          for (id in list_togglable_ids) {
            if (id %in% checked_boxes) {
              assign(id, input[[id]])
            } else {
              assign(id, NULL)
            }
          }

          # start the SQL constructor
          SQL <- sql.construct(
            table = table_full_name,
            date = date,
            cell_ll_lat = cell_ll_lat,
            cell_ll_lon = cell_ll_lon,
            hours = hours,
            fishing_hours = fishing_hours,
            mmsi_present = mmsi_present,
            flag = flag,
            geartype = geartype,
            mmsi = mmsi
          )

          # creating the query to count the number of rows the resulting table 
          # would have, so I can put a cap to avoid huge queries

          SQL_count <- count.sql(SQL)

          output$sql_query <- renderText({
            SQL
          })
          
          billing <- input$billing
          
          BQ_connection <- dbConnect(bigquery(),
                                     project = project,
                                     dataset = dataset,
                                     billing = billing,
                                     use_legacy_sql = FALSE
          ) # specify we are using Standard SQL

          count <- dbGetQuery(
            BQ_connection,
            SQL_count
          )

          max_rows <- 1000000 # add more 0s if you don't care for speed and need to perform large analyses

          # and here is where the count from SQL_count is used
          # to check the number of rows the query would retrieve
          # against max_rows just above
          
          if (count$count_col <= max_rows) {
            showModal(
              modalDialog(
                size = "l",
                "Fishing for data...",
                footer = NULL
              )
            )

            df <- dbGetQuery(
              BQ_connection,
              SQL
            )

            showModal(
              modalDialog(
                size = "l",
                "Building table...",
                footer = NULL
              )
            )

            output$queried_table <- renderDataTable(df) # now the table can be repopulated

            enable(id = "download_button") # and the download button enabled

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
                size = "l",
                message
              )
            )
          }
        } else if (which_event$csv) {
          showModal(
            modalDialog(
              size = "l",
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

          # I just check for the col_names to know whether the file
          # is compatible. Someone could go out of their way to make
          # a csv with the same col_names and different types of data
          # to crash the app for the sake of it, but no one else should
          # have any issue
          if ((isFALSE(all.equal(col_names_csv, column_100th))) && (isFALSE(all.equal(col_names_csv, column_10th)))) {
            df <- NULL
          }

          removeModal()
        } else if (which_event$gpkg) {
          sdf <- sf_data()

          # if the spatial data is not empty, then I revert the "geom"
          # column to cell_ll_lon and cell_ll_lat for consistency, before
          # assigning them to the dataframe
          if (!is.null(sdf) && !is.na(sdf)) {
            df <- sdf %>%
              dplyr::mutate(
                cell_ll_lon = sf::st_coordinates(.)[, 1],
                cell_ll_lat = sf::st_coordinates(.)[, 2]
              ) %>%
              as.data.frame()

            # and here I delete the "geom" column from the database
            df <- select(df, -c(geom))

            # this is to preserve the same order for
            # colnames, to avoid eventual inconsistencies
            # if the number of columns does not match
            # the df is set to NULL, because something went wrong
            if (length(colnames(df)) == 8) {
              df <- select(df, column_100th)
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
            size = "l",
            paste("The error '", err, "' arose while loading the dataframe. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )

    return(df)
  })

  # function to enable/disable/update inputs related to my_data/clipped_data
  # because I don't want inputs available to the user if that could break the app
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
            size = "l",
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
            size = "l",
            paste("The error '", err, "' arose while rendering the preview table. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # Summarizing is simple with a few tricks for month and year
  observeEvent(input$summarize_button, {
    tryCatch(
      {
        showModal(
          modalDialog(
            size = "l",
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

          # since there is no month column, the app has to create it now
          # in order to summarise by it
          if (!is.null(choice)) {
            if ("month" %in% choice) {
              df$month <- substr(
                df$date,
                start = 1,
                stop = 7
              )
            }

            # since there is no month column, the app has to create it now
            # in order to summarise by it
            if ("year" %in% choice) { # allow users to summarise by year

              df$year <- substr(
                df$date,
                start = 1,
                stop = 4
              )
            }

            # although only Total and mean are displayed, the actual table that
            # can be downloaded has more complete info such as quartiles and median
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

            # if no choice is made for aggregation, the entire dataframe is summarized
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

          # the preview-table to show instead of the full one, with only
          # the aggregation column and the derived results
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
              size = "l",
              "Maximum 7 fields"
            )
          )
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
            paste("The error '", err, "' arose while producing the summaries. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # again, switches, so that the functions depending on a list of them
  # are triggered by any change in the list. Then, to decide which
  # route the function should take, we check which switch is on (TRUE)
  which_sf_event <- reactiveValues(
    gpkg = FALSE, # activates when a gpkg file is uploaded, creating a new sf_data
    converted = FALSE # activates when sf_data is created via the 'Convert' button
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
              size = "l",
              "Converting data to GeoPackage...",
              footer = NULL
            )
          )

          df <- my_data()

          col_names_conv <- colnames(df)

          # create a geom/geometry column via cell_ll_lat and cell_ll_lon
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
              size = "l",
              "Uploading GeoPackage...",
              footer = NULL
            )
          )

          # this is to check for the presence of "GFW" layer before asking
          # the code to actually read that layer
          layers <- st_layers(input$uploaded_gpkg$datapath)

          layers_name <- layers$name

          if ("GFW" %in% layers_name) {
            sdf <- st_read(input$uploaded_gpkg$datapath,
              layer = "GFW",
              geometry_column = "geom"
            )

            col_names_gpkg <- colnames(sdf)

            removeModal()

            if (((isFALSE(all.equal(col_names_gpkg, sf_column_100th)) && isFALSE(all.equal(col_names_gpkg, sf_column_10th)))) || (st_crs(sdf) != st_crs(4326))) {

              # if the gpkg file, at the GFW layer, does not have the right column
              # names, the function is aborted and sdf (later sf_data) is set to NULL
              sdf <- NULL

              showModal(
                modalDialog(
                  size = "l",
                  "Please upload a file originating from fishRman"
                )
              )
            }

            which_event$gpkg <- TRUE # this part of the switch had to be moved here
            which_event$query <- FALSE # to avoid making repetitive checks just for
            which_event$csv <- FALSE # these boolean values
          } else {

            # if there is no GFW layer at all in the gpkg, the function
            # is aborted and the sdf (later sf_data) is set to NULL
            sdf <- NULL

            showModal(
              modalDialog(
                size = "l",
                "Please upload a file originating from fishRman"
              )
            )
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
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
            size = "l",
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
          size = "l",
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
            size = "l",
            "Uploading GeoPackage...",
            footer = NULL
          )
        )

        # look for the names of the layers in the file to fill
        # the selectInput
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
            size = "l",
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
                  size = "l",
                  "CRS does not match. Please upload a file with CRS 'EPSG 4326'"
                )
              )
            }
          } else {
            disable(id = "clip")

            showModal(
              modalDialog(
                size = "l",
                "Geometry type is invalid. Please choose a layer with geometry type 'POLYGON' or 'MULTIPOLYGON'"
              )
            )
          }
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
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
              size = "l",
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

          # we use st_intersects() instead of st_intersection because we don't
          # need to clip a larger geometry to only match another area, we just
          # need to filter the points that DO fall within such area. This means
          # that a filtering by a TRUE-FALSE result such as that of st_intersect()
          # performs better
          clipped_points <- points[st_intersects(points, area_of_interest) %>% lengths() > 0, ]

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
                size = "l",
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
            size = "l",
            paste("The error '", err, "' arose while clipping the data. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # function to derive csv-like (non-spatial, turning geom column into
  # cell_ll_lat and cell_ll_lon) data from clipped spatial data
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
            size = "l",
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

        # first, just use the bbox boundaries to set the coords for the plot
        if ((!is.null(sdf)) && (length(sdf$geom) > 0)) {
          bbox <- st_bbox(sdf)

          xmin <- bbox$xmin
          xmax <- bbox$xmax
          ymin <- bbox$ymin
          ymax <- bbox$ymax

          # then, if the plot gets revisualized, probably with new coordinates
          # assign such coordinates to xmin, xmax, ymin, ymax
          if (((which_viz_event$reviz)) && (!is.na(input$xrange[1])) && (!is.na(input$xrange[2]) && (!is.na(input$yrange[1])) && (!is.na(input$yrange[2])) && (input$xrange[1] != input$xrange[2]) && (input$yrange[1] != input$yrange[2]))) {
            xmin <- min(input$xrange)
            xmax <- max(input$xrange)
            ymin <- min(input$yrange)
            ymax <- max(input$yrange)
          }

          # a little buffer of 5% of the X or Y axis range so it is clear
          # where the data ends
          xbuff <- (xmax - xmin) * 0.05
          ybuff <- (ymax - ymin) * 0.05

          # now the actual coordinates for the "canvas" of the plot
          # can be derived from the set coordinates plus or minus
          # the buffer value
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
            size = "l",
            paste("The error '", err, "' arose while handling the bbox. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  rez <- reactive({ # assign resolution values on clipped and not clipped spatial data

    rez <- 0.1 # standard value, since it works for both tables

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

        # if it is the first time visualizing the plot and the column
        # names are those of the 0.01 degrees table, rez is 0.01
        # minimum rez is set to 0.01 too
        if (which_viz_event$origin) {
          if (isTRUE(all.equal(col_names_sdf, sf_column_100th))) {
            updateNumericInput(session,
              inputId = "map_rez",
              min = 0.01,
              max = 2,
              value = 0.01
            )
            rez <- 0.01

            # if it is the first time visualizing and the col names are
            # those of the 0.1 table, rez stays 0.1. Minimum rez is set
            # to 0.1 too
          } else if (isTRUE(all.equal(col_names_sdf, sf_column_10th))) {
            updateNumericInput(session,
              inputId = "map_rez",
              min = 0.1,
              max = 2,
              value = 0.1
            )

            rez <- 0.1
          }

          # if it is not the first time plotting, then adjust resolution
          # according to user input
        } else if (which_viz_event$reviz) {
          rez <- input$map_rez
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
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
              size = "l",
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
          if (summed != 0) {
            cumul_distr_length <- length.until(to_fill_column, summed)
          } else {
            cumul_distr_length <- nrow(grouped_df)
          }

          # subsetting grouped_df accordingly to cumul_distr_percent
          
          grouped_df <- grouped_df[c(1:cumul_distr_length), ]

          world_sf <- sf::st_as_sf( # world map to give some reference
            maps::map(
              "world",
              plot = FALSE,
              fill = TRUE
            )
          )

          #  check which global layers (EEZ, contiguous zone and national waters)
          # the user has decided to plot. Creates a variable for each of those
          layers_added <- input$add_layer

          if ("eez_boundaries_v11.gpkg" %in% layers_added) {
            world_eez <- st_read("./www/geo/eez_boundaries_v11.gpkg",
              layer = "eez_boundaries_v11",
              geometry_column = "geom"
            )
          }

          if ("eez_24nm_v3_boundaries.gpkg" %in% layers_added) {
            world_24nm <- st_read("./www/geo/eez_24nm_v3_boundaries.gpkg",
              layer = "eez_24nm_v3_boundaries",
              geometry_column = "geom"
            )
          }

          if ("eez_12nm_v3_boundaries.gpkg" %in% layers_added) {
            world_12nm <- st_read("./www/geo/eez_12nm_v3_boundaries.gpkg",
              layer = "eez_12nm_v3_boundaries",
              geometry_column = "geom"
            )
          }

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

          # checks which optional global layers exist in order to
          # add them to the plot function
          if (exists("world_eez")) {
            map <- map + geom_sf(
              data = world_eez,
              fill = "#BABABA",
              color = "#BABABA",
              size = 0.5
            )
          }

          if (exists("world_24nm")) {
            map <- map + geom_sf(
              data = world_24nm,
              fill = "#BABABA",
              color = "#BABABA",
              size = 0.5
            )
          }

          if (exists("world_12nm")) {
            map <- map + geom_sf(
              data = world_12nm,
              fill = "#BABABA",
              color = "#BABABA",
              size = 0.5
            )
          }

          map <- map + coord_sf(
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
            size = "l",
            paste("The error '", err, "' arose while producing the plot. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })

  # pdfview of the Handbook
  output$pdfview <- renderUI({
    tags$iframe(style = "height:90vh; width:100%; margin-top: 6px", src = "doc/Handbook.pdf")
  })
}
