server <- function(input, output, session) {

  intro.message()

  observeEvent(input$table_name_ui, {
    
    table_name_ui <- input$table_name_ui

    # checks which table the user decided to query, and presents the checkboxes
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

  # based on chosen table and input values to query
  # returns an SQL query string
  
  SQL <- reactive({
    
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
    
    return(SQL)
    
  })
  
  # the SQL query string is rendered as text in the UI
  
  observe({
    
    SQL <- SQL()
    
    output$sql_query <- renderText({
      SQL
    })
    
  })
  
  # what is happening down here, with which_event, is basically a switch, that I 
  # can later check in reactive expressions that must differentiate among the 
  # various types of inputs that they can receive. For instance, both when 
  # uploading a csv or querying data, I need the final result to be assigned to 
  # 'my_data', so that later on I can just refer to this value, which is 
  # qualitatively the same for both sources
  
  which_event <- reactiveValues(
    query = FALSE, # activated when new data is queried
    csv = FALSE # activated when new data is uploaded via csv
  )
  
  observeEvent(input$filter_button, {
    which_event$query <- TRUE
    which_event$csv <- FALSE
  })
  
  observeEvent(input$uploaded_csv, {
    which_event$query <- FALSE
    which_event$csv <- TRUE
  })
  
  # I used the possibleInputs list to have reactive events start at any
  # change of the list, then check which of the previously mentioned
  # switches was turned on (TRUE)
  
  possibleInputs <- reactive({
    list(
      input$filter_button,
      input$uploaded_csv
    )
  })

  # this function takes different paths depending on the input. If "Filter" button 
  # is pressed, it runs the SQL Query created by the user and assigns the data
  # to "my_data", if a csv file is uploaded, it assigns the dataframe to "my_data"
  
  my_data <- eventReactive(possibleInputs(), {
    tryCatch(
      {
        df <- NULL
        
        shinyjs::hide(id = "map") # to avoid users messing with revisualisation, possibly skipping the checks

        updatePrettyCheckbox( # uploading a new csv or running a new query "overwrites"
          session = session, # the "clip" mode, so that the summaries and analyses shown
          inputId = "clip", # are always related to the latest loaded data
          value = FALSE
        )
        
        output$summary_preview <- renderDataTable({}) # uploading a new csv or running a new query also voids previous analyses
        
        disable(id = "download_analyses_button") # also, having voided the analyses table, we must also prevent download of previous analyses
        
        if (which_event$query) {
          
          showModal(
            modalDialog(
              size = "l",
              "Fishing for data...",
              footer = NULL
            )
          )
          
          output$queried_table <- renderDataTable({}) # empty the table before starting the function
          
          SQL <- SQL()
          
          # creating the query to count the number of rows the resulting table 
          # would have, so I can put a cap to avoid huge queries

          SQL_count <- count.sql(SQL)
         
          billing <- input$billing
          
          if (billing == ""){
            billing <- "fish-r-man" 
          }
          
          # billing defaults to fish-r-man to make the online experience more comfortable
          # as the user is not meant to access their google account nor input 
          # their billing project
          
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

          # and here is where the count from SQL_count is used
          # to check the number of rows the query would retrieve
          # against max_rows just above
          
          if (count$count_col <= max_rows) {

            df <- dbGetQuery(
              BQ_connection,
              SQL
            )

            output$queried_table <- renderDataTable(df) # now the table can be repopulated
            
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

          col_names_df <- colnames(df)

          # I just check for the col_names to know whether the file
          # is compatible. Someone could go out of their way to make
          # a csv with the same col_names and different types of data
          # to crash the app for the sake of it, but no one else should
          # have any issue
          
          if ((isFALSE(all.equal(col_names_df, column_100th))) && (isFALSE(all.equal(col_names_df, column_10th)))) {
            df <- NULL
          }

          removeModal()
        }
        
        sdf <- df
        
        if (!is.null(sdf)){

        # create a geom/geometry column via cell_ll_lat and cell_ll_lon
        sdf <- st_as_sf(
          df, 
          coords = c("cell_ll_lon", "cell_ll_lat"), 
          remove = FALSE # retains cell_ll_lat and cell_ll_lon columns
          )
        
        # make sure to set the CRS, which is a very important check later on (when dealing with the area of interest)
        
        st_crs(sdf) <- 4326 
        
        }
        
        return(sdf)
      
        },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
            paste("The error '", err, "' arose while loading the dataframe. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      })
  })
  
  # download buttons for my_data are managed down here
  
  observe({
    
    tryCatch(
      {
    
    sdf <- my_data()
    
    # if my_data is not empty and has at least 1 row, turn it back into a
    # regular dataframe, rather than sf object, and assign it to the csv download
    # while the sf object is assigned to the gpkg download

    if ((!is.null(sdf)) && (length(sdf$geometry) > 0)) {
      
      df <- as.data.frame(sdf) %>%
        select(-c(geometry))
      
    output$download_button_gpkg <- downloadHandler(
      filename = function() {
        paste(
          "spatial-data-",
          Sys.Date(),
          ".gpkg",
          sep = ""
        )
      },
      content = function(file) {
        
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
      })
    
    output$download_button_csv <- downloadHandler(
      filename = function() {
        paste(
          "data-",
          Sys.Date(),
          ".csv",
          sep = ""
        )
      },
      content = function(file) {
        write.csv(
          df,
          file,
          row.names = F
        )
      }
    )}
    },
    error = function(err) {
      showModal(
        modalDialog(
          size = "l",
          paste("The error '", err, "' arose while preparing the downloadable files. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
        )
      )
    })
  })
  
  # function to enable/disable/update inputs related to my_data/clipped_data
  # because I don't want inputs available to the user if that could break the app
  
  # all inputs relying on sdf are enabled/disabled here depending on the fact
  # that my_data exists and has at least one point
  
  observe({
    tryCatch(
      {
       sdf <- my_data()
        
        if ((!is.null(sdf)) && (length(sdf$geometry) > 0)) {
          enable(id = "visualize_button")
          enable(id = "area_of_interest")
          enable(id = "re_visualize_button")
          enable(id = "download_button_csv")
          enable(id = "download_button_gpkg")
          enable(id = "summarize_button")
        } else {
          disable(id = "visualize_button")
          disable(id = "area_of_interest")
          disable(id = "re_visualize_button")
          disable(id = "download_button_csv")
          disable(id = "download_button_gpkg")
          disable(id = "summarize_button")
        }
      },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
            paste("The error '", err, "' arose while enabling/disabling inputs. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      }
    )
  })
  
  # check the column names for my_data (minus the geometry column) and updates
  # the available summaries in Descriptive Analyses" tab accordingly

  observe({
    tryCatch(
      {
        col_names_df <- colnames(my_data())
        
        col_names_df <- col_names_df[! col_names_df %in% c('geometry')]

        if (isTRUE(all.equal(col_names_df, column_100th))) {
          
          summaries <- available_summaries_100th

        } else if (isTRUE(all.equal(col_names_df, column_10th))) {
          
          summaries <- available_summaries_10th
          
        } else {
          
          summaries <- NULL
          
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
          whether_to_clip <- input$clip # whether to use my_data or clipped_data for the analyses

          if (whether_to_clip) {
            df <- as.data.frame(clipped_data())%>%
              select(-c(geometry))
          } else {
            df <- as.data.frame(my_data()) %>%
              select(-c(geometry))
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
              gfw.summarize()
            
          } else {

            # if no choice is made for aggregation, the entire dataframe is summarized
            summarized <- gfw.summarize(df)
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
          
          # the download file is also created here and the button is enabled

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

  # function to update list for selection of layer for clipping
  # it is the first check to make the process as safe as possible
  
  observeEvent(input$area_of_interest, {
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
        layers <- st_layers(input$area_of_interest$datapath)

        layers_name <- layers$name

        updateSelectInput(
          inputId = "area_of_interest_layer",
          choices = layers_name
        )

        enable(id = "area_of_interest_layer")

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
  observeEvent(input$area_of_interest_layer, {
    tryCatch(
      {
        chosen_layer <- input$area_of_interest_layer

        dsn <- input$area_of_interest$datapath

        sdf <- my_data()

        if ((!is.null(dsn)) && (!is.na(dsn))) {
          area_of_interest <- st_read(
            dsn = dsn,
            layer = chosen_layer
          )

          geom_type <- st_geometry(area_of_interest) %>%
            class()

          if (("sfc_POLYGON" %in% geom_type) || ("sfc_MULTIPOLYGON" %in% geom_type)) {
            if (st_crs(sdf) == st_crs(area_of_interest)) {
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
  clipped_data <- eventReactive(input$clip, {
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

          chosen_layer <- input$area_of_interest_layer

          dsn <- input$area_of_interest$datapath

          points <- my_data()

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
  
  # if the clipping is successful in every step, the input$clip checkbox stays
  # checked (TRUE), and can be used to know whether clipped_data exists and whether
  # the user would like to analyze that instead of my_data. Also, while it is
  # TRUE, the download csv and download gpkg buttons are enabled.
  
  observe({
    
    whether_to_clip <- input$clip 
    
    if (whether_to_clip){
      
      enable(id = "download_button_csv_clipped")
      enable(id = "download_button_gpkg_clipped")
      
    } else { 
      
      disable(id = "download_button_csv_clipped")
      disable(id = "download_button_gpkg_clipped")
      
    }
    
  })
  
  # here we prepare the files for the download buttons for the clipped_data
  
  observe({
    
    tryCatch(
      {
        sdf <- clipped_data()
        
        if ((!is.null(sdf)) && (length(sdf$geometry) > 0)) {
          
          df <- as.data.frame(sdf) %>%
            select(-c(geometry))
          
          output$download_button_gpkg_clipped <- downloadHandler(
            filename = function() {
              paste(
                "clipped-spatial-data-",
                Sys.Date(),
                ".gpkg",
                sep = ""
              )
            },
            content = function(file) {
              
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
            })
          
          output$download_button_csv_clipped <- downloadHandler(
            filename = function() {
              paste(
                "clipped-data-",
                Sys.Date(),
                ".csv",
                sep = ""
              )
            },
            content = function(file) {
              write.csv(
                df,
                file,
                row.names = F
              )
            }
          )}
      },
      error = function(err) {
        showModal(
          modalDialog(
            size = "l",
            paste("The error '", err, "' arose while preparing the downloadable files for the area of interest. Please be sure to follow the documentation. If the problem persists, contact the developer(s) for assistance (contacts below).")
          )
        )
      })
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
          sdf <- clipped_data()
        } else {
          sdf <- my_data()
        }

        # first, just use the bbox boundaries to set the coords for the plot
        if ((!is.null(sdf)) && (length(sdf$geometry) > 0)) {
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
          sdf <- clipped_data()
        } else {
          sdf <- my_data()
        }

        col_names_sdf <- colnames(sdf)

        col_names_sdf <- col_names_sdf[! col_names_sdf %in% c('geometry')]

        # if it is the first time visualizing the plot and the column
        # names are those of the 0.01 degrees table, rez is 0.01
        # minimum rez is set to 0.01 too
        if (which_viz_event$origin) {
          if (isTRUE(all.equal(col_names_sdf, column_100th))) {
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
          } else if (isTRUE(all.equal(col_names_sdf, column_10th))) {
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
            df <- clipped_data()
          } else {
            df <- my_data()
          }

          col_names_df <- colnames(df)
          
          col_names_df <- col_names_df[! col_names_df %in% c('geometry')]

          rez <- rez()

          # if statements to reaggregate data according to rez
          if (isTRUE(all.equal(col_names_df, column_100th))) {
            
            if (is.numeric(rez) && rez > 0.01 && rez <= 2) {
              df <- df %>%
                mutate(
                  cell_ll_lat = floor(cell_ll_lat / rez) * rez + 0.5 * rez,
                  cell_ll_lon = floor(cell_ll_lon / rez) * rez + 0.5 * rez
                )
            } # this sets new lat and lon for the new aggregation
            
            # aggregation starts here
            grouped_df <- df %>%
              as.data.frame() %>%
              group_by(cell_ll_lon, cell_ll_lat) %>%
              summarise(
                "Total fishing hours" = sum(fishing_hours),
                "Total hours" = sum(hours),
                "Total MMSI present" = sum(mmsi_present),
                "Mean fishing hours" = mean(fishing_hours),
                "Mean hours" = mean(hours),
                "Mean MMSI present" = mean(mmsi_present)
              )
          } else if (isTRUE(all.equal(col_names_df, column_10th))) {
            if (is.numeric(rez) && rez > 0.1 && rez <= 2) {
              df <- df %>%
                mutate(
                  cell_ll_lat = floor(cell_ll_lat / rez) * rez + 0.5 * rez,
                  cell_ll_lon = floor(cell_ll_lon / rez) * rez + 0.5 * rez
                )
            }

            grouped_df <- df %>%
              as.data.frame() %>%
              group_by(cell_ll_lon, cell_ll_lat) %>%
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

            col_names_grouped_no_geo <- col_names_grouped[!col_names_grouped %in% c("cell_ll_lat", "cell_ll_lon")]

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
            geom_tile(data = grouped_df, aes(x = cell_ll_lon, y = cell_ll_lat, fill = .data[[to_fill]])) +
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
