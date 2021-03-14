server <- function(input,output,session) {
  
  observeEvent(input$table_name_ui,{
    
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
    
    updatePrettyCheckboxGroup(
      session,
      inputId = 'filter_columns_ui',
      choices = fields_list
    )
  }) 
  
  observeEvent(input$filter_columns_ui,{
    
    for (field in list_togglable_ids) {
      
      if (field %in% input$filter_columns_ui) {
        
        enable(id = field)
        
      } else {
        
        disable (id = field)
      }
      
    }
    
  },
  ignoreNULL = FALSE)
  
  which_event <- reactiveValues(
    query = FALSE, 
    csv = FALSE
    )
  
  observeEvent(input$filter_button, {
    
    which_event$query <- TRUE
    which_event$csv <- FALSE
    
  })
  
  observeEvent(input$uploaded_csv, {
    
    which_event$query <- FALSE
    which_event$csv <- TRUE
    
  })
  
  possibleInputs <- reactive({
    
    list(
      input$filter_button,
      input$uploaded_csv
    )
    
  })

  my_data <- eventReactive(possibleInputs(), {
    
    summaries <- NULL
    
    disable(id = "convert_to_spatial_button")
    disable(id = "summarize_button")
    
    if (which_event$query){
    
    showModal(
      modalDialog(
        "Constructing SQL Query...", 
        footer=NULL
        )
      )
    
    output$queried_table <- renderDataTable({})
    
    table_name_ui <- input$table_name_ui
    
    if (table_name_ui == "Fishing effort at 100th degree"){
      
      summaries <- available_summaries_100th
      
    } else if (table_name_ui == "Fishing effort at 10th degree") {
      
      summaries <- available_summaries_10th
      
    }
    
    table_full_name <- paste(
      project, 
      dataset, 
      tables_list[match(
        table_name_ui,
        tables_list_ui)], 
      sep = "."
    )
    
    SQL <- "SELECT * FROM {`table_full_name`}"
    
    first_date <- input$date[1]
    second_date <- input$date[2]
    
    if ((!is.null(first_date) && !is.na(first_date)) && (!is.null(second_date)&& !is.na(second_date))) {
      
      date_SQL <- "AND _PARTITIONTIME >= {first_date} AND _PARTITIONTIME < {second_date}"
      
      SQL <- paste(
        SQL,
        date_SQL,
        sep = " "
      )
    }
    
    checked_boxes <- input$filter_columns_ui
    
    table_name_ui <- input$table_name_ui
    
    for (field in checked_boxes){
      
      if (field == "date" || field == "flag" || field == "geartype"){ 
        
        next } else { 
          
          first_field <- input[[field]][1]
          second_field <- input[[field]][2]
          
          if (
            
            (!is.null(first_field) && !is.na(first_field)) && 
            (!is.null(second_field) && !is.na(second_field))) {
            
            if (
              
              (field == "lat_bin" || field == "lon_bin") && 
              (table_name_ui == "Fishing effort at 10th degree")) {
              
              first_field <- first_field*10
              second_field <- second_field*10
              
            } else if (
              
              (field == "lat_bin" || field == "lon_bin") && 
              (table_name_ui == "Fishing effort at 100th degree")) {
              
              first_field <- first_field*100
              second_field <- second_field*100
              
            } 
            
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
            )}}
    }
    
    flags <- input$flag
    
    if (!is.null(flags) && !is.na(flags)) {
      
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
        ' OR', 
        ')'
      )
      
      SQL <- paste(
        SQL, 
        flag_SQL, 
        sep = " "
      )
    }
    
    gears <- input$geartype
    
    if (!is.null(gears) && !is.na(gears)){
      
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
        ' OR', 
        ')'
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
    
    GLUED_SQL <- glue_sql(
      SQL,
      .con = BQ_connection
    )
    
    output$sql_query <- renderText({GLUED_SQL})
    
    showModal(
      modalDialog(
        "Fishing for data...", 
        footer=NULL)
      )
    
    df <- dbGetQuery(
      BQ_connection, 
      GLUED_SQL
    )
    
    showModal(
      modalDialog(
        "Building table...",
        footer=NULL
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
          sep=""
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
    
    if (table_name_ui == "Fishing effort at 100th degree"){
    
    summaries <- available_summaries_100th
    
    } else if (table_name_ui == "Fishing effort at 10th degree"){
      
    summaries <- available_summaries_10th
      
    }
    
    removeModal()
    
    } else if (which_event$csv){
      
      showModal(
        modalDialog(
          "Uploading your data...",
          footer=NULL
          )
        )
    
      df <- read.csv(input$uploaded_csv$datapath,
                     header = TRUE,
                     sep = ",",
                     quote = '"')
      
      col_names_csv <- colnames(df)
      
      if (isTRUE(all.equal(col_names_csv,column_100th))){
        
        summaries <- available_summaries_100th
        
      } else if (isTRUE(all.equal(col_names_csv,column_10th))) {
        
        summaries <- available_summaries_10th
        
      } else {
        
        df <- my_data()
        
      }
      
      removeModal()
      
      } else { df <- NULL }
    
    updatePrettyCheckboxGroup(
      session,
      inputId = 'summaries',
      choices = summaries
    )
    
    if (!is.null(df)){
      
      enable(id = "convert_to_spatial_button")
      enable(id = "summarize_button")
      
    }
    
    return(df)
    }
  )
  
  observe(my_data())
  
  output$uploaded_csv_viz <- renderTable({head(my_data())})

  observeEvent(input$summarize_button, {
    
    showModal(
      modalDialog(
        "Summarizing...", 
        footer=NULL)
      )
    
    choice <- input$summaries
    
    if (length(choice) < 8){
    
    df <- my_data()
    
    if (!is.null(choice)){
    
    
    if ("month" %in% choice) {
      df$month <- substr(
        df$date, 
        start = 1, 
        stop = 7
      )
        }
      
    if ("year" %in% choice) {
      df$year <- substr(
        df$date, 
        start = 1, 
        stop = 4
      )
    }
    
    summarized <- group_by_at(df, vars(one_of(choice))) %>%
      summarize("Total fishing" = sum(fishing_hours),
                "Min. fishing" = min(fishing_hours),
                "1st Qu. fishing" = quantile(fishing_hours, 0.25),
                "Median fishing" = median(fishing_hours),
                "Mean fishing" = mean(fishing_hours), 
                "3rd Qu. fishing" = quantile(fishing_hours, 0.75),
                "Max. fishing" = max(fishing_hours),
                "Total vessel" = sum(vessel_hours),
                "Min. vessel" = min(vessel_hours),
                "1st Qu. vessel" = quantile(vessel_hours, 0.25),
                "Median vessel" = median(vessel_hours), 
                "Mean vessel" = mean(vessel_hours), 
                "3rd Qu. vessel" = quantile(vessel_hours, 0.75),
                "Max. vessel" = max(vessel_hours)
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
                                 "Total vessel" = sum(vessel_hours),
                                 "Min. vessel" = min(vessel_hours),
                                 "1st Qu. vessel" = quantile(vessel_hours, 0.25),
                                 "Median vessel" = median(vessel_hours), 
                                 "Mean vessel" = mean(vessel_hours), 
                                 "3rd Qu. vessel" = quantile(vessel_hours, 0.75),
                                 "Max. vessel" = max(vessel_hours)
      )
    }
    
    columns_to_append <- c("Total fishing",
                           "Mean fishing",
                           "Total vessel",
                           "Mean vessel")
    
    columns_to_show <- append(choice, columns_to_append)
    
    output$summary_preview <- renderDataTable ({summarized[,columns_to_show]})
    
    output$download_analyses_button <- downloadHandler(
      
      filename = function() {
        paste(
          "summary-",
          Sys.Date(), 
          ".csv", 
          sep=""
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
    
    removeModal()} else {
      showModal(
      modalDialog(
        "Maximum 7 fields")
    )}
   
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
    
    disable(id = "visualize_button")
    disable(id = "download_gpkg_button")
    
    if (which_sf_event$converted){
    
    showModal(
      modalDialog(
        "Converting data to GeoPackage...",
        footer=NULL
        )
      )
    
    df <- my_data()
    
    col_names_conv <- colnames(df)
    
    if (isTRUE(all.equal(col_names_conv,column_100th))){
      
      rez <- 100
      
    } else if (isTRUE(all.equal(col_names_conv,column_10th))) {
      
      rez <- 10
      
    }
    
    df$lat_bin <- df$lat_bin/rez
    df$lon_bin <- df$lon_bin/rez
    
    sdf <- st_as_sf(df,
                    coords = c("lon_bin",
                               "lat_bin")
                    )
    
    } else if (which_sf_event$gpkg){
      
      showModal(
        modalDialog(
          "Uploading GeoPackage...",
          footer=NULL
        )
      )
      
      
      sdf <- st_read(input$uploaded_gpkg$datapath,
              layer = "GFW",
              geometry_column = "geom")
      
      col_names_gpkg <- colnames(sdf)
      
      
      if (isFALSE(all.equal(col_names_gpkg,sf_column_100th)) & isFALSE(all.equal(col_names_gpkg,sf_column_10th))){
        
        sdf <- NULL
        
      }}
    
    
    
    if ((!is.null(sdf)) && (length(sdf$geom) > 0)) {
      
      enable(id = "download_gpkg_button")
      enable(id = "visualize_button")
      
    }
    
    removeModal()
    
    return(sdf)
  })
                           
  observe(sf_data())
  
  output$download_gpkg_button <- downloadHandler(
    
    filename = function() {
      paste(
        "spatial-data-",
        Sys.Date(), 
        ".gpkg", 
        sep=""
      )
    },
    content = function(file) {
      
      sdf <- sf_data()
      
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
  
  observeEvent(possibleVizInputs(), {
    
    if (which_viz_event$origin || which_viz_event$reviz){
    
    showModal(
      modalDialog(
        "Visualizing your data...",
        footer=NULL
      )
    )
    
    sdf <- sf_data()
    
    colnames(sdf)[colnames(sdf) == "geometry"] <- "geom" 
    
    st_geometry(sdf) <- "geom"
    
    col_names_sdf <- colnames(sdf)
    
    bbox <- st_bbox(sdf)
    
    lowx <- bbox$xmin - 1
    highx <- bbox$xmax + 1
    lowy <- bbox$ymin - 1 
    highy <- bbox$ymax + 1

    df <- sdf %>%
      dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                    lat = sf::st_coordinates(.)[,2])
    
    if (isTRUE(all.equal(col_names_sdf,sf_column_100th))){
      
      if (which_viz_event$origin){
        
        updateNumericInput(session,
                           inputId = "map_rez",
                           min = 0.01,
                           max = 1,
                           value = 0.01)
        rez <- 0.01
        
      } else {rez <- input$map_rez}
      
      if (is.numeric(rez) && rez > 0.01 && rez <= 1) {
        df <- df %>% 
        mutate(
          lat = floor(lat/rez) * rez + 0.5 * rez, 
          lon = floor(lon/rez) * rez + 0.5 * rez)}
      
      grouped_df <- df %>%
        
        as.data.frame() %>%
        
        group_by(lon, lat) %>%
        
        summarise("Total fishing hours" = sum(fishing_hours),
                  "Total vessel hours" = sum(vessel_hours),
                  "Total MMSI present" = sum(mmsi_present),
                  "Mean fishing hours" = mean(fishing_hours),
                  "Mean vessel hours" = mean(vessel_hours),
                  "Mean MMSI present" = mean(mmsi_present))
 
    } else if (isTRUE(all.equal(col_names_sdf,sf_column_10th))) {
      
      if (which_viz_event$origin){
        
        updateNumericInput(session,
                           inputId = "map_rez",
                           min = 0.1,
                           max = 1,
                           value = 0.1)
        
        rez <- 0.1
       
      } else {rez <- input$map_rez}
      
      if (is.numeric(rez) && rez > 0.1 && rez <= 1) {df <- df %>% 
        mutate(
          lat = floor(lat/rez) * rez + 0.5 * rez, 
          lon = floor(lon/rez) * rez + 0.5 * rez)}
      
      grouped_df <- df %>%
        
        as.data.frame() %>%
        
        group_by(lon, lat) %>%
        
        summarise("Total fishing hours" = sum(fishing_hours),
                  "Mean fishing hours" = mean(fishing_hours))
    
    }
    
    if (which_viz_event$origin) {col_names_grouped <- colnames(grouped_df)
    
    col_names_grouped_no_geo <- col_names_grouped[! col_names_grouped %in% c("lat", "lon")]
    
    updateSelectInput(session,
                      inputId = "mapped_column",
                      choices = col_names_grouped_no_geo)
    
    to_fill <- "Total fishing hours"} else {to_fill <- input$mapped_column}
    
    world_sf <- sf::st_as_sf(
      maps::map(
        "world", 
        plot = FALSE, 
        fill = TRUE
      )
    )
    
    map <- ggplot() +
      
      geom_tile(data = grouped_df, aes(x = lon, y = lat, fill = .data[[to_fill]])) +
      
      scale_fill_viridis() +
      
      geom_sf(data = world_sf, 
              fill = '#BABABA', 
              color = '#0A1738',
              size = 0.1) +
      
      coord_sf(xlim = c(lowx, highx),
               ylim = c(lowy, highy),
               expand = FALSE)
    
    output$viz_map <- renderPlot({return(map)})
    
    shinyjs::show("map")
    
    output$download_map_button <- downloadHandler(
      
      filename = function() {
        paste(
          to_fill,
          Sys.Date(), 
          ".png", 
          sep=""
        )
      },
      
      content = function(file) {
        
        ggsave(map, filename = file)
        
      }
    )
    
    removeModal()}
    
    })

  }
 