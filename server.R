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
    
    updatePrettyCheckboxGroup( #this way, the checkboxes are always named after the table fields
      session,
      inputId = 'filter_columns_ui',
      choices = fields_list
    )
  }) 
  
  #what follow enables and disables the input boxes from the "filter sidebar"
  #I do this so that I can later check on which boxes are ticked, instead of
  #checking directly for the inputs, and I can build the SQL Query based on
  #which inputs are enabled. The logic is simple, for inputs of type numeric range
  #the query adds AND, as in "WHERE cell_ll_lat > 10 AND cell_ll_lat < 20 ", since the
  #two things must be true at the same time. For categories, such as flag and
  #geartype, I add OR, as in " flag = 'ITA' OR flag = 'FRA' ", since we are 
  #looking for entries that much any of these categories
  
  #for MMSI, the SQL created is a simple 'like', but one can use the '%'
  #character to search things starting/ending/containing the input
  
  #The query starts without a WHERE, only AND, then, I substitute the first AND
  #of the query with a WHERE. I am sure someone else can come up with something
  #more elegant, but I figured this was the best option at the time of coding
  
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
  
  #what is happening down here, with which_event, you will see it often in this
  #code. It is basically a switch, that I can later check in reactive expressions
  #that must differentiate among the various type of inputs that they can receive
  #for instance, both when uploading a csv or querying data, I need the final 
  #result to be assigned to 'my_data', so that later on I can just refer to this
  #value, which is qualitatively the same for both sources
  
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
    
    checked_boxes <- input$filter_columns_ui
    
    table_name_ui <- input$table_name_ui
    
    if ((!is.null(first_date) && !is.na(first_date)) && (!is.null(second_date)&& !is.na(second_date))) {
      
      date_SQL <- "AND date >= {first_date} AND date < {second_date}"
      
      SQL <- paste(
        SQL,
        date_SQL,
        sep = " "
      )
    }
    
    for (field in checked_boxes){
      
      if (field == "date" || field == "flag" || field == "geartype" || field == "mmsi"){ 
        
        next } else { 
          
          first_field <- input[[field]][1]
          second_field <- input[[field]][2]
          
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
            )}}
    }
    
    mmsi <- input$mmsi
    
    if ("mmsi" %in% checked_boxes) {
      
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
    
    if (!is.null(gears) && !is.na(gears) && ("geartype" %in% checked_boxes)){
      
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
  
  output$uploaded_csv_viz <- renderTable({ #renderTable must have had an update 
    
    df <- my_data() #that is messing with the date format and, in an attempt to
    
    if (length(df$date) != 0){ #solve that, even with the header, so this is my
    
    header <- head(my_data()) #solution for now
    
    header$date <- as.character(as.Date(header$date, "%Y-%m-%d"))
    
    return(header)}
    
    })

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
    
    columns_to_append <- c("Total fishing", #this is to limit the amount of fields
                           "Mean fishing", #displayed on screen, since one could
                           "Total hours", #group by every single field and obtain
                           "Mean hours") #over 20 columns
    
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
    
    sdf <- st_as_sf(df,
                    coords = c("cell_ll_lon",
                               "cell_ll_lat")
                    )
    
    colnames(sdf)[colnames(sdf) == "geometry"] <- "geom" #this happens because 
                                              #st_write creates a column "geom",
                                      #but st_as_sf names it "geometry", messing
                                            #with my system of checking colnames
    
    st_geometry(sdf) <- "geom" #otherwise the sf object would still look for "geometry" for its geometry
    
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
      
      
      if (isFALSE(all.equal(col_names_gpkg,sf_column_100th)) && isFALSE(all.equal(col_names_gpkg,sf_column_10th))){
        
        sdf <- NULL
        
      }}
    
    
    
    if ((!is.null(sdf)) && (length(sdf$geom) > 0)) {
      
      enable(id = "download_gpkg_button")
      enable(id = "visualize_button")
      enable(id = "second_uploaded_gpkg")
    
      st_crs(sdf) <- 4326
      
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
  
  observeEvent(input$second_uploaded_gpkg, {
    
    showModal(
      modalDialog(
        "Uploading GeoPackage...",
        footer=NULL
      )
    )
    
    layers <- st_layers(input$second_uploaded_gpkg$datapath)
    
    layers_name <- layers$name
    
    enable(id = "second_gpkg_layer")
    
    updateSelectInput(inputId = "second_gpkg_layer",
                      choices = layers_name)
    
    removeModal()
 
  })
  
  observeEvent(input$second_gpkg_layer, {
    
    chosen_layer <- input$second_gpkg_layer
    
    dsn <- input$second_uploaded_gpkg$datapath
    
    if ((!is.null(dsn)) && (!is.na(dsn))){
      
      area_of_interest <- st_read(
        dsn = dsn,
        layer = chosen_layer)
      
      geom_type <- st_geometry(area_of_interest) %>%
        class()
      
      if (("sfc_POLYGON" %in% geom_type) || ("sfc_MULTIPOLYGON" %in% geom_type)){
        
        if (st_crs(sf_data()) == st_crs(area_of_interest)){
        
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
            "Geometry type is invalid. Please upload a file with geometry type 'POLYGON' or 'MULTIPOLYGON'"
          )
        )}
      
    }
    
  })
  
  clipped_sf_data <- eventReactive(input$clip, {
    
    whether_to_clip <- input$clip
    
    if (whether_to_clip){
    
    chosen_layer <- input$second_gpkg_layer
    
    dsn <- input$second_uploaded_gpkg$datapath
    
    points <- sf_data()
    
    area_of_interest <- st_read(
      dsn = dsn,
      layer = chosen_layer)
    
    clipped_points <- st_intersection(points, area_of_interest)
    
    if (length(clipped_points$geom) != 0){
      
      clipped_points <- subset(clipped_points, select = -c(ID) )
      
      return(clipped_points)
      
    } else {
        
      showModal(
        modalDialog(
          "The two datasets do not intersect."
        )
      )
      
      return(NULL)
      
      }
    
    }
  })
  
  observe(clipped_sf_data())
  
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
      
    if (isTRUE(input$clip) && !is.null(clipped_sf_data())) {
      
      sdf <- clipped_sf_data()
      
    } else {
        
      sdf <- sf_data()
      
      } 
      
    colnames(sdf)[colnames(sdf) == "geometry"] <- "geom" 
    
    st_geometry(sdf) <- "geom"
    
    col_names_sdf <- colnames(sdf)
    
    #what happens here with bbox is so that the plot can "zoom in" on the area
    #of interest, with a padding of 5% of the x and y range on either side of
    #the range
    
    bbox <- st_bbox(sdf)
    
    xbuff <- (bbox$xmax - bbox$xmin)*0.05
    ybuff <- (bbox$ymax - bbox$ymin)*0.05
    
    lowx <- bbox$xmin - xbuff
    highx <- bbox$xmax + xbuff
    lowy <- bbox$ymin - ybuff 
    highy <- bbox$ymax + ybuff

    df <- sdf %>%
      dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                    lat = sf::st_coordinates(.)[,2]) #lat and lon are easier to work with than "geom"
    
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
          lon = floor(lon/rez) * rez + 0.5 * rez)} #this sets new lat and lon for the new aggregation
      
      grouped_df <- df %>% #aggregation starts here
        
        as.data.frame() %>%
        
        group_by(lon, lat) %>%
        
        summarise("Total fishing hours" = sum(fishing_hours),
                  "Total hours" = sum(hours),
                  "Total MMSI present" = sum(mmsi_present),
                  "Mean fishing hours" = mean(fishing_hours),
                  "Mean hours" = mean(hours),
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
                  "Total hours" = sum(hours),
                  "Mean fishing hours" = mean(fishing_hours),
                  "Mean hours" = mean(hours))
    
    }
    
    if (which_viz_event$origin) {col_names_grouped <- colnames(grouped_df)
    
    col_names_grouped_no_geo <- col_names_grouped[! col_names_grouped %in% c("lat", "lon")]
    
    updateSelectInput(session,
                      inputId = "mapped_column",
                      choices = col_names_grouped_no_geo)
    
    to_fill <- "Total fishing hours"} else {to_fill <- input$mapped_column} #basically, if it's the first plot, it defaults to "Total fishing hours" to fill, otherwise, it is the chosen field
    
    world_sf <- sf::st_as_sf( #world map to give some reference 
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
      
      coord_sf(xlim = c(lowx, highx), #these are the zoom in coordinates
               ylim = c(lowy, highy), #I mentioned earlier
               expand = FALSE)
    
    output$viz_map <- renderPlot({return(map)})
    
    shinyjs::show("map") #the map output is always present on screen, it is only
    #shown when in use in order to have a cleaner UI
    
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
 