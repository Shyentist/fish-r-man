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
  
  my_data <- eventReactive(input$filter_button, {
    
    table_name_ui <- input$table_name_ui
    
    if (table_name_ui == "Fishing effort at 100th degree"){
      
      summaries <- available_summaries_100th
      
    } else if (table_name_ui == "Fishing effort at 10th degree") {
      
      summaries <- available_summaries_10th
      
    }
    
    updatePrettyCheckboxGroup(
      session,
      inputId = 'summaries',
      choices = summaries
    )
    
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
    
    selectedData <- dbGetQuery(
      BQ_connection, 
      GLUED_SQL
    )
    
    output$queried_table <- renderDataTable(selectedData)
    
    enable(id = "download_button")
    
    metaData <- paste(
"Software by 'Buonomo Pasquale. [2021]. https://github.com/Shyentist/fish-r-man'

Data by 'Global Fishing Watch. [2021]. www.globalfishingwatch.org' (last checked: ", Sys.Date(),")

Retrieved from their public dataset on Google's BigQuery with the following query: 

",
          GLUED_SQL,
          sep = ""
        )
    
    output$download_button <- downloadHandler(
      filename = function() {
        paste(
          "data-", 
          Sys.Date(), 
          ".zip", 
          sep=""
        )
      },
      content = function (con){
        write.csv(
          selectedData, 
          "data.csv",
          row.names = FALSE
        )
        
        write.table(
          metaData, 
          "metadata.txt",
          row.names = FALSE,
          col.names = FALSE
        )
        
        utils::zip(
          con, 
          files = c(
            "data.csv",
            "metadata.txt"
          )
        )
        
      },
      contentType = "application/zip"
    )
    
    return(selectedData)
  }
  )
  
  output$queried_table <- renderDataTable(my_data())
  
  output$uploaded_csv_viz <- renderTable({head(my_data())})
  
  observeEvent(input$uploaded_csv, {
  
  output$uploaded_csv_viz <- renderTable({
    
    df <- read.csv(input$uploaded_csv$datapath,
                 header = TRUE,
                 sep = ",",
                 quote = '"')
    
    col_names_csv <- colnames(df)
    
    if (isTRUE(all.equal(col_names_csv,column_100th))){
      
      summaries <- available_summaries_100th
      
    } else if (isTRUE(all.equal(col_names_csv,column_10th))) {
      
      summaries <- available_summaries_10th
      
    }
    
    updatePrettyCheckboxGroup(
      session,
      inputId = 'summaries',
      choices = summaries
    )
    
    
    return(head(df))
    
    })})
  
  observeEvent(input$summarize_button, {
    
    choice <- input$summaries
    
    if (!is.null(choice)){
    
    df <- my_data()
    
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
        write.csv(summarized, file, row.names = F)
      }
      )
    
    enable(id = "download_analyses_button")
   
  })

  }
 