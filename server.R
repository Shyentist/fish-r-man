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
  
  observeEvent(input$filter_button, {
    
    table_name_ui <- input$table_name_ui
    
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
    
    for (field in checked_boxes){
      
      if (field == "date" || field == "flag" || field == "geartype"){ 
        
        next } else { 
          
          first_field <- input[[field]][1]
          second_field <- input[[field]][2]
          
          if (
            
            (!is.null(first_field) && !is.na(first_field)) && 
            (!is.null(second_field) && !is.na(second_field))) {
            
            next_SQL <- sprintf(
              "AND %s >= {input$%s[1]} AND %s < {input$%s[2]}", 
              field, 
              field, 
              field, 
              field
            )
            
            SQL <- paste(
              SQL, 
              next_SQL, 
              sep = " "
            )}}
    }
    
    flags <- input$flags
    
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
    
    retrieved_data <- dbGetQuery(
      BQ_connection, 
      GLUED_SQL
    )
    
    output$queried_table <- renderDataTable(retrieved_data)
    
    enable(id = "download_button")
    
    selectedData <- reactive(
      {
        retrieved_data
      }
    )
    
    metaData <- reactive(
      {
        paste(
"Software by 'Buonomo Pasquale. [2020]. https://github.com/Shyentist/fish-r-man'

Data by 'Global Fishing Watch. [2020]. www.globalfishingwatch.org' (last checked: ", Sys.Date(),")

Retrieved from their public dataset on Google's BigQuery with the following query: 

",
          GLUED_SQL,
          sep = ""
        )
      }
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
          selectedData(), 
          "data.csv",
          row.names = FALSE
        )
        
        write.table(
          metaData(), 
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
  }
  )
}