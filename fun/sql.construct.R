# The logic for the SQL constructor is simple: every query starts with
# SELECT * FROM {'table'}. For inputs of type numeric range
# the query adds AND, as in "WHERE cell_ll_lat >= 10 AND cell_ll_lat < 20 ", since the
# two things must be true at the same time. For categories, such as flag and
# geartype, I add OR, as in " flag = 'ITA' OR flag = 'FRA' ", since we are
# looking for entries that match any of these categories

# for MMSI, the SQL created is a simple 'like', but one can use the '%'
# character to search things starting/ending/containing the input

# The query starts without a WHERE, only AND, then, I substitute the first AND
# of the query with a WHERE. I am sure someone else can come up with something
# more elegant, but I figured this was the best option at the time of coding

sql.construct <- function(table = NULL, date = NULL, cell_ll_lat = NULL, cell_ll_lon = NULL, hours = NULL, fishing_hours = NULL, mmsi_present = NULL, flag = NULL, geartype = NULL, mmsi = NULL) {
  SQL <- sprintf("SELECT * FROM `%s`", table)
  
  if (all(!is.na(date)) && all(!is.null(date))) {
    lower_end <- min(date)
    upper_end <- max(date)
    
    date_SQL <- sprintf(
      "AND date >= '%s' AND date < '%s'",
      lower_end,
      upper_end
    )
    
    SQL <- paste(
      SQL,
      date_SQL,
      sep = " "
    )
  }
  
  range_inputs <- list(cell_ll_lat, cell_ll_lon, hours, fishing_hours, mmsi_present)
  
  names(range_inputs) <- c("cell_ll_lat", "cell_ll_lon", "hours", "fishing_hours", "mmsi_present")
  
  list_inputs <- list(flag, geartype)
  
  names(list_inputs) <- c("flag", "geartype")
  
  for (range_name in names(range_inputs)) {
    range_values <- range_inputs[range_name] %>%
      unlist(recursive = TRUE, use.names = TRUE)
    
    if (all(!is.null(range_values)) && all(!is.na(range_values))) {
      lower_end <- min(range_values)
      upper_end <- max(range_values)
      
      ranges_SQL <- sprintf(
        "AND %s >= %s AND %s < %s",
        range_name,
        lower_end,
        range_name,
        upper_end
      )
      
      SQL <- paste(
        SQL,
        ranges_SQL,
        sep = " "
      )
    }
  }
  
  if (!is.null(mmsi) && !is.na(mmsi)) {
    if (mmsi != "") {
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
  }
  
  for (list_name in names(list_inputs)) {
    list_values <- list_inputs[list_name]
    
    list_values <- unlist(list_values, recursive = TRUE, use.names = TRUE)
    
    if (all(!is.null(list_values)) && all(!is.na(list_values))) {
      next_list_SQL <- "AND ("
      
      for (element in list_values) {
        next_element_SQL <- sprintf(
          "%s = '%s' OR ",
          list_name,
          element
        )
        
        next_list_SQL <- paste(
          next_list_SQL,
          next_element_SQL,
          sep = ""
        )
      }
      
      next_list_SQL <- stri_replace_last_fixed(
        next_list_SQL,
        " OR",
        ")"
      )
      
      SQL <- paste(
        SQL,
        next_list_SQL,
        sep = " "
      )
    }
  }
  
  SQL <- sub(
    "AND",
    "WHERE",
    SQL
  )
  
  return(SQL)
}