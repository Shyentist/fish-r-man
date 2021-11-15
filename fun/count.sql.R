# function that returns, from an SQL query, the query to count the number of 
# rows that the resulting table will have. It is useful to check on the amount 
# of data before requesting them from the server

count.sql <- function(SQL) {
  
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
  
  return(SQL_count)
  
}