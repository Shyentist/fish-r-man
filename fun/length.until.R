# this is a function to count the length of a vector necessary until it reaches an amount
# used for cumulative distribution
## source: https://stackoverflow.com/questions/8540143/add-consecutive-elements-of-a-vector-until-a-value

length.until <- function(x, max = 10) {
  s <- 0
  len <- length(x) # modified this to avoid function looping forever in case of bad
  # rounding or similar minor issues. This way, length is, at most, equal to the entire dataset
  start <- 1
  j <- 1
  for (i in seq_along(x)) {
    s <- s + x[i]
    while (s >= max) {
      if (i - j + 1 < len) {
        len <- i - j + 1
        start <- j
      }
      s <- s - x[j]
      j <- j + 1
    }
  }
  
  # commented the list below because we only need len
  # list(start = start, length = len)
  
  len
}