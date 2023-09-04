testthat::test_path("files", "summarized.catch.mmsi.csv")
testthat::test_path("files", "catch.csv")

test_that("the right summary is performed and returned", {
  expected.summary = read.csv("./files/summarized.catch.mmsi.csv", check.names=FALSE, colClasses = c("mmsi" = "character")) # the downloaded csv has user friendly spaces in its colnames, which would be converted into dots
  catch = read.csv("./files/catch.csv", colClasses = c("mmsi" = "character"))

  actual.summary = as.data.frame(gfw.summarize(group_by_at(catch, vars(one_of(c("mmsi")))))) # as.data.frame() used because the downloaded csv does not retain the tibble (tbl_df) class that is only useful in R tidyverse

  expect_equal(actual.summary, expected.summary)
})
