testthat::test_path("files", "catch.csv")

test_that("the right dataframe is returned from the API", {

  colClasses <- c("character")

  names(colClasses) <- "mmsi"

  catch = read.csv("./files/catch.csv", colClasses = colClasses)

  bait <- bait.gfw.effort(
    table = "fishing_effort_byvessel_v2",
    start_date = "2015-02-01",
    end_date = "2015-02-01",
    min_lat = 40,
    max_lat = 44.99,
    min_lon = 10,
    max_lon = 17.99)

  just.fished = fish(bait)

  expect_equal(just.fished, catch)

})
