testthat::test_path("files", "catch.csv")

test_that("the right dataframe is returned from the API", {

  catch = read.csv("./files/catch.csv", colClasses = c("mmsi" = "character"))

  bait <- bait.gfw.effort(
    table = "fishing_effort_byvessel_v2",
    start_date = "2012-01-01",
    end_date = "2012-01-10",
    min_lat = 0,
    max_lat = 44.99,
    min_lon = 0,
    max_lon = 17.99)

  just.fished = fish(bait)

  just.fished = summary(just.fished)

  catch = summary(catch)

  expect_equal(just.fished, catch)

  query = fish(bait, sql="query")

  expect_equal(query, "SELECT * FROM gfw_fishing_effort_byvessel_v2 WHERE dated >= '2012-01-01' AND dated <= '2012-01-10' AND lat >= 0 AND lat <= 44.99 AND lon >= 0 AND lon <= 17.99")

  count <- fish(bait, sql="count")

  expect_equal(count, 930)

})
