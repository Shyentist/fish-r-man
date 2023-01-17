testthat::test_path("files", "catch.csv")

test_that("the right dataframe is returned from the API", {
  catch = read.csv("./files/catch.csv")

  bait <- bait.gfw.effort(
    table = "fishing_effort_byvessel_v2",
    start_date = "2015-02-01",
    end_date = "2015-02-01",
    min_lat = 42,
    max_lat = 44.99,
    min_lon = 10,
    max_lon = 17.99)

  just.fished = fish(bait)

  expect_equal(just.fished, catch)

})
