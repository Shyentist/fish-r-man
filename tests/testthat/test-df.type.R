testthat::test_path("files", "catch.csv")

test_that("the right type of dataframe is determined", {
  df = read.csv("./files/catch.csv")

  expected.type = "GFW Fishing Effort By Vessel"

  actual.type = df.type(df)

  expect_equal(actual.type, expected.type)
})
