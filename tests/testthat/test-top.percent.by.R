testthat::test_path("files", "catch.csv")
testthat::test_path("files", "catch.top.90.hours.csv")

test_that("the right percentage of the dataframe is kept", {
  catch = read.csv("./files/catch.csv", colClasses = c("mmsi" = "character"))
  catch.top.90.fh = read.csv("./files/catch.top.90.hours.csv", colClasses = c("mmsi" = "character"))

  to.test = top.percent.by(catch, 90, "hours")

  sum.to.test = sum(to.test["hours"])

  sum.to.test.against = sum(catch.top.90.fh["hours"])

  expect_equal(sum.to.test, sum.to.test.against)

  expect_equal(summary(to.test), summary(catch.top.90.fh))

})
