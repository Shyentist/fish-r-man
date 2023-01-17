testthat::test_path("files", "catch.csv")
testthat::test_path("files", "catch.top.90.fh.csv")

test_that("the right percentage of the dataframe is kept", {
  catch = read.csv("./files/catch.csv")
  catch.top.90.fh = read.csv("./files/catch.top.90.fh.csv")

  to.test = top.percent.by(catch, 90, "fishing_hours")

  sum.to.test = sum(to.test["fishing_hours"])

  sum.to.test.against = sum(catch.top.90.fh["fishing_hours"])

  expect_equal(sum.to.test, sum.to.test.against)

  expect_equal(to.test, catch.top.90.fh)

})
