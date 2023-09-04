test_that("the right endpoint is returned", {

  endpoint <- bait.gfw.effort(
    table = "fishing_effort_byvessel_v2",
    start_date = "2012-01-01",
    end_date = "2012-01-10",
    min_lat = 0,
    max_lat = 44.99,
    min_lon = 0,
    max_lon = 17.99)

  expect_equal(endpoint, "/gfw?table=fishing_effort_byvessel_v2&start_date=2012-01-01&end_date=2012-01-10&min_lat=0&max_lat=44.99&min_lon=0&max_lon=17.99")
})
