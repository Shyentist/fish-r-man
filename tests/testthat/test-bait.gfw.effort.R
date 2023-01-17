test_that("the right query is shown and prepared for right endpoint", {

  bait <- bait.gfw.effort(
    table = "fishing_effort_byvessel_v2",
    start_date = "2015-02-01",
    end_date = "2015-02-01",
    min_lat = 42,
    max_lat = 44.99,
    min_lon = 10,
    max_lon = 17.99)

  expect_equal(toString(bait[[1]]), "/gfw")

  expect_equal(toString(bait[[2]]), '{"endpoint":["/gfw"],"table":["fishing_effort_byvessel_v2"],"start_date":["2015-02-01"],"end_date":["2015-02-01"],"min_lat":[42],"max_lat":[44.99],"min_lon":[10],"max_lon":[17.99]}')

  expect_equal(
    bait[[3]],
    "SELECT * FROM global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2 WHERE date >= '2015-02-01' AND date <= '2015-02-01' AND cell_ll_lat >= 42 AND cell_ll_lat <= 44.99 AND cell_ll_lon >= 10 AND cell_ll_lon <= 17.99")
  # for the user's own convenience, not sent to the endpoint, but matches what the endpoint will search
})
