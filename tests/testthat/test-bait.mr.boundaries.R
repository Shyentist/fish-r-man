test_that("the right endpoint is returned", {

  endpoint <- bait.mr.boundaries(file = "eez_24nm")

  expect_equal(endpoint, "/mr?file=eez_24nm_v3_boundaries")

  endpoint <- bait.mr.boundaries(file = "eez_12nm")

  expect_equal(endpoint, "/mr?file=eez_12nm_v3_boundaries")

  endpoint <- bait.mr.boundaries(file = "eez")

  expect_equal(endpoint, "/mr?file=eez_boundaries_v11")
})
