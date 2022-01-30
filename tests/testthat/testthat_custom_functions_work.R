library(testthat)

test_that("SQL constructor works", {
  
  source("fun/sql.construct.R")
  
  expect_equal(
    sql.construct(
      table = "global-fishing-watch.gfw_public_data.fishing_effort_v2", 
      date = c(
        as.Date.character(x = "2020-01-01", format = "%Y-%m-%d"), 
        as.Date.character(x = "2021-01-01", format = "%Y-%m-%d")), 
      cell_ll_lat = c(-45, 30), 
      cell_ll_lon = c(-9, 20),
      hours = c(0, 100),
      fishing_hours = c(1, 99),
      mmsi_present = c(2, 3)
      ), 
    "SELECT * FROM `global-fishing-watch.gfw_public_data.fishing_effort_v2` WHERE date >= '2020-01-01' AND date < '2021-01-01' AND cell_ll_lat >= -45 AND cell_ll_lat < 30 AND cell_ll_lon >= -9 AND cell_ll_lon < 20 AND hours >= 0 AND hours < 100 AND fishing_hours >= 1 AND fishing_hours < 99 AND mmsi_present >= 2 AND mmsi_present < 3")
  
  expect_equal(
    sql.construct(
      table = "global-fishing-watch.gfw_public_data.fishing_effort_v2", 
      date = NULL, 
      cell_ll_lat = NULL, 
      cell_ll_lon = NULL,
      hours = NULL,
      fishing_hours = NULL,
      mmsi_present = NULL
    ), 
    "SELECT * FROM `global-fishing-watch.gfw_public_data.fishing_effort_v2`")
  
  expect_equal(
    sql.construct(
      table = "global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2", 
      date = c(
        as.Date.character(x = "2020-01-01", format = "%Y-%m-%d"), 
        as.Date.character(x = "2021-01-01", format = "%Y-%m-%d")), 
      cell_ll_lat = c(-45, 30), 
      cell_ll_lon = c(-9, 20),
      hours = c(0, 100),
      fishing_hours = c(1, 99),
      geartype = c("trawlers", "drifting_longlines", "squid_jigger"),
      flag = c("ITA", "FRA", "CHN"),
      mmsi = "%123%"
    ), 
    "SELECT * FROM `global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2` WHERE date >= '2020-01-01' AND date < '2021-01-01' AND cell_ll_lat >= -45 AND cell_ll_lat < 30 AND cell_ll_lon >= -9 AND cell_ll_lon < 20 AND hours >= 0 AND hours < 100 AND fishing_hours >= 1 AND fishing_hours < 99 AND mmsi LIKE '%123%' AND flag IN ('ITA', 'FRA', 'CHN') AND geartype IN ('trawlers', 'drifting_longlines', 'squid_jigger')")
  
  expect_equal(
    sql.construct(
      table = "global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2", 
      date = NULL, 
      cell_ll_lat = NULL, 
      cell_ll_lon = NULL,
      hours = NULL,
      fishing_hours = NULL,
      geartype = NULL,
      flag = NULL,
      mmsi = NULL
    ), 
    "SELECT * FROM `global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2`")

  })

test_that("count.sql works", {
  
  source("fun/count.sql.R")
  
  expect_equal(count.sql("SELECT * FROM `global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2`"), "SELECT COUNT(*) as count_col FROM `global-fishing-watch.gfw_public_data.fishing_effort_byvessel_v2`")
  expect_equal(count.sql("SELECT * FROM `global-fishing-watch.gfw_public_data.fishing_effort_v2`"), "SELECT COUNT(*) as count_col FROM `global-fishing-watch.gfw_public_data.fishing_effort_v2`")
  
})

test_that("length.until works", {
  
  source("fun/length.until.R")
  
  expect_equal(length.until(c(0,1,2,3,4,5), 14), 4)
  expect_equal(length.until(c(2,1,3,5,4,0), 13), 4)
  expect_equal(length.until(c(0,1,2), 6), 3)
  
})