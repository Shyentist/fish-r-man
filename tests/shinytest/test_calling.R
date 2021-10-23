library(shinytest)

setwd("C:/Users/pasqu/Documents/GitKraken/fish-r-man")#change with path to your repo

#TESTING CSV UPLOAD AND DESCRIPTIVE ANALYSIS FOR "10TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_and_descriptive_for_10"
  )

#TESTING CSV UPLOAD AND DESCRIPTIVE ANALYSIS FOR "100TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_and_descriptive_for_100"
)

#TESTING GPKG UPLOAD AND SPATIAL ANALYSIS FOR "10TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_and_spatial_for_10"
)

#TESTING GPKG UPLOAD AND SPATIAL ANALYSIS FOR "100TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_and_spatial_for_100"
)

#TESTING CROPPING AN AREA FROM "10TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_geo10_and_clip"
)

#TESTING CROPPING AN AREA FROM "100TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_geo100_and_clip"
)

