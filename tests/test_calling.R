library(shinytest)

#TESTING CSV UPLOAD, ANALYSES, AND CLIPPING FOR "10TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_and_analyses_for_10"
  )

#TESTING CSV UPLOAD, ANALYSES, AND CLIPPING FOR "100TH DEGREE" TABLE

testApp(
  appDir = getwd(),
  testnames = "upload_and_analyses_for_100"
)