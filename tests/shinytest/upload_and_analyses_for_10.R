app <- ShinyDriver$new("../../", loadTimeout = 100000)
app$snapshotInit("upload_and_analyses_for_10")

app$setInputs(tabs = "Analysis")
app$uploadFile(uploaded_csv = "unrelated.csv") #csv not derived from fishRman, no input is activated
Sys.sleep(3)
app$snapshot()
app$uploadFile(uploaded_csv = "empty_query_10.csv") #empty csv, no input is activated
Sys.sleep(3)
app$snapshot()
app$uploadFile(uploaded_csv = "impossible_query_10.csv") #empty csv, no input is activated
Sys.sleep(3)
app$snapshot()
app$uploadFile(uploaded_csv = "successful_query_10.csv") #fitting csv, inputs are activated
Sys.sleep(3)
app$snapshot()
app$setInputs(summaries = c("month", "cell_ll_lat", "cell_ll_lon"))#choice for summaries are chosen
app$setInputs(summarize_button = "click", timeout_ = 150000)
Sys.sleep(6)
app$snapshot()
app$setInputs(analyses_types_tabs = "Spatial") #on to the spatial analysis
app$setInputs(visualize_button = "click", timeout_ = 150000)
Sys.sleep(6)
app$snapshot()
app$setInputs(cumul_distr_percent = 95) #revisualize with new options
app$setInputs(map_rez = 0.2)
app$setInputs(mapped_column = "Mean fishing hours") 
app$setInputs(add_layer = "eez_boundaries_v11.gpkg")
app$setInputs(re_visualize_button = "click", timeout_ = 150000)
app$snapshot()
app$uploadFile(area_of_interest = "overlay.gpkg") #same as above but for clipped data
app$setInputs(area_of_interest_layer = "sanctuary")
app$setInputs(clip = TRUE)
app$setInputs(visualize_button = "click", timeout_ = 150000)
Sys.sleep(3)
app$snapshot()
app$setInputs(analyses_types_tabs = "Descriptive")
app$setInputs(summaries = "month")
app$setInputs(summarize_button = "click", timeout_ = 150000)
Sys.sleep(3)
app$snapshot()