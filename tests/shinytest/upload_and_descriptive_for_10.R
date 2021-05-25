app <- ShinyDriver$new("../../", loadTimeout = 100000)
app$snapshotInit("upload_and_descriptive_for_10")

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
app$setInputs(summaries = "month")
app$setInputs(summaries = c("month", "cell_ll_lat"))
app$setInputs(summaries = c("month", "cell_ll_lat", "cell_ll_lon"))#choice for summaries are chosen
app$setInputs(summarize_button = "click")
Sys.sleep(6)
app$snapshot()
