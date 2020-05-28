#------------------- All Data Tab ------------------


output$allData_Table <- DT::renderDataTable({
  colnames <- as.character(sapply(names(data.CU.Metrics), GetLabel))
  DT::datatable(data.CU.Metrics, colnames=colnames)
})

# Downloadable csv of selected dataset
output$allData_Download <- downloadHandler(  
  filename = "data.csv",
  content = function(file) {write.csv(data.CU.Metrics, file, row.names = FALSE)}
)
