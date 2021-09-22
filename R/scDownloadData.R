### Not work yet.
scDownloadData <- function(inpConf, inpMeta, inp1, inp1a, inp1b, inp2,
                           dataset, inpH5, inpGene, download=FALSE){
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[grp == TRUE]$ID),
                   with = FALSE]
  colnames(ggData)[1] = c("X")
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI){
    ggData$val = inpMeta[[inpConf[UI == inp2]$ID]]
    if(inp1a!="N/A" && length(inp1b)){
      ggData <- ggData[ggData[[inpConf[UI == inp1a]$ID]] %in% inp1b, , drop=FALSE]
    }
  } else {
    h5file <- H5File$new(file.path(datafolder, dataset, inpH5), mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    ggData = h5data$read(args = list(inpGene, quote(expr=)))
    rownames(ggData) <- inpGene
    if(inp1a!="N/A" && length(inp1b)){
      ggData <- ggData[ggData[[inpConf[UI == inp1a]$ID]] %in% inp1b, , drop=FALSE]
    }
    saveRDS(ggData, "www/ggData.rds")
    h5file$close_all()
  }

  if(download){
    # Actual data.table
    return(ggData)
  }else{
    ggData[seq.int(min(10, nrow(ggData))), , drop=FALSE]
  }
}


# ####serverside
#
# updateSelectInput(session,"scDwl1inp1", "Cell information:",
#                   choices = dataSource$sc1conf[grp == TRUE]$UI,
#                   selected = dataSource$sc1def$grp1)
# updateSelectInput(session,"scDwl1inp1a", "Cell information to subset by:",
#                   choices = c("N/A", dataSource$sc1conf[grp == TRUE]$UI),
#                   selected = "N/A")
# updateSelectizeInput(session, "scDwl1inp2", server = TRUE,
#                      choices = c(dataSource$sc1conf[is.na(fID)]$UI, "expression"),
#                      selected = dataSource$sc1conf[is.na(fID)]$UI[1], options = list(
#                        maxOptions = length(dataSource$sc1conf[is.na(fID)]$UI) + 3,
#                        create = TRUE, persist = TRUE, render = I(optCrt)))
# ### Plots for tab downloader
# output$scDwl1inp1b.ui <- renderUI({
#   if(input$scDwl1inp1a!="N/A"){
#     sub = strsplit(dataSource$sc1conf[UI == input$scDwl1inp1a]$fID, "\\|")[[1]]
#     checkboxGroupInput("scDwl1inp1b", "Select which cells to download", inline = TRUE,
#                        choices = sub)
#   }else{
#     sub = NULL
#   }
# })
# output$scDwl.dt <- renderDataTable({
#   ggData = scDownloadData(dataSource$sc1conf, dataSource$sc1meta, input$scDwl1inp1,
#                           input$scDwl1inp1a, input$scDwl1inp1b, input$scDwl1inp2,
#                           dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene)
#   datatable(ggData, rownames = TRUE, extensions = "Buttons",
#             options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel")))
# })
