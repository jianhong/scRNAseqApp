scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2,
                    dataset, inpH5, inpGene, inpsplt){
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
                   with = FALSE]
  colnames(ggData) = c("group", "sub")
  h5file <- H5File$new(file.path(datafolder, dataset, inpH5), mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=)))
  ggData[val2 < 0]$val2 = 0
  h5file$close_all()
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    ggData = ggData[sub %in% inpsub2]
  }

  # Split inp1 if necessary
  if(is.na(inpConf[UI == inp1]$fCL)){
    if(inpsplt == "Quartile"){nBk = 4}
    if(inpsplt == "Decile"){nBk = 10}
    ggData$group = cut(ggData$group, breaks = nBk)
  }

  # Actual data.table
  ggData$express = FALSE
  ggData[val2 > 0]$express = TRUE
  ggData1 = ggData[express == TRUE, .(nExpress = .N), by = "group"]
  ggData = ggData[, .(nCells = .N), by = "group"]
  ggData = ggData1[ggData, on = "group"]
  ggData = ggData[, c("group", "nCells", "nExpress"), with = FALSE]
  ggData[is.na(nExpress)]$nExpress = 0
  ggData$pctExpress = 100 * ggData$nExpress / ggData$nCells
  ggData = ggData[order(group)]
  colnames(ggData)[3] = paste0(colnames(ggData)[3], "_", inp2)
  return(ggData)
}
