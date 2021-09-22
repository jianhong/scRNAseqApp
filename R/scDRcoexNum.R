scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2,
                        inpsub1, inpsub2, dataset, inpH5, inpGene){
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) = c("sub")
  h5file <- H5File$new(file.path(datafolder, dataset, inpH5), mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=)))
  ggData[val1 < 0]$val1 = 0
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=)))
  ggData[val2 < 0]$val2 = 0
  h5file$close_all()
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    ggData = ggData[sub %in% inpsub2]
  }

  # Actual data.table
  ggData$express = "none"
  ggData[val1 > 0]$express = inp1
  ggData[val2 > 0]$express = inp2
  ggData[val1 > 0 & val2 > 0]$express = "both"
  ggData$express = factor(ggData$express, levels = unique(c("both", inp1, inp2, "none")))
  ggData = ggData[, .(nCells = .N), by = "express"]
  ggData$percent = 100 * ggData$nCells / sum(ggData$nCells)
  ggData = ggData[order(express)]
  colnames(ggData)[1] = "expression > 0"
  return(ggData)
}
