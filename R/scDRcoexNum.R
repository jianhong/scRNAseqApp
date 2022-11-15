#' @importFrom data.table .N
scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2,
                        inpsub1, inpsub2, dataset, inpH5, inpGene,
                        datafolder){
  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("sub")
  ggData$val1 <- read_exprs(file.path(datafolder, dataset, inpH5),
                           inpGene[inp1], valueOnly=TRUE)
  ggData$val2 <- read_exprs(file.path(datafolder, dataset, inpH5),
                           inpGene[inp2], valueOnly=TRUE)
  ggData[val1 < 0]$val1 <- 0
  ggData[val2 < 0]$val2 <- 0
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    ggData <- ggData[sub %in% inpsub2]
  }

  # Actual data.table
  ggData$express <- "none"
  ggData[val1 > 0]$express <- inp1
  ggData[val2 > 0]$express <- inp2
  ggData[val1 > 0 & val2 > 0]$express <- "both"
  ggData$express <- factor(ggData$express, levels = unique(c("both", inp1, inp2, "none")))
  ggData <- ggData[, .(nCells = .N), by = "express"]
  ggData$percent <- 100 * ggData$nCells / sum(ggData$nCells)
  ggData <- ggData[order(express)]
  colnames(ggData)[1] <- "expression > 0"
  return(ggData)
}
