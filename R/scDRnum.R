#' @importFrom data.table .N
scDRnum <- function(inpConf, inpMeta, inpCellInfo, inpGeneName,
                    inpsubName, inpsubValue,
                    dataset, inpH5, inpGene, inpsplt,
                    datafolder){
  if(length(inpCellInfo)>1){
    inpCellInfoSubGroup <- inpCellInfo[-1]
    inpCellInfo <- inpCellInfo[1]
  }else{
    inpCellInfoSubGroup <- NULL
  }
  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[UI == inpCellInfo]$ID,
                        inpConf[UI == inpsubName]$ID),
                   with = FALSE, drop=FALSE]
  if(nrow(ggData)<1){
    dt <- data.frame("group"=numeric(), "nCells"=numeric(),
                     "nExpress"=numeric(), "pctExpress"=numeric())
    return(dt)
  }
  colnames(ggData) <- c("group", "sub")
  h5file <- H5File$new(file.path(datafolder, dataset, inpH5), mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val2 <- h5data$read(args = list(inpGene[inpGeneName], quote(expr=)))
  ggData[val2 < 0]$val2 <- 0
  h5file$close_all()
  if(length(inpsubValue) != 0 & length(inpsubValue) != nlevels(ggData$sub)){
    ggData <- ggData[ggData$sub %in% inpsubValue]
  }
  if(length(inpCellInfoSubGroup) != 0 &
     length(inpCellInfoSubGroup) != nlevels(ggData$group)){
    ggData <- ggData[ggData$group %in% inpCellInfoSubGroup]
  }

  # Split inpCellInfo if necessary
  if(is.na(inpConf[UI == inpCellInfo]$fCL)){
    if(inpsplt == "Quartile"){nBk <- 4}
    if(inpsplt == "Decile"){nBk <- 10}
    ggData$group <- cut(ggData$group, breaks = nBk)
  }

  # Actual data.table
  ggData$express <- FALSE
  ggData[val2 > 0]$express <- TRUE
  ggData1 <- ggData[express == TRUE, .(nExpress = .N), by = "group"]
  ggData <- ggData[, .(nCells = .N), by = "group"]
  ggData <- ggData1[ggData, on = "group"]
  ggData <- ggData[, c("group", "nCells", "nExpress"), with = FALSE]
  ggData[is.na(nExpress)]$nExpress <- 0
  ggData$pctExpress <- 100 * ggData$nExpress / ggData$nCells
  ggData <- ggData[order(group)]
  colnames(ggData)[3] <- paste0(colnames(ggData)[3], "_", inpGeneName)
  return(ggData)
}
