# Plot proportion plot
#' @importFrom ggplot2 ggplot aes_string geom_col ylab coord_flip xlab
#' scale_fill_manual theme
#' @importFrom data.table .SD .N
scProp <- function(inpConf, inpMeta, inp1, grpKey, grpVal, inp2,
                   inptyp, flipXY, labelsFontsize,
                   dataset, geneIdMap,
                   valueFilterKey, valueFilterCutoff){
  # Prepare ggData
  if(grpKey!="N/A" && length(grpVal)){
    colN <- c(inpConf[inpConf$UI == inp1]$ID,
              inpConf[inpConf$UI == inp2]$ID,
              inpConf[inpConf$UI == grpKey]$ID)
  }else{
    colN <- c(inpConf[inpConf$UI == inp1]$ID,
              inpConf[inpConf$UI == inp2]$ID)
  }
  ggData <- inpMeta[, colN, with = FALSE]
  ggData <- subGrp(ggData, grpKey, grpVal, inpConf)
  subFilterColname <- 'subValue'
  ggData <- cbindFilterValues(ggData, inpConf, inpMeta, subFilterColname,
                              geneIdMap, dataset,
                              valueFilterKey, valueFilterCutoff)
  keep <- filterCells(ggData,
                      valueFilterKey=subFilterColname,
                      valueFilterCutoff=valueFilterCutoff)

  ggData <- ggData[keep, c(1, 2), drop=FALSE]
  colnames(ggData) <- c("X", "grp")
  ggData <- ggData[, list(nCells = .N), by = c("X", "grp")]
  ggData <- ggData[, {tot = sum(.SD$nCells)
  .SD[,list(pctCells = 100 * sum(.SD$nCells) / tot,
         nCells = .SD$nCells), by = "grp"]}, by = "X"]

  # Do factoring
  ggData <- relevelData(ggData, "grp")
  ggData <- relevelData(ggData, "X")
  ggCol <- relevelCol(inpConf, inp2, ggData, "grp")

  # Actual ggplot
  if(inptyp == "Proportion"){
    ggOut <- ggplot(ggData, aes_string("X", "pctCells", fill = "grp")) +
      geom_col() + ylab("Cell Proportion (%)")
  } else {
    ggOut <- ggplot(ggData, aes_string("X", "nCells", fill = "grp")) +
      geom_col() + ylab("Number of Cells")
  }
  if(flipXY){
    ggOut <- ggOut + coord_flip()
  }
  ggOut <- ggOut + xlab(inp1) +
    sctheme(base_size = .globals$sList[labelsFontsize], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "right")
  return(ggOut)
}
