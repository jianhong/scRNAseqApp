# Plot proportion plot
#' @importFrom ggplot2 ggplot aes_string geom_col ylab coord_flip xlab
#' scale_fill_manual theme
#' @importFrom data.table .SD .N
scProp <- function(inpConf, inpMeta, inp1, inp1a, inp1b, inp2,
                   inptyp, inpflp, inpfsz){
  # Prepare ggData
  if(inp1a!="N/A" && length(inp1b)){
    colN <- c(inpConf[inpConf$UI == inp1]$ID,
              inpConf[inpConf$UI == inp2]$ID,
              inpConf[inpConf$UI == inp1a]$ID)
  }else{
    colN <- c(inpConf[inpConf$UI == inp1]$ID,
              inpConf[inpConf$UI == inp2]$ID)
  }
  ggData <- inpMeta[, colN, with = FALSE]
  if(inp1a!="N/A" && length(inp1b)){
    ggData <- ggData[ggData[[inpConf[inpConf$UI == inp1a]$ID]] %in% inp1b,
                     c(1, 2), drop=FALSE]
  }
  colnames(ggData) <- c("X", "grp")
  ggData <- ggData[, list(nCells = .N), by = c("X", "grp")]
  ggData <- ggData[, {tot = sum(.SD$nCells)
  .SD[,list(pctCells = 100 * sum(.SD$nCells) / tot,
         nCells = .SD$nCells), by = "grp"]}, by = "X"]

  # Do factoring
  ggCol <- strsplit(inpConf[inpConf$UI == inp2]$fCL, "\\|")[[1]]
  names(ggCol) <- levels(ggData$grp)
  ggLvl <- levels(ggData$grp)[levels(ggData$grp) %in% unique(ggData$grp)]
  ggLvl <- sortLevels(ggLvl)
  ggData$grp <- factor(ggData$grp, levels = ggLvl)
  ggCol <- ggCol[ggLvl]
  ggData$X <- factor(ggData$X, levels=sortLevels(as.character(unique(ggData$X))))

  # Actual ggplot
  if(inptyp == "Proportion"){
    ggOut <- ggplot(ggData, aes_string("X", "pctCells", fill = "grp")) +
      geom_col() + ylab("Cell Proportion (%)")
  } else {
    ggOut <- ggplot(ggData, aes_string("X", "nCells", fill = "grp")) +
      geom_col() + ylab("Number of Cells")
  }
  if(inpflp){
    ggOut <- ggOut + coord_flip()
  }
  ggOut <- ggOut + xlab(inp1) +
    sctheme(base_size = .globals$sList[inpfsz], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "right")
  return(ggOut)
}
