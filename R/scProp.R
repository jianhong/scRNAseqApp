# Plot proportion plot
scProp <- function(inpConf, inpMeta, inp1, inp1a, inp1b, inp2,
                   inptyp, inpflp, inpfsz){
  # Prepare ggData
  if(inp1a!="N/A" && length(inp1b)){
    colN <- c(inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID, inpConf[UI == inp1a]$ID)
  }else{
    colN <- c(inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID)
  }
  ggData = inpMeta[, colN, with = FALSE]
  if(inp1a!="N/A" && length(inp1b)){
    ggData <- ggData[ggData[[inpConf[UI == inp1a]$ID]] %in% inp1b, c(1, 2), drop=FALSE]
  }
  colnames(ggData) = c("X", "grp")
  ggData = ggData[, .(nCells = .N), by = c("X", "grp")]
  ggData = ggData[, {tot = sum(nCells)
  .SD[,.(pctCells = 100 * sum(nCells) / tot,
         nCells = nCells), by = "grp"]}, by = "X"]

  # Do factoring
  ggCol = strsplit(inpConf[UI == inp2]$fCL, "\\|")[[1]]
  names(ggCol) = levels(ggData$grp)
  ggLvl = levels(ggData$grp)[levels(ggData$grp) %in% unique(ggData$grp)]
  ggLvl = sortLevels(ggLvl)
  ggData$grp = factor(ggData$grp, levels = ggLvl)
  ggCol = ggCol[ggLvl]
  ggData$X <- factor(ggData$X, levels=sortLevels(as.character(unique(ggData$X))))

  # Actual ggplot
  if(inptyp == "Proportion"){
    ggOut = ggplot(ggData, aes(X, pctCells, fill = grp)) +
      geom_col() + ylab("Cell Proportion (%)")
  } else {
    ggOut = ggplot(ggData, aes(X, nCells, fill = grp)) +
      geom_col() + ylab("Number of Cells")
  }
  if(inpflp){
    ggOut = ggOut + coord_flip()
  }
  ggOut = ggOut + xlab(inp1) +
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "right")
  return(ggOut)
}
