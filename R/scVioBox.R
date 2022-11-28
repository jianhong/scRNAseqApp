# Plot violin / boxplot
#' @importFrom stats rnorm
#' @importFrom ggplot2 ggplot aes_string geom_violin geom_boxplot geom_jitter xlab
#' ylab scale_fill_manual theme
scVioBox <- function(inpConf, inpMeta, inp1, grpKey, grpVal, inp1c,
                     inp2, dataset, inpGene,
                     inptyp, inppts, pointSize, labelsFontsize){
  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[inpConf$UI == inp1]$ID,
                        inpConf[inpConf$grp == TRUE]$ID),
                   with = FALSE]
  colnames(ggData)[1] = c("X")
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI){
    ggData$val <- inpMeta[[inpConf[inpConf$UI == inp2]$ID]]
    if(length(inp1c)){
      ggData <- ggData[ggData$val>=inp1c[1], , drop=FALSE]
    }
  } else {
    ggData$val <- read_exprs(dataset, inpGene[inp2], valueOnly=TRUE)
    if(length(inp1c)){
      ggData <- ggData[ggData$val>=inp1c[1], , drop=FALSE]
    }
    ggData[ggData$val < 0]$val <- 0
    tmpNoise <- rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
    ggData$val <- ggData$val + tmpNoise
  }

  ggData <- subGrp(ggData, grpKey, grpVal, inpConf)

  # Do factoring
  ggData <- relevelData(ggData, "X")
  ggCol <- relevelCol(inpConf, inp1, ggData, "X")

  # Actual ggplot
  if(inptyp == "violin"){
    ggOut <- ggplot(ggData, aes_string("X", "val", fill = "X")) +
      geom_violin(scale = "width")
  } else {
    ggOut <- ggplot(ggData, aes_string("X", "val", fill = "X")) +
      geom_boxplot()
  }
  if(inppts){
    ggOut <- ggOut + geom_jitter(size = pointSize, shape = 16)
  }
  ggOut <- ggOut + xlab(inp1) + ylab(inp2) +
    sctheme(base_size = .globals$sList[labelsFontsize], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "none")
  return(ggOut)
}
