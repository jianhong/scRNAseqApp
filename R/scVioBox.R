# Plot violin / boxplot
#' @importFrom stats rnorm
#' @importFrom ggplot2 ggplot aes_string geom_violin geom_boxplot geom_jitter xlab
#' ylab scale_fill_manual theme
scVioBox <- function(inpConf, inpMeta, inp1, inp1a, inp1b, inp1c, inp2, dataset, inpH5, inpGene,
                     inptyp, inppts, inpsiz, inpfsz,
                     datafolder){
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
    ggData$val <- read_exprs(file.path(datafolder, dataset, inpH5),
                             inpGene[inp2], valueOnly=TRUE)
    if(length(inp1c)){
      ggData <- ggData[ggData$val>=inp1c[1], , drop=FALSE]
    }
    ggData[val < 0]$val <- 0
    tmpNoise <- rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
    ggData$val <- ggData$val + tmpNoise
  }

  if(inp1a!="N/A" && length(inp1b)){
    ggData <-
      ggData[ggData[[inpConf[inpConf$UI == inp1a]$ID]] %in% inp1b,
             , drop=FALSE]
  }
  # Do factoring
  ggCol <- strsplit(inpConf[inpConf$UI == inp1]$fCL, "\\|")[[1]]
  names(ggCol) <- levels(ggData$X)
  ggLvl <- levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)]
  ggLvl <- sortLevels(ggLvl)
  ggData$X <- factor(ggData$X, levels = ggLvl)
  ggCol <- ggCol[ggLvl]

  # Actual ggplot
  if(inptyp == "violin"){
    ggOut <- ggplot(ggData, aes_string("X", "val", fill = "X")) +
      geom_violin(scale = "width")
  } else {
    ggOut <- ggplot(ggData, aes_string("X", "val", fill = "X")) +
      geom_boxplot()
  }
  if(inppts){
    ggOut <- ggOut + geom_jitter(size = inpsiz, shape = 16)
  }
  ggOut <- ggOut + xlab(inp1) + ylab(inp2) +
    sctheme(base_size = .globals$sList[inpfsz], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "none")
  return(ggOut)
}
