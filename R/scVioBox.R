# Plot violin / boxplot
scVioBox <- function(inpConf, inpMeta, inp1, inp1a, inp1b, inp1c, inp2, dataset, inpH5, inpGene,
                     inptyp, inppts, inpsiz, inpfsz){
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[grp == TRUE]$ID),
                   with = FALSE]
  colnames(ggData)[1] = c("X")
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI){
    ggData$val = inpMeta[[inpConf[UI == inp2]$ID]]
    if(length(inp1c)){
      ggData <- ggData[ggData$val>=inp1c[1], , drop=FALSE]
    }
  } else {
    h5file <- H5File$new(file.path(datafolder, dataset, inpH5), mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    ggData$val = h5data$read(args = list(inpGene[inp2], quote(expr=)))
    if(length(inp1c)){
      ggData <- ggData[ggData$val>=inp1c[1], , drop=FALSE]
    }
    ggData[val < 0]$val = 0
    set.seed(42)
    tmpNoise = rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
    ggData$val = ggData$val + tmpNoise
    h5file$close_all()
  }

  if(inp1a!="N/A" && length(inp1b)){
    ggData <- ggData[ggData[[inpConf[UI == inp1a]$ID]] %in% inp1b, , drop=FALSE]
  }
  # Do factoring
  ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]]
  names(ggCol) = levels(ggData$X)
  ggLvl = levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)]
  ggData$X = factor(ggData$X, levels = ggLvl)
  ggCol = ggCol[ggLvl]

  # Actual ggplot
  if(inptyp == "violin"){
    ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_violin(scale = "width")
  } else {
    ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_boxplot()
  }
  if(inppts){
    ggOut = ggOut + geom_jitter(size = inpsiz, shape = 16)
  }
  ggOut = ggOut + xlab(inp1) + ylab(inp2) +
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "none")
  return(ggOut)
}
