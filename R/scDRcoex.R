# Plot gene coexpression on dimred
bilinear <- function(x,y,xy,Q11,Q21,Q12,Q22){
  oup <- (xy-x)*(xy-y)*Q11 + x*(xy-y)*Q21 + (xy-x)*y*Q12 + x*y*Q22
  oup <- oup / (xy*xy)
  return(oup)
}
#' @importFrom grDevices rgb
#' @importFrom data.table data.table
#' @importFrom plotly plot_ly layout
#' @importFrom ggplot2 ggplot aes_string geom_point xlab ylab scale_color_gradientn
#' guides guide_colorbar coord_fixed
scDRcoex <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2,
                     inpsub1, inpsub2, dataset, inpH5, inpGene,
                     inpsiz, inptype, inpcol, inpord, inpfsz, inpasp, inptxt,
                     datafolder){
  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[inpConf$UI == inpdrX]$ID,
                        inpConf[inpConf$UI == inpdrY]$ID,
                        inpConf[inpConf$UI == inpsub1]$ID),
                   with = FALSE]
  if(nrow(ggData)==0) return(NULL)
  colnames(ggData) <- c("X", "Y", "sub")
  rat <- getRatio(ggData)

  ggData$val1 <- read_exprs(file.path(datafolder, dataset, inpH5),
                            inpGene[inp1], valueOnly=TRUE)
  ggData$val2 <- read_exprs(file.path(datafolder, dataset, inpH5),
                            inpGene[inp2], valueOnly=TRUE)
  ggData[ggData$val1 < 0]$val1 <- 0
  ggData[ggData$val2 < 0]$val2 <- 0
  bgCells <- FALSE
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    bgCells <- TRUE
    ggData2 <- ggData[!ggData$sub %in% inpsub2]
    ggData <- ggData[ggData$sub %in% inpsub2]
  }
  ## color for group
  # Do factoring if required
  ggCol <- NULL
  if(!is.na(inpConf[inpConf$UI == inpsub1]$fCL)){
    ggCol <- strsplit(inpConf[inpConf$UI == inpsub1]$fCL, "\\|")[[1]]
    names(ggCol) <- levels(ggData$sub)
    ggLvl <- levels(ggData$sub)[levels(ggData$sub) %in% unique(ggData$sub)]
    ggData$sub <- factor(ggData$sub, levels = ggLvl)
    ggCol <- ggCol[ggLvl]
  }

  # Generate coex color palette
  cInp <- strsplit(inpcol, "; ")[[1]]
  if(cInp[1] == "Red (Gene1)"){
    c10 <- c(255,0,0)
  } else if(cInp[1] == "Orange (Gene1)"){
    c10 <- c(255,140,0)
  } else {
    c10 <- c(0,255,0)
  }
  if(cInp[2] == "Green (Gene2)"){
    c01 <- c(0,255,0)
  } else {
    c01 <- c(0,0,255)
  }
  c00 <- c(217,217,217)
  c11 <- c10 + c01
  nGrid <- 16
  nPad <- 2
  nTot <- nGrid + nPad * 2
  gg <- data.table(v1 = rep(0:nTot,nTot+1),
                   v2 = sort(rep(0:nTot,nTot+1)))
  gg$vv1 <- gg$v1 - nPad
  gg[gg$vv1 < 0]$vv1 <- 0
  gg[gg$vv1 > nGrid]$vv1 <- nGrid
  gg$vv2 <- gg$v2 - nPad
  gg[gg$vv2 < 0]$vv2 <- 0
  gg[gg$vv2 > nGrid]$vv2 <- nGrid
  gg$cR <- bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
  gg$cG <- bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
  gg$cB <- bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
  gg$cMix <- rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
  gg <- gg[, c("v1", "v2", "cMix")]

  # Map colours
  ggData$v1 <- round(nTot * ggData$val1 / max(ggData$val1, na.rm = TRUE))
  ggData$v2 <- round(nTot * ggData$val2 / max(ggData$val2, na.rm = TRUE))
  ggData$v0 <- ggData$v1 + ggData$v2
  ggData <- gg[ggData, on = c("v1", "v2")]
  if(inpord == "Max-1st"){
    ggData <- ggData[order(ggData$v0)]
  } else if(inpord == "Min-1st"){
    ggData <- ggData[order(-ggData$v0)]
  } else if(inpord == "Random"){
    ggData <- ggData[sample(nrow(ggData))]
  }

  # Actual ggplot
  if(inptype=="3D"){
    nTot <- max(c(ggData$val1, ggData$val2), na.rm = TRUE)
    ggData$norm1 <- round(nTot * ggData$val1 / max(ggData$val1, na.rm = TRUE))
    ggData$norm2 <- round(nTot * ggData$val2 / max(ggData$val2, na.rm = TRUE))
    ggData$Z <- log2(ggData$norm1+1)-log2(ggData$norm2+1)
    return(layout(plot_ly(x=ggData$X, y=ggData$Y, z=ggData$Z,
                   type="scatter3d",
                   mode="markers",
                   color=ggData$sub,
                   colors=ggCol,
                   text = paste0(inp1, ": ", ggData$val1, "\n",
                                 inp2, ": ", ggData$val2),
                   size = 1),
                  scene =
                    list(xaxis = list(title = inpdrX),
                         yaxis = list(title = inpdrY),
                         zaxis = list(title = paste0('Log2 Fold Change (',
                                                     inp1, '/', inp2, ')')))))
  }
  ggOut <- ggplot(ggData, aes_string("X", "Y"))
  if(bgCells){
    ggOut <- ggOut +
      geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16)
  }
  ggOut <- ggOut +
    geom_point(size = inpsiz, shape = 16, color = ggData$cMix) +
    xlab(inpdrX) + ylab(inpdrY) +
    sctheme(base_size = .globals$sList[inpfsz], XYval = inptxt) +
    scale_color_gradientn(inp1, colours = .globals$cList[[1]]) +
    guides(color = guide_colorbar(barwidth = 15))
  if(inpasp == "Square") {
    ggOut <- ggOut + coord_fixed(ratio = rat)
  } else if(inpasp == "Fixed") {
    ggOut <- ggOut + coord_fixed()
  }
  return(ggOut)
}
