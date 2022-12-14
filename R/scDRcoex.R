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
scDRcoex <- function(inpConf, inpMeta,
                     dimRedX, dimRedY,
                     gene1, gene2,
                     subsetCellKey, subsetCellVal,
                     dataset, geneIdMap,
                     plotType,
                     pointSize,
                     GeneExprDotCol,
                     GeneExprDotOrd,
                     labelsFontsize,
                     plotAspectRatio,
                     keepXYlables,
                     valueFilterKey, valueFilterCutoff){
  if(is.null(gene1) || is.null(gene2) || gene1=="" || gene2==""){
    return(NULL)
  }
  subFilterColname <- 'subValue'
  subGrpColname <- 'sub'
  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[inpConf$UI == dimRedX]$ID,
                        inpConf[inpConf$UI == dimRedY]$ID,
                        inpConf[inpConf$UI == subsetCellKey]$ID),
                   with = FALSE]
  if(nrow(ggData)==0) return(NULL)
  colnames(ggData) <- c("X", "Y", subGrpColname)
  if(plotType=="3D"){
    ggData$sampleID <- inpMeta$sampleID
  }
  ggData <- cbindFilterValues(ggData, inpConf, inpMeta, subFilterColname,
                              geneIdMap, dataset,
                              valueFilterKey, valueFilterCutoff)
  rat <- getRatio(ggData)

  ggData <- getCoexpVal(ggData, dataset, geneIdMap, gene1, gene2)
  keep <- filterCells(ggData,
                      subGrpColname, subsetCellVal,
                      subFilterColname, valueFilterCutoff)

  bgCells <- sum(!keep)>0
  if(bgCells){
    ggData2 <- ggData[!keep]
    ggData <- ggData[keep]
  }
  ## color for group
  # Do factoring if required
  ggData <- relevelData(ggData, subGrpColname)
  ggCol <- relevelCol(inpConf, subsetCellKey, ggData, subGrpColname)

  # Generate coex color palette
  cInp <- strsplit(GeneExprDotCol, "; ")[[1]]

  nTot <- getTotalNumber(nGrid = 16, nPad = 2)
  gg <- getCoexpCol(GeneExprDotCol, nGrid = 16, nPad = 2)

  # Map colours
  ggData$v1 <- round(nTot * ggData$val1 / max(ggData$val1, na.rm = TRUE))
  ggData$v2 <- round(nTot * ggData$val2 / max(ggData$val2, na.rm = TRUE))
  ggData$v0 <- ggData$v1 + ggData$v2
  ggData <- gg[ggData, on = c("v1", "v2")]
  ggData <- orderGeneExpr(ggData, GeneExprDotOrd, 'v0')

  # Actual ggplot
  if(plotType=="3D"){
    nTot <- max(c(ggData$val1, ggData$val2), na.rm = TRUE)
    ggData$norm1 <- round(nTot * ggData$val1 / max(ggData$val1, na.rm = TRUE))
    ggData$norm2 <- round(nTot * ggData$val2 / max(ggData$val2, na.rm = TRUE))
    ggData$Z <- log2(ggData$norm1+1)-log2(ggData$norm2+1)
    return(layout(plot_ly(x=ggData$X, y=ggData$Y, z=ggData$Z,
                          customdata=ggData$sampleID,
                   type="scatter3d",
                   mode="markers",
                   color=if(GeneExprDotCol=="Default"){ggData[[subGrpColname]]
                     } else{ ggData$Z },
                   colors=if(GeneExprDotCol=="Default") ggCol else
                     .globals$cList[[GeneExprDotCol]],
                   text = paste0(gene1, ": ", ggData$val1, "\n",
                                 gene2, ": ", ggData$val2),
                   size = 1),
                  scene =
                    list(xaxis = list(title = dimRedX),
                         yaxis = list(title = dimRedY),
                         zaxis = list(title = paste0('Log2 Fold Change (',
                                                     gene1, '/', gene2, ')')))))
  }
  ggOut <- ggplot(ggData, aes(x=ggData$X, y=ggData$Y))
  if(bgCells){
    ggOut <- labelBackgroundCells(ggOut, ggData2, pointSize,
                                  color="snow2", shape=16)
  }
  ggOut <- ggOut +
    geom_point(size = pointSize, shape = 16, color = ggData$cMix) +
    xlab(dimRedX) + ylab(dimRedY) +
    sctheme(base_size = .globals$sList[labelsFontsize], XYval = keepXYlables) +
    scale_color_gradientn(gene1, colours = .globals$cList[[1]]) +
    guides(color = guide_colorbar(barwidth = 15))

  ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
  return(ggOut)
}
