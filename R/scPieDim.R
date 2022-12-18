# Plot gene expression scatter_pie
#' @importFrom hdf5r H5File
#' @importFrom data.table rbindlist
#' @importFrom scales rescale
#' @importFrom ggforce geom_arc_bar geom_circle geom_mark_hull
#' @importFrom ggplot2 geom_rect
scPieDim <- function(inpConf, inpMeta,
                     dataset, geneIdMap,
                     dimRedX, dimRedY,
                     genelist,
                     subsetCellKey, subsetCellVal,
                     valueFilterKey, valueFilterCutoff,
                     CoExpred,
                     pointSize,
                     lableCircle,
                     plotCellBg,
                     markGrp,
                     alpha,
                     plotType,
                     labelsFontsize,
                     plotAspectRatio,
                     keepXYlables){
  subFilterColname <- 'subValue'
  subGrpColname <- 'sub'
  geneList <- scGeneList(genelist, geneIdMap)
  geneList <- geneList[geneList$present == TRUE]
  shiny::validate(
    need(nrow(geneList) <= 10,
         "More than 10 genes to plot! Please reduce the gene list!"))
  shiny::validate(
    need(nrow(geneList) > 1, "Please input at least 2 genes to plot!"))

  # Prepare ggData
  ggData <- inpMeta[, c("sampleID",
                        inpConf[inpConf$UI == dimRedX]$ID,
                        inpConf[inpConf$UI == dimRedY]$ID,
                        inpConf[inpConf$UI == subsetCellKey]$ID),
                    with = FALSE]
  if(nrow(ggData)==0) return(NULL)
  colnames(ggData) <- c("sampleID", "X", "Y", subGrpColname)
  ggData <- cbindFilterValues(ggData, inpConf, inpMeta, subFilterColname,
                              geneIdMap, dataset,
                              valueFilterKey, valueFilterCutoff)
  rat <- getRatio(ggData)
  expr <- lapply(geneIdMap[geneList$gene][
    seq.int(min(c(10, length(geneList$gene))))],
                 read_exprs, h5f=dataset, valueOnly=TRUE)
  expr <- do.call(cbind, expr)
  expr[expr<0] <- 0
  expr_keep <- rowSums(expr)>0
  if(CoExpred) expr_keep <- expr_keep & rowSums(expr>0)==ncol(expr)
  keep <- filterCells(ggData,
                      subGrpColname, subsetCellVal,
                      subFilterColname, valueFilterCutoff)
  if(sum(keep & expr_keep)*nrow(geneList)>5000){
    ## filter the expression event, otherwise too slow
    expr_keep <- rowSums(expr)
    expr_keep <- expr_keep >= sort(expr_keep, decreasing = TRUE)[
      floor(1000/nrow(geneList))]
  }
  ggData_ <- ggData[keep & expr_keep]
  expr <- expr[keep & expr_keep, , drop=FALSE]
  ggData <- ggData[keep]
  size <- diff(range(ggData$X))/ 60 * pointSize
  expr <- apply(expr, 2, rescale, to=c(size/4, size))
  expr <- as.list(as.data.frame(expr))
  expr <- mapply(function(d, n){
    cbind(ggData_, geneName=n, "val"=d)
  }, expr, names(expr), SIMPLIFY = FALSE)
  expr <- rbindlist(expr)

  expr$X0 <- expr$X + (as.numeric(factor(as.character(expr$geneName)))-1)*size/2
  ggOut <- ggplot(data=expr, aes(x0=expr$X, y0=expr$Y))
  if(markGrp){
    ggOut <- ggOut +
      geom_mark_hull(data=ggData,
                     aes(x = ggData$X, y = ggData$Y,
                         fill = ggData$sub, label = ggData$sub),
                     inherit.aes = FALSE, show.legend = FALSE)
  }
  if(plotCellBg){
    ggOut <- ggOut +
    geom_point(data=ggData, aes(x=ggData$X, y=ggData$Y),
               color='snow2', shape=16, inherit.aes = FALSE)
  }
  ggOut <- ggOut +
    switch(plotType,
           sunburst =
             geom_arc_bar(aes(amount=1, r0=0,
                              r=expr$val, fill=expr$geneName),
                          stat = 'pie', color=NA,
                          alpha=alpha),
           pie =
             geom_arc_bar(aes(amount=expr$val, r0=0,
                              r=size, fill=expr$geneName),
                          stat = 'pie', color=NA,
                          alpha=alpha),
           donut =
             geom_arc_bar(aes(amount=expr$val, r0=size/2,
                              r=size, fill=expr$geneName),
                          stat = 'pie', color=NA,
                          alpha=alpha),
           bar =
             geom_rect(aes(xmin=expr$X0-.02*size, ymin=expr$Y,
                           xmax=expr$X0+.46*size, ymax=expr$Y+expr$val,
                           fill=expr$geneName),
                          position = "identity",
                          alpha=alpha, color=NA)) +
    guides(fill = guide_legend(title = 'genename'),
           alpha = "none")

  if(lableCircle){
    if(plotType != "bar"){
      ggOut <- ggOut +
        geom_circle(aes(color=expr$sub, r=size), fill=NA, lwd=.5) +
        guides(color = guide_legend(title = subsetCellKey))
    }else{
      ggOut <- ggOut +
        geom_rect(aes(xmin=expr$X - .025*size, ymin=expr$Y-.05,
                      xmax=expr$X + (nrow(geneList)+.05)*size/2,
                      ymax=expr$Y + size * 1.05,
                      color=expr$sub),
                  fill=NA, linewidth=.5) +
        guides(color = guide_legend(title = subsetCellKey))
    }
  }
  ggOut <- ggOut +
    xlab(dimRedX) + ylab(dimRedY)+
    sctheme(base_size = .globals$sList[labelsFontsize], XYval = keepXYlables)
  ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
  return(ggOut)
}
