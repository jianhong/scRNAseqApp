# Plot gene expression scatter_pie
#' @importFrom hdf5r H5File
#' @importFrom data.table rbindlist
#' @importFrom scales rescale
#' @importFrom ggforce geom_arc_bar geom_circle geom_mark_hull
scPieDim <- function(inpConf, inpMeta,
                     dataset, geneIdMap,
                     dimRedX, dimRedY,
                     genelist,
                     subsetCellKey, subsetCellVal,
                     valueFilterKey, valueFilterCutoff,
                     pointSize,
                     lableCircle,
                     plotCellBg,
                     markGrp,
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
  size <- diff(range(ggData_$X))/ 60 * pointSize
  expr <- rescale(expr, to=c(size/4, size))
  expr <- as.list(as.data.frame(expr))
  expr <- mapply(function(d, n){
    cbind(ggData_, geneName=n, "val"=d)
  }, expr, names(expr), SIMPLIFY = FALSE)
  expr <- rbindlist(expr)

  expr$X0 <- expr$X + (as.numeric(factor(as.character(expr$geneName)))-.5)*size
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
               color='gray', shape=16, inherit.aes = FALSE)
  }
  ggOut <- ggOut +
    switch(plotType,
           sunburst =
             geom_arc_bar(aes(amount=1, r0=0, r=expr$val, fill=expr$geneName),
                          stat = 'pie', color=NA),
           pie =
             geom_arc_bar(aes(amount=expr$val, r0=0,
                              r=size, fill=expr$geneName),
                          stat = 'pie', color=NA),
           donut =
             geom_arc_bar(aes(amount=expr$val, r0=size/2,
                              r=size, fill=expr$geneName),
                          stat = 'pie', color=NA),
           bar =
             geom_segment(aes(x=expr$X0, y=expr$Y,
                              xend=expr$X0, yend=expr$Y+expr$val,
                              color=expr$geneName),
                          position = "identity",
                          linewidth=size)) +
    guides(fill = guide_legend(title = 'genename'))

  if(lableCircle){
    if(plotType != "bar"){
      ggOut <- ggOut +
        geom_circle(aes(color=expr$sub, r=size), fill=NA, lwd=.5) +
        guides(color = guide_legend(title = subsetCellKey))
    }else{
      ggOut <- ggOut +
        geom_rect(aes(xmin=expr$X - .05*size, ymin=expr$Y-.05,
                      xmax=expr$X + (nrow(geneList)+.05)*size,
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
