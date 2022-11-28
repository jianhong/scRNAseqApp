# Plot gene expression bubbleplot / heatmap
#' @importFrom stats as.dendrogram dist hclust quantile as.formula
#' @importFrom ComplexHeatmap merge_dendrogram Heatmap draw rowAnnotation
#' HeatmapAnnotation
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar grid.circle unit.c
#' @importFrom hdf5r H5File
#' @importFrom data.table rbindlist dcast.data.table data.table :=
scBubbHeat <- function(inpConf, inpMeta, inp, inpGrp, grpKey, grpVal, inpGrp1c, inpPlt,
                       dataset, inpGene, inpScl, inpRow, inpCol,
                       inpcols, flipXY,
                       plotAllCells = FALSE, save = FALSE,
                       colorBreaks,
                       legendTitle="expression",
                       returnColorRange=FALSE){
  # Identify genes that are in our dataset
  if(missing(inpGrp1c)) inpGrp1c <- 0
  if(inpPlt == "Bubbleplot"){
    plotAllCells <- FALSE
  }
  geneList <- scGeneList(inp, inpGene)
  geneList <- geneList[geneList$present == TRUE]
  shiny::validate(need(nrow(geneList) <= 500, "More than 500 genes to plot! Please reduce the gene list!"))
  shiny::validate(need(nrow(geneList) > 1, "Please input at least 2 genes to plot!"))
  #axis_fontsize <- round(min(c(500/nrow(geneList), 12), na.rm=TRUE), digits = 1)
  #bulb_pointsize <- min(c(round(400/nrow(geneList)), 8), na.rm=TRUE)

  # Prepare ggData
  h5file <- H5File$new(file.path(.globals$datafolder, dataset,
                                 .globals$filenames$sc1gexpr),
                       mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData <- data.table()
  for(iGene in geneList$gene){
    tmp <- inpMeta[, c("sampleID",
                       inpConf[inpConf$grp == TRUE]$ID),
                   with = FALSE]
    tmp$grpBy <- inpMeta[[inpConf[inpConf$UI == inpGrp]$ID]]
    tmp$geneName <- iGene
    tmp$val <- h5data$read(args = list(inpGene[iGene], quote(expr=)))
    ggData <- rbindlist(list(ggData, tmp))
  }
  h5file$close_all()

  ggData <- subGrp(ggData, grpKey, grpVal, inpConf)

  # Aggregate
  ggData$val <- expm1(ggData$val)
  ggData$val[is.infinite(ggData$val)] <-
    max(ggData$val[!is.infinite(ggData$val)], na.rm = TRUE)
  if(!plotAllCells){
    ggData <- ggData[, list(val = mean(.SD$val[.SD$val>=inpGrp1c]),
                            prop = sum(.SD$val>0) / length(.SD$sampleID)),
                    by = c("geneName", "grpBy")]
    ggDataAvg <- NULL
  }else{
    ggDataAvg <- ggData[, list(val = mean(.SD$val[.SD$val>=inpGrp1c]),
                               prop = sum(.SD$val>0) / length(.SD$sampleID)),
                       by = c("geneName", "grpBy")]
    ggDataAvg$val <- log1p(ggDataAvg$val)
  }
  ggData$val <- log1p(ggData$val)

  # Scale if required
  colRange <- range(ggData$val, na.rm=TRUE)
  colRange1 <- quantile(ggData$val, probs = c(0, .01, .5, .99, 1),
                       na.rm = TRUE,
                       names = FALSE)
  if(inpScl){
    ggData[, "val":= if(length(unique(.SD$val))==1){
        0
      } else {
        scale(.SD$val)
      }, keyby = "geneName"]
    colRange <- range(ggData$val, na.rm=TRUE)
    if(colRange[1]<0){
      colRange <- c(-max(abs(range(ggData$val, na.rm=TRUE))),
                    max(abs(range(ggData$val, na.rm=TRUE))))
    }else{
      colRange <- c(0, max(abs(range(ggData$val, na.rm=TRUE))))
    }
    colRange1 <- quantile(ggData$val,
                          probs = c(0, .01, .5, .99, 1),
                          na.rm = TRUE,
                          names = FALSE)
    colRange1 <- c(colRange[1], colRange1[-c(1, 5)], colRange[2])
  }
  if(returnColorRange){
    colRange1 <- colRange1[c(-3)]
    return(colRange1)
  }
  if(!is.na(colorBreaks[1])){
    if(colorBreaks[1]<colRange[1]) colorBreaks[1] <- colRange[1]
    if(colorBreaks[2]>colRange[2]) colorBreaks[2] <- colRange[2]
    ggData$val[ggData$val<colorBreaks[1]] <- colorBreaks[1]
    ggData$val[ggData$val>colorBreaks[2]] <- colorBreaks[2]
    if(colorBreaks[1]==colorBreaks[2]){
      col_fun <- .globals$cList[[inpcols]][1]
    }else{
      col_fun <- colorRamp2(
        breaks=seq(colorBreaks[1],
                   colorBreaks[2],
                   length.out=length(.globals$cList[[inpcols]])),
        colors = .globals$cList[[inpcols]])
    }
  }else{
    if(colRange[1]==colRange[2]){
      col_fun <- .globals$cList[[inpcols]][1]
    }else{
      col_fun <- colorRamp2(
        breaks=seq(colRange[1], colRange[2],
                   length.out=length(.globals$cList[[inpcols]])),
        colors = .globals$cList[[inpcols]])
    }
  }

  # reshape the data to matrix
  if(plotAllCells){
    ggData$grpBy <- paste(ggData$grpBy, ggData$sampleID, sep="__")
  }
  reshapeMat <- function(value.var){
    ggMatrix <- dcast.data.table(ggData,
                                 as.formula("geneName~grpBy"),
                                 value.var = value.var)
    tmp <- ggMatrix$geneName
    ggMatrix <- as.matrix(ggMatrix[, -1])
    ggMatrix[is.na(ggMatrix)] <- 0
    ggMatrix[is.infinite(ggMatrix)] <-
      .Machine$integer.max * sign(ggMatrix[is.infinite(ggMatrix)])
    rownames(ggMatrix) <- tmp
    return(ggMatrix)
  }
  ggMat =reshapeMat(value.var = "val")

  cluster_rows <- inpRow
  if(inpRow){
    cluster_rows <- as.dendrogram(hclust(dist(ggMat)))
  }
  cluster_columns <- inpCol
  if(inpCol){
    cluster_columns <- as.dendrogram(hclust(dist(t(ggMat))))
  }

  layer_fun <- NULL
  rect_gp <- gpar(col = NA)
  if(inpPlt == "Bubbleplot"){
    ggProp <- reshapeMat(value.var = "prop")
    layer_fun <- function(j, i, x, y, width, height, fill) {
      r <- min(unit.c(width, height), na.rm = TRUE)
      idx <- i+(j-1)*ncol(ggProp)
      g_prop <- as.vector(ggProp)[idx]
      grid.circle(x = x, y = y, r = abs(g_prop)/2 * r,
                  gp = gpar(fill = fill, col = NA))
    }
    rect_gp <- gpar(type = "none")
  }

  if(flipXY){
    ggMat <- t(ggMat)
    if(plotAllCells){
      group <- ggData$ident[match(rownames(ggMat),
                                  ggData$grpBy)]
      anno <- rowAnnotation(group=group, show_legend = FALSE,
                                show_annotation_name = FALSE)
      ht_list <- Heatmap(ggMat,
                         name = legendTitle,
                         col = col_fun,
                         heatmap_legend_param =
                           list(title=legendTitle,
                                direction = "horizontal",
                                title_position = "lefttop"),
                         cluster_rows = TRUE,
                         cluster_row_slices = inpCol,
                         cluster_columns = cluster_rows,
                         show_row_names = !plotAllCells,
                         row_split = group,
                         right_annotation = anno,
                         row_title_side = "right",
                         row_title_rot = 0,
                         column_names_rot = 45,
                         layer_fun = layer_fun,
                         rect_gp = rect_gp,
                         use_raster = TRUE)
    }else{
      ht_list <- Heatmap(ggMat,
                         name = legendTitle,
                         col = col_fun,
                         heatmap_legend_param =
                           list(title=legendTitle,
                                direction = "horizontal",
                                title_position = "lefttop"),
                         cluster_rows = cluster_columns,
                         cluster_columns = cluster_rows,
                         show_row_names = !plotAllCells,
                         column_names_rot = 45,
                         layer_fun = layer_fun,
                         rect_gp = rect_gp)
    }
  }else{
    if(plotAllCells){
      group <- ggData$ident[match(colnames(ggMat),
                                  ggData$grpBy)]
      anno <- HeatmapAnnotation(group=group, show_legend = FALSE,
                                show_annotation_name = FALSE)
      ht_list <- Heatmap(ggMat,
                         name = legendTitle,
                         col = col_fun,
                         heatmap_legend_param =
                           list(title=legendTitle,
                                direction = "horizontal",
                                title_position = "lefttop"),
                         cluster_rows = cluster_rows,
                         cluster_columns = TRUE,
                         cluster_column_slices = inpCol,
                         show_column_names = !plotAllCells,
                         column_split = group,
                         bottom_annotation = anno,
                         column_title_side = "bottom",
                         column_title_rot = 45,
                         layer_fun = layer_fun,
                         rect_gp = rect_gp,
                         use_raster = TRUE)
    }else{
      ht_list <- Heatmap(ggMat,
                         name = legendTitle,
                         col = col_fun,
                         heatmap_legend_param =
                           list(title=legendTitle,
                                direction = "horizontal",
                                title_position = "lefttop"),
                         cluster_rows = cluster_rows,
                         cluster_columns = cluster_columns,
                         show_column_names = !plotAllCells,
                         column_names_rot = 45,
                         layer_fun = layer_fun,
                         rect_gp = rect_gp)
    }
  }
  return(draw(ht_list,
              heatmap_legend_side = "bottom",
              annotation_legend_side = "bottom"))
}
