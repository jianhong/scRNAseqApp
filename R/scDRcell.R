# Plot cell information on dimred
#' @noRd
#' @importFrom ggplot2 ggplot aes_string geom_point xlab ylab scale_color_gradientn
#' guides guide_colorbar scale_color_manual guide_legend theme element_text
#' coord_fixed geom_segment
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats weighted.mean
#' @importFrom slingshot SlingshotDataSet slingLineages slingClusterLabels
#' slingMST slingParams
#' @importFrom data.table .SD
#' @importFrom SingleCellExperiment reducedDim
scDRcell <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab,
                     inpSlingshot, slingshotFilename){
  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[inpConf$UI == inpdrX]$ID,
                        inpConf[inpConf$UI == inpdrY]$ID,
                        inpConf[inpConf$UI == inp1]$ID,
                        inpConf[inpConf$UI == inpsub1]$ID),
                   with = FALSE]
  if(ncol(ggData)!=4) return(ggplot())
  colnames(ggData) <- c("X", "Y", "val", "sub")
  rat <- getRatio(ggData)
  bgCells <- FALSE
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    bgCells <- TRUE
    ggData2 <- ggData[!ggData$sub %in% inpsub2]
    ggData <- ggData[ggData$sub %in% inpsub2]
  }
  if(inpord == "Max-1st"){
    ggData <- ggData[order(ggData$val)]
  } else if(inpord == "Min-1st"){
    ggData <- ggData[order(-ggData$val)]
  } else if(inpord == "Random"){
    ggData <- ggData[sample(nrow(ggData))]
  }

  # Do factoring if required
  if(!is.na(inpConf[inpConf$UI == inp1]$fCL)){
    ggCol <- strsplit(inpConf[inpConf$UI == inp1]$fCL, "\\|")[[1]]
    names(ggCol) <- levels(ggData$val)
    ggLvl <- levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)]
    ggData$val <- factor(ggData$val, levels = ggLvl)
    ggCol <- ggCol[ggLvl]
  }

  # Actual ggplot
  ggOut <- ggplot(ggData, aes_string("X", "Y", color = "val"))
  if(bgCells){
    ggOut <- ggOut +
      geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16)
  }
  ggOut <- ggOut +
    geom_point(size = inpsiz, shape = 16) +
    xlab(inpdrX) + ylab(inpdrY) +
    sctheme(base_size = .globals$sList[inpfsz], XYval = inptxt)
  # slingshot
  if(inpSlingshot){
    if(file.exists(slingshotFilename)){
      lineages <- readRDS(slingshotFilename)
      if(inp1 %in% names(lineages)){
        lineages <- lineages[[inp1]]
        x <- SlingshotDataSet(lineages)
        X <- reducedDim(x)
        checkRedDimName <- function(a, b){
          a <- gsub('[^a-z]', '', tolower(a))
          b <- gsub('[^a-z]', '', tolower(b))
          a %in% b
        }
        if(checkRedDimName(inpdrX, colnames(X)) &&
           checkRedDimName(inpdrY, colnames(X))){
          linInd <- seq_along(slingLineages(x))
          clusterLabels <- slingClusterLabels(x)
          connectivity <- slingMST(x)
          clusters <- rownames(connectivity)
          nclus <- nrow(connectivity)
          centers <- t(vapply(clusters,function(clID){
            w <- clusterLabels[,clID]
            return(apply(X, 2, weighted.mean, w = w))
          }, rep(0,ncol(X))))
          rownames(centers) <- clusters
          X <- X[rowSums(clusterLabels) > 0, , drop = FALSE]
          clusterLabels <- clusterLabels[rowSums(clusterLabels) > 0, ,
                                         drop = FALSE]
          linC <- slingParams(x)
          clus2include <- unique(unlist(slingLineages(x)[linInd]))
          lineDf <- data.frame()
          for(i in seq_len(nclus-1)){
            for(j in seq(i+1,nclus)){
              if(connectivity[i,j]==1 &
                 all(clusters[c(i,j)] %in% clus2include)){
                lineDf <- rbind(lineDf, c(centers[i, 1:2, drop=TRUE],
                                          centers[j, 1:2, drop=TRUE]))
              }
            }
          }
          colnames(lineDf) <- c("x", "y", "xend", "yend")
          pts <- centers[clusters %in% clus2include, 1:2]
          colnames(pts) <- c("x", "y")
          pts <- cbind(as.data.frame(pts), color='black')
          if(any(linC$start.given)){
            if(length(linC$start.clus[linC$start.given])>0){
              pts[linC$start.clus[linC$start.given], "color"] <- "green3"
            }
          }
          if(any(linC$end.given)){
            if(length(linC$end.clus[linC$end.given])>0){
              pts[linC$end.clus[linC$end.given], "color"] <- "red2"
            }
          }
          ggOut <- ggOut +
            geom_segment(data=lineDf, aes_string(x="x", y="y",
                                                 xend="xend", yend="yend"),
                         inherit.aes=FALSE) +
            geom_point(data=pts, aes_string(x="x", y="y", color="color"),
                       size = inpsiz*3, alpha=.5,
                       inherit.aes = FALSE)
        }
      }
    }
  }
  # label
  if(is.na(inpConf[inpConf$UI == inp1]$fCL)){
    ggOut <- ggOut +
      scale_color_gradientn("", colours = .globals$cList[[inpcol]]) +
      guides(color = guide_colorbar(barwidth = 15))
  } else {
    sListX <- min(nchar(paste0(levels(ggData$val), collapse = "")), 200)
    sListX <- 0.75 * (.globals$sList - (1.5 * floor(sListX/50)))
    ggOut <- ggOut + scale_color_manual("", values = ggCol) +
      guides(color = guide_legend(override.aes = list(size = 5),
                                  nrow = inpConf[inpConf$UI == inp1]$fRow)) +
      theme(legend.text = element_text(size = sListX[inpfsz]))
    if(inplab){
      ggData3 <- ggData[, list(X = mean(.SD$X),
                               Y = mean(.SD$Y)),
                        by = "val"]
      lListX <- min(nchar(paste0(ggData3$val, collapse = "")), 200)
      lListX <- .globals$lList - (0.25 * floor(lListX/50))
      ggOut <- ggOut +
        geom_text_repel(data = ggData3,
                        aes_string("X", "Y", label = "val"),
                        color = "grey10",
                        bg.color = "grey95",
                        bg.r = 0.15,
                        size = lListX[inpfsz],
                        seed = 123)
    }
  }
  if(inpasp == "Square") {
    ggOut <- ggOut + coord_fixed(ratio = rat)
  } else if(inpasp == "Fixed") {
    ggOut <- ggOut + coord_fixed()
  }
  return(ggOut)
}
