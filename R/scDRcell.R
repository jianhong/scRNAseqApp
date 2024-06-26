# Plot cell information on dimred
#' @noRd
#' @importFrom ggplot2 ggplot aes .data geom_point xlab ylab
#' scale_color_gradientn guides guide_colorbar scale_color_manual
#' guide_legend theme element_text coord_fixed geom_segment
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats weighted.mean
#' @importFrom slingshot SlingshotDataSet slingLineages slingClusterLabels
#' slingMST slingParams
#' @importFrom data.table .SD
#' @importFrom SingleCellExperiment reducedDim
scDRcell <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        inp1,
        subsetCellKey,
        subsetCellVal,
        pointSize,
        gradientCol,
        GeneExprDotOrd,
        labelsFontsize,
        plotAspectRatio,
        keepXYlables,
        inplab,
        dataset,
        geneIdMap,
        valueFilterKey,
        valueFilterCutoff,
        hideFilterCell=FALSE,
        inpSlingshot,
        slingshotFilename,
        ...) {
    subFilterColname <- 'subValue'
    subGrpColname <- 'sub'
    valColname <- 'val'
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    # Prepare ggData
    ggData <- inpMeta[, unique(c(
        inpConf[inpConf$UI == dimRedX]$ID,
        inpConf[inpConf$UI == dimRedY]$ID,
        inpConf[inpConf$UI == inp1]$ID,
        inpConf[inpConf$UI %in% subsetCellKey]$ID)),
        with = FALSE]
    if (ncol(ggData) < 3)
        return(ggplot())
    colnames(ggData)[c(1,2)] <- c("X", "Y")
    ggData <-
        cbindFilterValues(
            ggData,
            inpConf,
            inpMeta,
            subFilterColname,
            geneIdMap,
            dataset,
            valueFilterKey,
            valueFilterCutoff
        )
    rat <- getRatio(ggData)
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff,
        inpConf)
    
    if(subsetCellKey[1]==inp1){
        subGrpColname <- valColname
        colnames(ggData)[3] <- valColname
    }else{## make the first subsetCellKey as sub
        if(ncol(ggData)<4){
            return(ggplot())
        }
        colnames(ggData)[c(3,4)] <- c(valColname, subGrpColname)
    }
    bgCells <- sum(!keep) > 0
    
    if (bgCells) {
        ggData2 <- ggData[!keep]
        ggData <- ggData[keep]
    }
    
    ggData <- orderGeneExpr(ggData, GeneExprDotOrd, valColname)
    
    # Do factoring if required
    ggData <- relevelData(ggData, valColname)
    ggCol <- relevelCol(inpConf, inp1, ggData, valColname)
    
    # Actual ggplot
    ggOut <- ggXYplot(ggData)
    if (bgCells) {
        ggOut <- labelBackgroundCells(
            ggOut,
            ggData2,
            pointSize,
            color = "snow2",
            shape = 16,
            hide = hideFilterCell)
    }
    ggOut <- pointPlot(
        ggOut,
        pointSize,
        labelsFontsize,
        dimRedX,
        dimRedY,
        keepXYlables)
    # slingshot
    if (inpSlingshot) {
        if (file.exists(slingshotFilename)) {
            lineages <- readRDS(slingshotFilename)
            if (inp1 %in% names(lineages)) {
                lineages <- lineages[[inp1]]
                x <- SlingshotDataSet(lineages)
                X <- reducedDim(x)
                checkRedDimName <- function(a, b) {
                    a <- gsub('[^a-z]', '', tolower(a))
                    b <- gsub('[^a-z]', '', tolower(b))
                    a %in% b
                }
                if (checkRedDimName(dimRedX, colnames(X)) &&
                    checkRedDimName(dimRedY, colnames(X))) {
                    linInd <- seq_along(slingLineages(x))
                    clusterLabels <- slingClusterLabels(x)
                    connectivity <- slingMST(x)
                    clusters <- rownames(connectivity)
                    nclus <- nrow(connectivity)
                    if (nclus > 1) {
                        centers <- t(vapply(clusters, function(clID) {
                            w <- clusterLabels[, clID]
                            return(apply(X, 2, weighted.mean, w = w))
                        }, rep(0, ncol(
                            X
                        ))))
                        rownames(centers) <- clusters
                        X <- X[rowSums(clusterLabels) > 0, , drop = FALSE]
                        clusterLabels <-
                            clusterLabels[
                                rowSums(clusterLabels) > 0, ,
                                drop = FALSE]
                        linC <- slingParams(x)
                        clus2include <- unique(unlist(slingLineages(x)[linInd]))
                        lineDf <- data.frame()
                        for (i in seq_len(nclus - 1)) {
                            for (j in seq(i + 1, nclus)) {
                                if (connectivity[i, j] == 1 &&
                                    all(clusters[c(i, j)] %in% clus2include,
                                        na.rm = TRUE)) {
                                    lineDf <-
                                        rbind(
                                            lineDf, c(
                                                centers[
                                                    i, c(1, 2), drop = TRUE],
                                                centers[
                                                    j, c(1, 2), drop = TRUE]))
                                }
                            }
                        }
                        colnames(lineDf) <- c("x", "y", "xend", "yend")
                        pts <- centers[clusters %in% clus2include, c(1, 2)]
                        colnames(pts) <- c("x", "y")
                        pts <- cbind(as.data.frame(pts), color = 'black')
                        if (any(linC$start.given)) {
                            if (length(linC$start.clus[linC$start.given]) > 0) {
                                pts[linC$start.clus[linC$start.given],
                                    "color"] <- "green3"
                            }
                        }
                        if (any(linC$end.given)) {
                            if (length(linC$end.clus[linC$end.given]) > 0) {
                                pts[linC$end.clus[linC$end.given],
                                    "color"] <- "red2"
                            }
                        }
                        ggOut <- ggOut +
                            geom_segment(
                                data = lineDf,
                                aes(
                                    x = .data[["x"]],
                                    y = .data[["y"]],
                                    xend = .data[["xend"]],
                                    yend = .data[["yend"]]
                                ),
                                inherit.aes = FALSE
                            ) +
                            geom_point(
                                data = pts,
                                aes(
                                    x = .data[["x"]],
                                    y = .data[["y"]],
                                    color = .data[["color"]]
                                ),
                                size = pointSize * 3,
                                alpha = .5,
                                inherit.aes = FALSE
                            )
                    }
                }
            }
        }
    }
    # label
    if (is.na(inpConf[inpConf$UI == inp1]$fCL)) {
        ggOut <- ggOut +
            scale_color_gradientn("", colours = .globals$cList[[gradientCol]]) +
            guides(color = guide_colorbar(barwidth = 15))
    } else {
        sListX <- min(nchar(paste0(
            levels(ggData[[valColname]]),
            collapse = "")), 200)
        sListX <- 0.75 * (.globals$sList - (1.5 * floor(sListX / 50)))
        ggOut <- ggOut + scale_color_manual("", values = ggCol) +
            theme(legend.text = element_text(size = sListX[labelsFontsize]))
        if(length(ggCol)>50){
            ggOut <- ggOut +
                guides(color = "none")
            showNotification(paste('Too many overlapping labels.',
                                   'Not all labels can be show!',
                                   'The legend are also removed.',
                                   'Consider filter out some data points.'),
                             type = "warning")
        }else{
            ggOut <- ggOut +
                guides(color = guide_legend(
                    override.aes = list(size = 5),
                    nrow = inpConf[inpConf$UI == inp1]$fRow
                ))
        }
        if (inplab) {
            ggData3 <- 
                ggData[, list(
                    X = mean(.SD$X),
                    Y = mean(.SD$Y)), by = valColname]
            lListX <-
                min(nchar(paste0(ggData3[[valColname]], collapse = "")), 200)
            lListX <- .globals$lList - (0.25 * floor(lListX / 50))
            ggOut <- ggOut +
                geom_text_repel(
                    data = ggData3,
                    aes(
                        .data[["X"]], .data[["Y"]],
                        label = .data[[valColname]]),
                    color = "grey10",
                    bg.color = "grey95",
                    bg.r = 0.15,
                    size = lListX[labelsFontsize],
                    seed = 123
                )
        }
    }
    ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
    return(ggOut)
}
