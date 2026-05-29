# Plot cell information on dimred
#' @noRd
#' @importFrom ggplot2 ggplot aes .data geom_point xlab ylab scale_color_gradientn guides guide_colorbar scale_color_manual guide_legend theme element_text coord_fixed geom_segment
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats weighted.mean
#' @importFrom slingshot SlingshotDataSet slingLineages slingClusterLabels slingMST slingParams
#' @importFrom data.table .SD
#' @importFrom SingleCellExperiment reducedDim
scDRcell <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        dimRedZ,
        cellinfoID,
        cellinfoName=cellinfoID,
        subsetCellKey,
        subsetCellVal,
        subsetCellPct=100,
        pointSize,
        gradientCol,
        GeneExprDotOrd,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        plotAspectRatio,
        keepXYlables,
        inplab,
        dataset,
        geneIdMap,
        valueFilterKey,
        valueFilterCutoff,
        valueFilterCutoff2,
        hideFilterCell=FALSE,
        inpSlingshot,
        slingshotFilename,
        inpShowEdge,#spring links, cell-cell graph edges
        edgeFilename,
        editorStatus,
        xlim=NULL,ylim=NULL,
        inpCellBorder=FALSE,# stereo-seq cell borders
        cellborderFilename='',
        cellSegAlpha=1,
        cellSegColor=NA,
        inpBgImg=FALSE,
        backgroundImage='',
        ...) {
    subFilterColname <- 'subValue'
    subGrpColname <- 'sub'
    valColname <- 'val'
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    if(cellinfoName!=cellinfoID){
        if(is.na(cellinfoName)||cellinfoName==""){
            cellinfoName <- cellinfoID
        }
    }
    # Prepare ggData
    plot3D <- isTRUE(dimRedZ %in% inpConf$UI)
    if(plot3D){
        ggData <- inpMeta[, unique(c(
            inpConf[inpConf$UI == dimRedX]$ID,
            inpConf[inpConf$UI == dimRedY]$ID,
            inpConf[inpConf$UI == dimRedZ]$ID,
            inpConf[inpConf$UI == cellinfoID]$ID,
            inpConf[inpConf$UI %in% subsetCellKey]$ID,
            inpConf[inpConf$UI == cellinfoName]$ID)),
            with = FALSE]
    }else{
        ggData <- inpMeta[, unique(c(
            inpConf[inpConf$UI == dimRedX]$ID,
            inpConf[inpConf$UI == dimRedY]$ID,
            inpConf[inpConf$UI == cellinfoID]$ID,
            inpConf[inpConf$UI %in% subsetCellKey]$ID,
            inpConf[inpConf$UI == cellinfoName]$ID)),
            with = FALSE]
    }
    
    if (ncol(ggData) < 3)
        return(ggplot())
    if(plot3D){
        colnames(ggData)[c(1,2,3)] <- c("X", "Y", "Z")
        cnid <- 4
        inpCellBorder <- FALSE
        inpBgImg <- FALSE
        inpShowEdge <- FALSE
    }else{
        colnames(ggData)[c(1,2)] <- c("X", "Y")
        cnid <- 3
    }
    dots <- list(...)
    if('interactive' %in% names(dots)){
        if(isTRUE(dots$interactive)){
            ggData$sampleID <- inpMeta$sampleID
        }
    }
    if(isTRUE(inpCellBorder)){
        if (file.exists(cellborderFilename)) {
            cellborder <- readRDS(cellborderFilename)
            # cellborderFilename must be a RDS filename.
            # The RDS file must be a list of cell borders.
            # The names of the list is the reductions name, such as umap
            if(is.list(cellborder)){
                dimRed_prefix <- sub('.$', '', dimRedX)
                if(dimRed_prefix==sub('.$', '', dimRedY)){
                    if(dimRed_prefix %in% names(cellborder)){
                        cellborder <- cellborder[[dimRed_prefix]]
                        if(!all(c('x', 'y', 'idx', 'sampleID') %in%
                                colnames(cellborder))){
                            showNotification(
                                "The cell border should be a table with
                                x, y, idx, sampleID",
                                type = 'error',
                                duration = 5)
                            inpCellBorder <- FALSE
                        }else{
                            ggData$sampleID <- inpMeta$sampleID
                        }
                    }else{
                        inpCellBorder <- FALSE
                    }
                }else{
                    inpCellBorder <- FALSE
                }
            }else{
                inpCellBorder <- FALSE
            }
        }else{
            inpCellBorder <- FALSE
        }
    }
    if(isTRUE(inpBgImg)){
        if(file.exists(backgroundImage)){
            backgroundImage <- readRDS(backgroundImage)
            if(is.list(backgroundImage)){
                dimRed_prefix <- sub('.$', '', dimRedX)
                if(dimRed_prefix==sub('.$', '', dimRedY)){
                    if(dimRed_prefix %in% names(backgroundImage)){
                        backgroundImage <- backgroundImage[[dimRed_prefix]]
                        backgroundAlignFun <- 
                            backgroundImage$FUN
                        if(is.null(backgroundAlignFun)){
                            backgroundAlignFun <- transformImage
                        }
                        backgroundAlignArgs <-
                            backgroundImage$args
                        backgroundImage <- backgroundImage$raster_df
                        if(!all(c('x', 'y', 'value') %in% 
                                colnames(backgroundImage))){
                            showNotification(
                                "The background should be a table with
                                x, y, value",
                                type = 'error',
                                duration = 5)
                            inpBgImg <- FALSE
                        }
                    }else{
                        inpBgImg <- FALSE
                    }
                }else{
                    inpBgImg <- FALSE
                }
            }else{
                inpBgImg <- FALSE
            }
        }else{
            inpBgImg <- FALSE
        }
    }
    lassoSelected <- rep(TRUE, nrow(ggData))
    if('selectedCellIDs' %in% names(dots)){
        if(length(dots$selectedCellIDs)){
            if(all(dots$selectedCellIDs %in% inpMeta$sampleID)){
                lassoSelected <- inpMeta$sampleID %in% dots$selectedCellIDs
            }
        }
    }
    ggData <-
        cbindFilterValues(
            ggData,
            inpConf,
            inpMeta,
            subFilterColname,
            geneIdMap,
            dataset,
            valueFilterKey,
            valueFilterCutoff,
            valueFilterCutoff2
        )
    rat <- getRatio(ggData)
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff,
        valueFilterCutoff2,
        inpConf, 
        subsetCellPct,
        lassoSelected)
    
    if(length(subsetCellKey)==0){
        return(ggplot())
    }
    if(subsetCellKey[1]==cellinfoID){
        subGrpColname <- valColname
        colnames(ggData)[cnid] <- valColname
    }else{## make the first subsetCellKey as sub
        if(ncol(ggData)<cnid+1){
            return(ggplot())
        }
        colnames(ggData)[c(cnid,cnid+1)] <- c(valColname, subGrpColname)
    }
    bgCells <- sum(!keep) > 0
    
    if(inpShowEdge){
        if (file.exists(edgeFilename)) {
            edges <- readRDS(edgeFilename)
            # edgeFilename must be a RDS filename. The RDS file must be a list of 
            # edges. The names of the list is the reductions name, such as umap
            if(!is.list(edges)){
                inpShowEdge <- FALSE
            }
            dimRed_prefix <- sub('.$', '', dimRedX)
            if(dimRed_prefix!=sub('.$', '', dimRedY)){
                inpShowEdge <- FALSE
            }
            if(!dimRed_prefix %in% names(edges)){
                inpShowEdge <- FALSE
            }else{
                edges <- edges[[dimRed_prefix]]
            }
            if(inpShowEdge){
                ggData$idx <- seq.int(nrow(ggData))
            }
        }else{
            inpShowEdge <- FALSE
        }
    }
    
    if (bgCells) {
        ggData2 <- ggData[!keep]
        ggData <- ggData[keep]
    }
    
    ggData <- orderGeneExpr(ggData, GeneExprDotOrd, valColname)
    
    # Do factoring if required
    ggData <- relevelData(ggData, valColname)
    ggCol <- relevelCol(inpConf, cellinfoID, ggData, valColname)
    
    if(cellinfoName!=cellinfoID){
        if(cellinfoName==subsetCellKey[1]){
            cellinfoName <- subGrpColname
        }
        tab <- unique(ggData[, c(valColname, cellinfoName), with = FALSE])
        if(length(levels(ggData[[valColname]])) != nrow(tab)){
            # showNotification(paste(
            #     'The length of cell labels are not identical',
            #     'with that of the cell info IDs'),
            #     type = "warning")
            cellinfoName <- names(ggCol)
        }else{
            cellinfoName <- tab[match(names(ggCol), tab[[valColname]]),
                                cellinfoName, with = FALSE]
            cellinfoName <- as.character(cellinfoName[[1]])
        }
    }else{
        cellinfoName <- names(ggCol)
    }
    
    if(plot3D){
        return(layout(
            plot_ly(
                x = ggData$X,
                y = ggData$Y,
                z = ggData$Z,
                text = ggData$val,
                type = "scatter3d",
                mode = "markers",
                color = ggData$val,
                colors = ggCol,
                size = pointSize * 3
            ),
            scene =
                list(
                    xaxis = list(title = dimRedX),
                    yaxis = list(title = dimRedY),
                    zaxis = list(title = dimRedZ)
                )
        )) 
    }
    
    # Actual ggplot
    if(isTRUE(inpBgImg)){
        backgroundAlignArgs$plot_data <- ggData
        ggData <- do.call(backgroundAlignFun, backgroundAlignArgs)
        if(inpCellBorder){
            backgroundAlignArgs$plot_data <- cellborder
            cellborder <- do.call(backgroundAlignFun, backgroundAlignArgs)
        }
        ggOut <- ggXYplot(ggData, backgroundImage)
    }else{
        ggOut <- ggXYplot(ggData) 
    }
    if (bgCells) {
        ggOut <- labelBackgroundCells(
            ggOut,
            ggData2,
            pointSize,
            color = "snow2",
            shape = 16,
            hide = hideFilterCell)
    }
    
    if (inpShowEdge) {
        keep <- edges[, 1] %in% ggData$idx & edges[, 2] %in% ggData$idx
        if(sum(keep)){
            edgeDf <- cbind(ggData[match(edges[keep, 1], ggData$idx), c('X', 'Y')],
                            ggData[match(edges[keep, 2], ggData$idx), c('X', 'Y')])
            colnames(edgeDf) <- c('x', 'y', 'xend', 'yend')
            if(isTRUE(inpBgImg)){
                backgroundAlignArgs$plot_data <- edgeDf
                edgeDf <- do.call(backgroundAlignFun,
                                  backgroundAlignArgs)
            }
            
            ggOut <- ggOut +
                geom_segment(
                    data = edgeDf,
                    aes(
                        x = .data[["x"]],
                        y = .data[["y"]],
                        xend = .data[["xend"]],
                        yend = .data[["yend"]]
                    ),
                    inherit.aes = FALSE,
                    linewidth = 0.5,
                    colour = '#CCCCCC',
                    alpha = 0.5
                )
        }
        
    }
    
    ggOut <- pointPlot(
        ggOut = ggOut,
        pointSize = pointSize,
        fontSize = labelsFontsize,
        labelsFontFamily = labelsFontFamily,
        dimRedX = dimRedX,
        dimRedY = dimRedY,
        keepXYlables = keepXYlables,
        inpCellBorder = inpCellBorder,
        cellborder = cellborder,
        cellSegColor = cellSegColor,
        cellSegAlpha = cellSegAlpha)
    # slingshot
    if (inpSlingshot) {
        if (file.exists(slingshotFilename)) {
            lineages <- readRDS(slingshotFilename)
            if (cellinfoID %in% names(lineages)) {
                lineages <- lineages[[cellinfoID]]
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
                        if(isTRUE(inpBgImg)){
                            backgroundAlignArgs$plot_data <- lineDf
                            lineDf <- do.call(backgroundAlignFun,
                                              backgroundAlignArgs)
                            backgroundAlignArgs$plot_data <- pts
                            pts <- do.call(backgroundAlignFun,
                                           backgroundAlignArgs)
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
    if (is.na(inpConf[inpConf$UI == cellinfoID]$fCL)) {
        ggOut <- ggOut +
            scale_color_gradientn("", colours = availableThemes(gradientCol))+
            guides(color = guide_colorbar(barwidth = 15))
        if(isTRUE(inpCellBorder)){
            ggOut <- ggOut +
                scale_fill_gradientn("", colours = availableThemes(gradientCol)) 
        }
    } else {
        ggOut <- ggOut + scale_color_manual(
            "", values = ggCol, labels=cellinfoName) +
            theme(legend.text = element_text(size = labelsFontsize,
                                             family = labelsFontFamily))
        if(isTRUE(inpCellBorder)){
            ggOut <- ggOut + 
                scale_fill_manual(
                    "", values = ggCol, labels=cellinfoName) 
        }
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
                    nrow = inpConf[inpConf$UI == cellinfoID]$fRow
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
                    size = labelsFontsize*.3508333,
                    family = labelsFontFamily,
                    seed = 123
                )
        }
    }
    ggOut <- fixCoord(ggOut, plotAspectRatio, rat, xlim, ylim)
    return(ggOut)
}
