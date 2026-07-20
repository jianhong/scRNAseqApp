# Plot gene coexpression on dimred
bilinear <- function(x, y, xy, Q11, Q21, Q12, Q22) {
    oup <- (xy - x) * (xy - y) * Q11 + x * (xy - y) *
        Q21 + (xy - x) * y * Q12 + x *
        y * Q22
    oup <- oup / (xy * xy)
    return(oup)
}
#' @importFrom grDevices rgb
#' @importFrom data.table data.table
#' @importFrom plotly plot_ly layout
#' @importFrom ggplot2 ggplot aes .data geom_point xlab ylab scale_color_gradientn guides guide_colorbar coord_fixed scale_colour_identity scale_fill_identity
scDRcoex <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        gene1,
        gene2,
        subsetCellKey,
        subsetCellVal,
        dataset,
        geneIdMap,
        plotType,
        pointSize,
        GeneExprDotCol,
        GeneExprDotOrd,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        plotAspectRatio,
        keepXYlables,
        valueFilterKey,
        valueFilterCutoff,
        valueFilterCutoff2,
        hideFilterCell = FALSE,
        inpCellBorder=FALSE,# stereo-seq cell borders
        cellborderFilename='',
        cellSegAlpha=1,
        cellSegColor=NA,
        inpBgImg=FALSE,
        backgroundImage='',
        inpXlim=NULL,
        useNorm=FALSE,
        streamType='proportional',
        ...) {
    if (is.null(gene1) || is.null(gene2) || gene1 == "" || gene2 == "") {
        return(NULL)
    }
    subFilterColname <- 'subValue'
    subGrpColname <- 'sub'
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    # Prepare ggData
    ggData <- inpMeta[, unique(c(
        inpConf[inpConf$UI == dimRedX]$ID,
        inpConf[inpConf$UI == dimRedY]$ID,
        inpConf[inpConf$UI %in% subsetCellKey]$ID)),
        with = FALSE]
    if (nrow(ggData) == 0)
        return(NULL)
    if(ncol(ggData)<3){
        return(NULL)
    }
    colnames(ggData)[c(1, 2)] <- c("X", "Y")
    if (plotType == "3D") {
        ggData$sampleID <- inpMeta$sampleID
        inpCellBorder <- FALSE
        inpBgImg <- FALSE
    }
    
    checkCellSegmentationAvailability(environment())
    backgroundAlignFun <- checkBgImgAvailability(environment())
    
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
    
    dots <- list(...)
    lassoSelected <- rep(TRUE, nrow(ggData))
    if('selectedCellIDs' %in% names(dots)){
        if(length(dots$selectedCellIDs)){
            if(all(dots$selectedCellIDs %in% inpMeta$sampleID)){
                lassoSelected <- inpMeta$sampleID %in% dots$selectedCellIDs
            }
        }
    }
    
    ggData <- getCoexpVal(ggData, dataset, geneIdMap, gene1, gene2)
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff,
        valueFilterCutoff2,
        inpConf,
        lassoSelected=lassoSelected)
    
    colnames(ggData)[3] <- subGrpColname ## make the first subsetCellKey as sub
    bgCells <- sum(!keep) > 0
    if (bgCells) {
        ggData2 <- ggData[!keep]
        ggData <- ggData[keep]
    }
    ## color for group
    # Do factoring if required
    ggData <- relevelData(ggData, subGrpColname)
    if (plotType == "3D") {
        ggCol <- relevelCol(inpConf, subsetCellKey[1], ggData, subGrpColname)
    }
    
    # Generate coex color palette
    cInp <- strsplit(GeneExprDotCol, "; ")[[1]]
    
    nTot <- getTotalNumber(nGrid = 16, nPad = 2)
    gg <- getCoexpCol(GeneExprDotCol, nGrid = 16, nPad = 2)
    
    # Map colours
    if(length(ggData$val1)==0) return(NULL)
    if(length(ggData$val2)==0) return(NULL)
    mv1 <- max(ggData$val1, na.rm = TRUE)
    mv2 <- max(ggData$val2, na.rm = TRUE)
    ggData$v1 <-
        round(nTot * ggData$val1 / mv1)
    ggData$v2 <-
        round(nTot * ggData$val2 / mv2)
    ggData$v0 <- ggData$v1 + ggData$v2
    ggData <- gg[ggData, on = c("v1", "v2")]
    ggData <- orderGeneExpr(ggData, GeneExprDotOrd, 'v0')
    ggData <- ggData[!is.na(ggData$X) & !is.na(ggData$Y), ]
    if(nrow(ggData)==0) return(ggplot())
    ggData$cMix[is.na(ggData$cMix)] <- "snow2"
    
    # Actual ggplot
    if (plotType == "3D") {
        nTot <- max(c(ggData$val1, ggData$val2), na.rm = TRUE)
        ggData$norm1 <-
            round(nTot * ggData$val1 / mv1)
        ggData$norm2 <-
            round(nTot * ggData$val2 / mv2)
        ggData$Z <- log2(ggData$norm1 + 1) - log2(ggData$norm2 + 1)
        return(layout(
            plot_ly(
                x = ggData$X,
                y = ggData$Y,
                z = ggData$Z,
                customdata = ggData$sampleID,
                type = "scatter3d",
                mode = "markers",
                color = if (GeneExprDotCol == "Default") {
                    ggData[[subGrpColname]]
                } else{
                    ggData$Z
                },
                colors = if (GeneExprDotCol == "Default")
                    ggCol
                else
                    availableThemes(GeneExprDotCol),
                text = paste0(
                    gene1, ": ", ggData$val1, "\n",
                    gene2, ": ", ggData$val2),
                size = 1
            ),
            scene =
                list(
                    xaxis = list(title = dimRedX),
                    yaxis = list(title = dimRedY),
                    zaxis = list(title = paste0(
                        'Log2 Fold Change (',
                        gene1, '/', gene2, ')'
                    ))
                )
        ))
    }
    
    if (plotType == "Ridgeplot"){
        ggData[[subGrpColname]] <- factor(
            ggData[[subGrpColname]],
            levels = rev(sortLevels(as.character(
                unique(ggData[[subGrpColname]])
            ))))
        ## use raw value? val1, val2
        ggData1 <- ggData[, c(subGrpColname, 'val1'), with = FALSE]
        ggData2 <- ggData[, c(subGrpColname, 'val2'), with = FALSE]
        colnames(ggData1) <- colnames(ggData2) <- 
            c(subGrpColname, 'val')
        ggData1$group <- gene1
        ggData2$group <- gene2
        ## the cell id is ordered by sum of the two gene expression
        ggData1$cellID <- ggData2$cellID <- seq.int(nrow(ggData))
        ggData <- rbind(ggData1, ggData2)
        ggData[, "row_idx" := .I]
        first_occ <- ggData[, list(first_row = min(.SD$row_idx)),
                            by = c("group",subGrpColname, "cellID")]
        setorderv(first_occ, 'first_row')
        first_occ[, "idx" := seq_len(.N),
                  by = c("group", subGrpColname)]
        setnames(first_occ, "idx", "cell_idx")
        ggData[first_occ, "idx" := cell_idx,
               on = c("group",subGrpColname, "cellID")]
        ggData[["row_idx"]] <- NULL
        ggOut <- stream_plot(ggData, x='idx', y='val',
                             group='group',
                             groupY=subGrpColname,
                             normByTotal=useNorm,
                             type=streamType) +
            scale_y_discrete(expand = c(0.01, 0.01)) +
            scale_x_continuous(expand = c(0, 0)) +
            ylab(subsetCellKey) +
            xlab("cell ID")
        if (length(inpXlim) == 2) {
            ggOut <- ggOut + xlim(inpXlim)
        }
        return(ggOut)
    }
    
    ggData$val <- ggData$cMix
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

    ggOut <- ggOut +
        scale_colour_identity(guide = 'none') +
        scale_fill_identity(guide = 'none')
    
    ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
    return(ggOut)
}
