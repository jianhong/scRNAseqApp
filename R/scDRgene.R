# Plot gene expression on dimred
#' @importFrom ggplot2 ggplot aes .data geom_point xlab ylab guides
#' guide_colorbar scale_color_gradientn coord_fixed scale_y_discrete
#' scale_x_continuous xlim
#' @importFrom ggridges geom_density_ridges theme_ridges
scDRgene <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        gene1,
        subsetCellKey,
        subsetCellVal,
        dataset,
        geneIdMap,
        pointSize,
        gradientCol,
        GeneExprDotOrd,
        labelsFontsize,
        plotAspectRatio,
        keepXYlables,
        inpPlt = "Dotplot",
        inpXlim,
        inpColRange = 0,
        valueFilterKey,
        valueFilterCutoff,
        ...) {
    if (gene1[1] == "") {
        return(ggplot())
    }
    if (is.na(geneIdMap[gene1])) {
        return(ggplot())
    }
    if (is.null(geneIdMap[gene1])) {
        return(ggplot())
    }
    # Prepare ggData
    subFilterColname <- 'subValue'
    subGrpColname <- 'sub'
    exprColname <- 'val'
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    ggData <- inpMeta[, c(
        inpConf[inpConf$UI == dimRedX]$ID,
        inpConf[inpConf$UI == dimRedY]$ID,
        inpConf[inpConf$UI %in% subsetCellKey]$ID),
        with = FALSE]
    cnid <- if(ncol(ggData)>2) 3 else 0
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
    
    ggData[[exprColname]] <- read_exprs(
        dataset,
        geneIdMap[gene1],
        valueOnly = TRUE)
    if (any(ggData[[exprColname]] < 0)) {
        ggData[ggData[[exprColname]] < 0][[exprColname]] <- 0
    }
    
    keep <- filterCells(
        ggData[, -c(1, 2)],
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff
    )
    
    ## make the first subsetCellKey as sub
    colnames(ggData)[c(1, 2)] <- c("X", "Y")
    if(cnid>2) colnames(ggData)[cnid] <- subGrpColname
    rat <- getRatio(ggData)
    bgCells <- sum(!keep) > 0
    
    if (bgCells) {
        ggData2 <- ggData[!keep]
        ggData <- ggData[keep]
    }
    ggData <- orderGeneExpr(ggData, GeneExprDotOrd, exprColname)
    
    if (is.logical(inpColRange[1])) {
        return(range(ggData[[exprColname]]))
    }
    # Actual ggplot
    if (inpPlt == "Dotplot") {
        if (length(inpColRange) == 1) {
            inpColRange <- c(min(
                0, min(inpColRange, na.rm = TRUE),
                na.rm = TRUE),
                inpColRange)
        }
        if (inpColRange[2] > 0) {
            ggData[ggData[[exprColname]] > inpColRange[2], exprColname] <-
                inpColRange[2]
        }
        ggOut <- ggXYplot(ggData)
        if (bgCells) {
            ggOut <- labelBackgroundCells(
                ggOut,
                ggData2,
                pointSize,
                color = "snow2",
                shape = 16)
        }
        ggOut <- pointPlot(
            ggOut,
            pointSize,
            labelsFontsize,
            dimRedX,
            dimRedY,
            keepXYlables) +
            guides(color = guide_colorbar(barwidth = 15))
        if (inpColRange[2] > 0) {
            ggOut <- ggOut +
                scale_color_gradientn(
                    gene1,
                    colours = .globals$cList[[gradientCol]],
                    limits = inpColRange)
        } else{
            ggOut <- ggOut +
                scale_color_gradientn(
                    gene1,
                    colours = .globals$cList[[gradientCol]])
        }
        ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
    } else{
        ## ridgePlot
        ggData[[subGrpColname]] <- factor(
            ggData[[subGrpColname]],
            levels = rev(sortLevels(as.character(
                unique(ggData[[subGrpColname]])
            ))))
        ggOut <- ggplot(
            ggData,
            aes(
                x = .data[[exprColname]],
                y = .data[[subGrpColname]],
                fill = .data[[subGrpColname]]
            )) +
            geom_density_ridges(scale = 4, show.legend = FALSE) +
            theme_ridges() +
            scale_y_discrete(expand = c(0.01, 0)) +
            scale_x_continuous(expand = c(0, 0)) +
            ylab("Subsets") +
            xlab("Expression Level")
        if (length(inpXlim) == 2) {
            ggOut <- ggOut + xlim(inpXlim)
        }
    }
    
    return(ggOut)
}
