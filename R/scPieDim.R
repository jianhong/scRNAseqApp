# Plot gene expression scatter_pie
#' @importFrom data.table rbindlist
#' @importFrom scales rescale
#' @importFrom ggforce geom_arc_bar geom_circle geom_mark_hull
#' @importFrom ggplot2 geom_rect .data
scPieDim <- function(
        inpConf,
        inpMeta,
        dataset,
        geneIdMap,
        dimRedX,
        dimRedY,
        genelist,
        subsetCellKey,
        subsetCellVal,
        valueFilterKey,
        valueFilterCutoff,
        CoExpred,
        pointSize,
        lableCircle,
        plotCellBg,
        markGrp,
        alpha,
        plotType,
        labelsFontsize,
        plotAspectRatio,
        keepXYlables) {
    subFilterColname <- 'subValue'
    subGrpColname <- 'sub'
    geneList <- scGeneList(genelist, geneIdMap)
    geneList <- geneList[geneList$present == TRUE]
    shiny::validate(need(
        nrow(geneList) <= 10,
        "More than 10 genes to plot! Please reduce the gene list!"
    ))
    shiny::validate(need(
        nrow(geneList) > 1, 
        "Please input at least 2 genes to plot!"))
    
    # Prepare ggData
    ggData <- inpMeta[, c(
        "sampleID",
        inpConf[inpConf$UI == dimRedX]$ID,
        inpConf[inpConf$UI == dimRedY]$ID,
        inpConf[inpConf$UI %in% subsetCellKey]$ID),
        with = FALSE]
    if (nrow(ggData) == 0)
        return(NULL)
    if(ncol(ggData)<4){
        return(NULL)
    }
    cnid <- if(ncol(ggData)>3) 4 else 0
    colnames(ggData)[seq.int(3)] <- c("sampleID", "X", "Y")
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
    expr <-
        lapply(geneIdMap[geneList$gene][
            seq.int(min(c(10, length(geneList$gene))))],
            read_exprs,
            h5f = dataset,
            valueOnly = TRUE)
    expr <- do.call(cbind, expr)
    expr[expr < 0] <- 0
    expr_keep <- rowSums(expr) > 0
    if (CoExpred)
        expr_keep <- expr_keep & rowSums(expr > 0) == ncol(expr)
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff)
    
    if(cnid>3) colnames(ggData)[cnid] <- subGrpColname
    
    if (sum(keep & expr_keep) * nrow(geneList) > 5000) {
        ## filter the expression event, otherwise too slow
        expr_keep <- rowSums(expr)
        expr_keep <-
            expr_keep >=
            sort(expr_keep, decreasing = TRUE)[floor(1000 / nrow(geneList))]
    }
    ggData_ <- ggData[keep & expr_keep]
    expr <- expr[keep & expr_keep, , drop = FALSE]
    ggData <- ggData[keep]
    size <- diff(range(ggData$X)) / 60 * pointSize
    expr <- apply(expr, 2, rescale, to = c(size / 4, size))
    expr <- as.list(as.data.frame(expr))
    expr <- mapply(function(d, n) {
        cbind(ggData_, geneName = n, "val" = d)
    }, expr, names(expr), SIMPLIFY = FALSE)
    expr <- rbindlist(expr)
    
    expr$X0 <-
        expr$X + (as.numeric(factor(as.character(expr$geneName))) - 1) * size /
        2
    ggOut <- ggplot(data = expr, aes(x0 = .data[["X"]], y0 = .data[["Y"]]))
    if (markGrp) {
        ggOut <- ggOut +
            geom_mark_hull(
                data = ggData,
                aes(
                    x = .data[["X"]],
                    y = .data[["Y"]],
                    fill = .data[[subGrpColname]],
                    label = .data[[subGrpColname]]
                ),
                inherit.aes = FALSE,
                show.legend = FALSE
            )
    }
    if (plotCellBg) {
        ggOut <- ggOut +
            geom_point(
                data = ggData,
                aes(x = .data$X, y = .data$Y),
                color = 'snow2',
                shape = 16,
                inherit.aes = FALSE
            )
    }
    ggOut <- ggOut +
        switch(
            plotType,
            sunburst =
                geom_arc_bar(
                    aes(
                        amount = 1,
                        r0 = 0,
                        r = .data$val,
                        fill = .data$geneName
                    ),
                    stat = 'pie',
                    color = NA,
                    alpha = alpha
                ),
            pie =
                geom_arc_bar(
                    aes(
                        amount = .data$val,
                        r0 = 0,
                        r = size,
                        fill = .data$geneName
                    ),
                    stat = 'pie',
                    color = NA,
                    alpha = alpha
                ),
            donut =
                geom_arc_bar(
                    aes(
                        amount = .data$val,
                        r0 = size / 2,
                        r = size,
                        fill = .data$geneName
                    ),
                    stat = 'pie',
                    color = NA,
                    alpha = alpha
                ),
            bar =
                geom_rect(
                    aes(
                        xmin = .data$X0 - .02 * size,
                        ymin = .data$Y,
                        xmax = .data$X0 + .46 * size,
                        ymax = .data$Y + .data$val,
                        fill = .data$geneName
                    ),
                    position = "identity",
                    alpha = alpha,
                    color = NA
                )
        ) +
        guides(
            fill = guide_legend(title = 'genename'),
            alpha = "none")
    
    if (lableCircle) {
        if (plotType != "bar") {
            ggOut <- ggOut +
                geom_circle(
                    aes(color = .data$sub, r = size),
                    fill = NA,
                    lwd = .5) +
                guides(color = guide_legend(title = subsetCellKey))
        } else{
            ggOut <- ggOut +
                geom_rect(
                    aes(
                        xmin = .data$X - .025 * size,
                        ymin = .data$Y - .05,
                        xmax = .data$X + (nrow(geneList) + .05) * size / 2,
                        ymax = .data$Y + size * 1.05,
                        color = .data$sub
                    ),
                    fill = NA,
                    linewidth = .5
                ) +
                guides(color = guide_legend(title = subsetCellKey))
        }
    }
    ggOut <- ggOut +
        xlab(dimRedX) + ylab(dimRedY) +
        sctheme(
            base_size = .globals$sList[labelsFontsize], XYval = keepXYlables)
    ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
    return(ggOut)
}
