# Plot gene expression scatter_pie
#' @importFrom data.table rbindlist
#' @importFrom scales rescale
#' @importFrom ggforce geom_arc_bar geom_circle geom_mark_hull
#' @importFrom ggplot2 geom_rect .data geom_density_2d_filled geom_density_2d
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
        valueFilterCutoff2,
        CoExpred,
        pointSize,
        lableCircle,
        plotCellBg,
        markGrp,
        alpha,
        gradientCol="White-Red",
        plotType,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        plotAspectRatio,
        keepXYlables,
        sunburst_type='expr',
        streamType="ridge",
        ...) {
    subFilterColname <- 'subValue'
    subGrpColname <- 'sub'
    if(sunburst_type=='expr'){
        geneList <- scGeneList(genelist, geneIdMap)
        geneList <- geneList[geneList$present == TRUE]
        shiny::validate(need(
            nrow(geneList) <= 10,
            "More than 10 genes to plot! Please reduce the gene list!"
        ))
        shiny::validate(need(
            nrow(geneList) > 1, 
            "Please input at least 2 genes to plot!"))
    }else{
        geneList <- data.frame(type=genelist)
        shiny::validate(need(
            nrow(geneList) > 1, 
            "Please input at least 2 cell information to plot!"))
    }
    if(plotType=='density'){
        shiny::validate(need(
            nrow(geneList)==2,
            "For density plot, please input 2 genes only!"
        ))
    }
    
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
            valueFilterCutoff,
            valueFilterCutoff2
        )
    rat <- getRatio(ggData)
    if(sunburst_type=='expr'){
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
    }else{
        expr <- inpMeta[, inpConf[inpConf$UI %in% geneList$type]$ID, with = FALSE]
        expr_keep <- rep(TRUE, nrow(expr))
    }
    
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff,
        valueFilterCutoff2,
        inpConf=inpConf)
    
    if(cnid>3) colnames(ggData)[cnid] <- subGrpColname
    
    if(plotType=='density'){
        # replace the reduction by the values of expression
        ggData$X <- as.data.frame(expr)[[1]]
        ggData$Y <- as.data.frame(expr)[[2]]
        dimRedX <- colnames(expr)[1]
        dimRedY <- colnames(expr)[2]
    }
    if(plotType!='stream'){
        if(sunburst_type=='expr'){
            if (sum(keep & expr_keep) * nrow(geneList) > 5000) {
                showNotification('The plots are subsamples results (maximal cells 5K).')
                ## filter the expression event, otherwise too slow
                df <- data.frame(keep=keep & expr_keep, expr_levels = rowSums(expr))
                df$id <- seq.int(nrow(df))
                df <- df[df$keep, , drop=FALSE]
                df <- df[order(df$expr_levels, decreasing = TRUE), , drop=FALSE]
                df <- df[seq.int(ceiling(5000/nrow(geneList))), , drop=FALSE]
                expr_keep <- seq_along(expr_keep) %in% df$id
            }
        }else{
            if (sum(keep & expr_keep) * nrow(geneList) > 5000) {
                showNotification('The plots are subsamples results (maximal cells 5K).')
                ## filter the cell, otherwise too slow
                ## use the less cell information, if it contain more information
                ## mean cell deconvolution does not work 
                expr_keep <- apply(expr, 1, function(.ele) length(.ele>0.1))
                df <- data.frame(keep=keep & expr_keep, expr_levels = expr_keep)
                df$id <- seq.int(nrow(df))
                df <- df[df$keep, , drop=FALSE]
                df <- df[order(df$expr_levels, decreasing = FALSE), , drop=FALSE]
                df <- df[seq.int(ceiling(5000/nrow(geneList))), , drop=FALSE]
                expr_keep <- seq_along(expr_keep) %in% df$id
            }
        }
    }else{
        expr_keep <- TRUE
    }
    ggData_ <- ggData[keep & expr_keep]
    expr <- expr[keep & expr_keep, , drop = FALSE]
    ggData <- ggData[keep]
    size <- diff(range(ggData$X, na.rm = TRUE)) / 60 * pointSize
    if(plotType %in% c("sunburst", "pie", "donut", "bar")){
        expr <- apply(expr, 2, rescale, to = c(size / 4, size))
    }
    expr <- as.list(as.data.frame(expr))
    expr <- mapply(function(d, n) {
        cbind(ggData_, geneName = n, "val" = d)
    }, expr, names(expr), SIMPLIFY = FALSE)
    expr <- rbindlist(expr)
    expr$X0 <-
        expr$X + (as.numeric(factor(as.character(expr$geneName))) - 1) * size /
        2
    if(plotType=='stream'){
        ggData <- expr[, c("sampleID", subGrpColname, "geneName", "val"),
                       with=FALSE]
        ## reorder the cells to make sure the output order are same as co-expr.
        ggData$geneName <- factor(ggData$geneName, levels=geneList$gene)
        ggData[, 'mv' :=max(.SD$val, na.rm=TRUE), by=c("geneName")]
        nTot <- getTotalNumber(nGrid = 16, nPad = 2)
        ggData$v1 = round(nTot*ggData$val/ggData$mv)
        ggData$mv <- NULL
        ggData[, 'v0' := sum(.SD$v1), by=c(subGrpColname, "sampleID")]
        ggData$v1 <- NULL
        setorderv(ggData, c("geneName", "v0"))
        ggData[, 'cellID':=seq.int(.N), by=c('geneName')]
        ggData[[subGrpColname]] <- factor(
            ggData[[subGrpColname]],
            levels = rev(sortLevels(as.character(
                unique(ggData[[subGrpColname]])
            ))))
        setDT(ggData)
        ggData[, "row_idx" := .I]
        first_occ <- ggData[, list(first_row = min(.SD$row_idx)),
                            by = c("geneName",subGrpColname, "cellID")]
        setorderv(first_occ, 'first_row')
        first_occ[, "idx" := seq_len(.N),
                  by = c("geneName", subGrpColname)]
        setnames(first_occ, "idx", "cell_idx")
        ggData[first_occ, "idx" := cell_idx,
               on = c("geneName",subGrpColname, "cellID")]
        ggData[["row_idx"]] <- NULL
        ggOut <- stream_plot(ggData, x='idx', y='val',
                             group='geneName',
                             groupY=subGrpColname,
                             normByTotal=CoExpred,
                             type=streamType) +
            scale_y_discrete(expand = c(0.01, 0.01)) +
            scale_x_continuous(expand = c(0, 0)) +
            ylab(subsetCellKey) +
            xlab("cell ID")
        return(ggOut)
    }
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
            density = 
                geom_density_2d_filled(
                    aes(x=.data$X, y=.data$Y),
                    inherit.aes = FALSE,
                    alpha=alpha),
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
                ),
            sum = {
                dt_sum <- expr[, {list(val_sum=sum(.SD$val, na.rm = TRUE))},
                               by = c("sampleID", "X", "Y")]
                geom_point(
                    aes(
                        x = .data$X, y = .data$Y,
                        color = .data$val_sum
                    ),
                    data = dt_sum,
                    size = pointSize,
                    shape = 16,
                    alpha = alpha,
                    inherit.aes = FALSE
                )
            },
            max = {
                dt_max <- expr[, {list(val_max=max(.SD$val, na.rm = TRUE))},
                               by = c("sampleID", "X", "Y")]
                geom_point(
                    aes(
                        x = .data$X, y = .data$Y,
                        color = .data$val_max
                    ),
                    data = dt_max,
                    size = pointSize,
                    shape = 16,
                    alpha = alpha,
                    inherit.aes = FALSE
                )
            },
            mean = {
                dt_mean <- expr[, {list(val_mean=mean(.SD$val, na.rm = TRUE))},
                               by = c("sampleID", "X", "Y")]
                geom_point(
                    aes(
                        x = .data$X, y = .data$Y,
                        color = .data$val_mean
                    ),
                    data = dt_mean,
                    size = pointSize,
                    shape = 16,
                    alpha = alpha,
                    inherit.aes = FALSE
                )
            }
        )
    if(plotType %in% c("density")){
        ggOut <- ggOut + geom_density_2d(
            aes(x=.data$X, y=.data$Y),
            inherit.aes = FALSE,
            alpha=alpha
        )
    }
    
    if(plotType %in% c("sum", "max", "mean", "density")){
        ggOut <- ggOut + scale_color_gradientn(
            colours = availableThemes(gradientCol))
    }else{
        ggOut <- ggOut +
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
    }
    ggOut <- ggOut +
        xlab(dimRedX) + ylab(dimRedY) +
        sctheme(
            base_size = labelsFontsize,
            family = labelsFontFamily, XYval = keepXYlables)
    ggOut <- fixCoord(ggOut, plotAspectRatio, rat)
    return(ggOut)
}
