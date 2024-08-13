# Plot gene expression bubbleplot / heatmap
#' @importFrom stats as.dendrogram dist hclust quantile as.formula
#' @importFrom ComplexHeatmap merge_dendrogram Heatmap draw rowAnnotation
#' HeatmapAnnotation Legend pindex
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar grid.circle unit.c unit grid.xaxis grid.yaxis 
#'  seekViewport current.viewport grid.lines
#' @importFrom rhdf5 h5read
#' @importFrom data.table rbindlist dcast.data.table data.table := uniqueN
#' @importFrom ggdendro dendro_data
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom ggplot2 .data
scBubbHeat <- function(
        inpConf,
        inpMeta,
        inp,
        inpGrp,
        grpKey,
        grpVal,
        inpGrp1c,
        inpPlt,
        dataset,
        inpGene,
        inpScl,
        inpRow,
        row_dend_side = "left",
        row_dend_width = unit(10, "mm"),
        inpCol,
        column_dend_side = "top",
        column_dend_height = unit(10, "mm"),
        legend_side = 'bottom',
        inpcols,
        pointSize,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        flipXY,
        plotAllCells = FALSE,
        save = FALSE,
        colorBreaks,
        legendTitle = "expression",
        returnColorRange = FALSE,
        reorder=FALSE,
        orderX,
        splitBy,
        sreorder=FALSE,
        orderS) {
    # Identify genes that are in our dataset
    if (missing(inpGrp1c))
        inpGrp1c <- 0
    if (inpPlt != "Heatmap") {
        plotAllCells <- FALSE
    }
    geneList <- scGeneList(inp, inpGene)
    geneList <- geneList[geneList$present == TRUE]
    shiny::validate(need(
        nrow(geneList) <= .globals$maxHeatmapGene,
        paste("More than", .globals$maxHeatmapGene,
              "genes to plot! Please reduce the gene list!")
    ))
    shiny::validate(need(
        nrow(geneList) > 1,
        "Please input at least 2 genes to plot!"))
    if(nrow(geneList)>.globals$maxNumGene){
        showNotification(
            paste('Plotting expression data for too many genes.',
                  'It will take a while. Please be patient.'),
            duration = 5,
            type = 'message'
        )
    }
    
    # Prepare ggData
    ggData <- data.table()
    vals <- h5read(file.path(
        .globals$datafolder,
        dataset,
        .globals$filenames$sc1gexpr
    ), .globals$h5fGrp, index=list(inpGene[geneList$gene], NULL))
    rownames(vals) <- geneList$gene
    tmp <- inpMeta[, c(
        "sampleID",
        inpConf[inpConf$grp == TRUE]$ID),
        with = FALSE]
    tmp$grpBy <- inpMeta[[inpConf[inpConf$UI == inpGrp]$ID]]
    # Load splitBy
    if(!missing(splitBy)){
        if(splitBy!=""){
            if(splitBy %in% inpConf$UI){
                if(plotAllCells){
                    showNotification(
                        'Plotting all cells data does not support splitBy.',
                        duration = 5,
                        type = 'warning'
                    )
                }else{
                    tmp$splitBy <- inpMeta[[inpConf[inpConf$UI == splitBy]$ID]]
                    if(sreorder){
                        tmp$splitBy <- factor(as.character(tmp$splitBy),
                                              levels=orderS)
                    }
                }
            }
        }
    }
    for (iGene in geneList$gene) {
        tmp$geneName <- iGene
        tmp$val <- vals[iGene, ]
        ggData <- rbindlist(list(ggData, tmp))
    }
    ggData <- subGrp(ggData, grpKey, grpVal, inpConf)
    
    if (inpPlt == "Violin") {
        ggData$geneName <- factor(as.character(ggData$geneName), levels = geneList$gene)
        if(reorder){
            ggData$grpBy <- factor(ggData$grpBy, levels=orderX)
        }
        showLegend <- FALSE
        if('splitBy' %in% colnames(ggData)){
            showLegend <- TRUE
            ggCol <- relevelCol(inpConf, splitBy, ggData, "splitBy")
            if(length(unique(ggData$splitBy))==2){
                ggOut <- ggplot(ggData, aes(
                    .data[["grpBy"]], .data[["val"]], fill = .data[["splitBy"]])) +
                    geom_split_violin(scale = "width") +
                    facet_grid(rows=as.formula("geneName ~ ."), scales = "free")
            }else{
                ggOut <- ggplot(ggData, aes(
                    .data[["grpBy"]], .data[["val"]], fill = .data[["splitBy"]])) +
                    geom_violin(scale = "width") +
                    facet_grid(rows=as.formula("geneName ~ ."), scales = "free")
            }
        }else{
            ggCol <- relevelCol(inpConf, inpGrp, ggData, "grpBy")
            ggOut <- ggplot(ggData, aes(
                .data[["grpBy"]], .data[["val"]], fill = .data[["grpBy"]])) +
                geom_violin(scale = "width") +
                facet_grid(rows=as.formula("geneName ~ ."), scales = "free")
        }
        
        if(pointSize>0) ggOut <- ggOut +
            geom_jitter(size = pointSize, shape = 16)
        ggOut <- ggOut + 
            xlab(inpGrp) + ylab("expression") +
            sctheme(
                base_size = labelsFontsize,
                family = labelsFontFamily,
                Xang = 45,
                XjusH = 1) +
            scale_fill_manual("", values = ggCol) +
            theme(legend.position = ifelse(
                showLegend, "bottom", "none"))
        return(plot(ggOut))
    }
    
    # Aggregate
    ggData$val <- expm1(ggData$val)
    ggData$val[is.infinite(ggData$val)] <-
        max(ggData$val[!is.infinite(ggData$val)], na.rm = TRUE)
    cNames <- c("geneName", "grpBy", "splitBy")[
        c("geneName", "grpBy", "splitBy") %in% 
            colnames(ggData)
    ]
    if (!plotAllCells) {
        ggData <- ggData[, list(
            val = mean(.SD$val[.SD$val >= inpGrp1c]),
            prop = sum(.SD$val > 0) / length(.SD$sampleID)
        ),
        by = cNames]
        ggDataAvg <- NULL
    } else{
        ggDataAvg <- ggData[, list(
            val = mean(.SD$val[.SD$val >= inpGrp1c]),
            prop = sum(.SD$val > 0) / length(.SD$sampleID)
        ),
        by = cNames]
        ggDataAvg$val <- log1p(ggDataAvg$val)
    }
    ggData$val <- log1p(ggData$val)
    
    # Scale if required
    colRange <- range(ggData$val, na.rm = TRUE)
    colRange1 <- quantile(
        ggData$val,
        probs = c(0, .01, .5, .99, 1),
        na.rm = TRUE,
        names = FALSE
    )
    if (inpScl) {
        ggData[, "val" := if (length(unique(.SD$val)) == 1) {
            0
        } else {
            scale(.SD$val)
        }, keyby = "geneName"]
        colRange <- range(ggData$val, na.rm = TRUE)
        if (colRange[1] < 0) {
            colRange <- c(-max(abs(
                range(ggData$val, na.rm = TRUE)
            )),
            max(abs(
                range(ggData$val, na.rm = TRUE)
            )))
        } else{
            colRange <- c(0, max(abs(
                range(ggData$val, na.rm = TRUE)
            )))
        }
        colRange1 <- quantile(
            ggData$val,
            probs = c(0, .01, .5, .99, 1),
            na.rm = TRUE,
            names = FALSE
        )
        colRange1 <- c(colRange[1], colRange1[-c(1, 5)], colRange[2])
    }
    if (returnColorRange) {
        colRange1 <- colRange1[c(-3)]
        return(colRange1)
    }
    
    if (!is.na(colorBreaks[1])) {
        if (colorBreaks[1] < colRange[1])
            colorBreaks[1] <- colRange[1]
        if (colorBreaks[2] > colRange[2])
            colorBreaks[2] <- colRange[2]
        ggData$val[ggData$val < colorBreaks[1]] <- colorBreaks[1]
        ggData$val[ggData$val > colorBreaks[2]] <- colorBreaks[2]
        if (colorBreaks[1] == colorBreaks[2]) {
            col_fun <- .globals$cList[[inpcols]][1]
        } else{
            col_fun <- colorRamp2(
                breaks = seq(
                    colorBreaks[1],
                    colorBreaks[2],
                    length.out = length(.globals$cList[[inpcols]])
                ),
                colors = .globals$cList[[inpcols]]
            )
        }
    } else{
        if (colRange[1] == colRange[2]) {
            col_fun <- .globals$cList[[inpcols]][1]
        } else{
            col_fun <- colorRamp2(
                breaks = seq(
                    colRange[1],
                    colRange[2],
                    length.out = length(.globals$cList[[inpcols]])
                ),
                colors = .globals$cList[[inpcols]]
            )
        }
    }
    # reshape the data to matrix
    if (plotAllCells) {
        ggData$grpBy <- paste(ggData$grpBy, ggData$sampleID, sep = "__")
    }else{
        if(reorder){
            ggData$grpBy <- factor(as.character(ggData$grpBy), levels=orderX)
        }
    }
    if(sreorder){
        ggData$splitBy <- factor(as.character(ggData$splitBy), levels =orderS)
    }
    
    reshapeMat <- function(value.var) {
        ggMatrix <- dcast.data.table(
            ggData,
            as.formula(ifelse('splitBy' %in% colnames(ggData),
                              "geneName~grpBy+splitBy", "geneName~grpBy")),
            value.var = value.var,
            fun.aggregate = mean,
            sep = '___')
        tmp <- ggMatrix$geneName
        ggMatrix <- as.matrix(ggMatrix[, -1])
        ggMatrix[is.na(ggMatrix)] <- 0
        ggMatrix[is.infinite(ggMatrix)] <-
            .Machine$integer.max * sign(ggMatrix[is.infinite(ggMatrix)])
        rownames(ggMatrix) <- tmp
        return(ggMatrix)
    }
    ggMat <- reshapeMat(value.var = "val")
    if(!plotAllCells) ggProp <- reshapeMat(value.var = 'prop')
    
    ggMatSplit <- sub('^.*___', '', colnames(ggMat))
    ggMat <- ggMat[geneList$gene[geneList$gene %in% rownames(ggMat)],
                   order(ggMatSplit), drop=FALSE]
    if(!plotAllCells) ggProp <- 
        ggProp[geneList$gene[geneList$gene %in% rownames(ggProp)],
               order(ggMatSplit), drop=FALSE]
    ggMatSplit <- sub('^.*___', '', colnames(ggMat))
    ggMatGrp <- sub('___.*$', '', colnames(ggMat))
    colnames(ggMat) <- sub('___', '_', colnames(ggMat))
    if(!plotAllCells) colnames(ggProp) <- sub('___', '_', colnames(ggProp))
    cluster_rows <- inpRow
    if (inpRow) {
        cluster_rows <- as.dendrogram(hclust(dist(ggMat)))
    }
    cluster_columns <- inpCol
    if (inpCol) {
        cluster_columns <- as.dendrogram(hclust(dist(t(ggMat))))
    }
    
    layer_fun <- NULL
    rect_gp <- gpar(col = NA)
    font_gp <- gpar(fontsize = labelsFontsize, fontfamily = labelsFontFamily)
    title_position <- ifelse(legend_side=='bottom',
                             "lefttop","topleft")
    legend_direction <- ifelse(legend_side=='bottom',
                               "horizontal","vertical")
    if (inpPlt == "Bubbleplot") {
        if(pointSize>0){
            dotsize <- unit(pointSize*3.2, 'mm')
        }else{
            dotsize <- unit(4, "mm")
        }
        layer_fun <- function(j, i, x, y, w, h, fill){
            grid.circle(x=x,y=y,
                        r= sqrt(pindex(ggProp, i, j)) * dotsize,
                        gp = gpar(fill = col_fun(pindex(ggMat, i, j)),
                                  col = NA))
        }
        lgd_list = list(
            Legend( labels = c(0, 10, 25, 50, 75), title = "percentage",
                    title_position = title_position,
                    graphics = list(
                        function(x, y, w, h) {
                            grid.circle(x = x, y = y, r = 0  * dotsize,
                                        gp = gpar(fill = "black"))},
                        function(x, y, w, h) {
                            grid.circle(x = x, y = y, r = sqrt(0.1)  * dotsize,
                                        gp = gpar(fill = "black"))},
                        function(x, y, w, h) {
                            grid.circle(x = x, y = y, r = sqrt(0.25) * dotsize,
                                        gp = gpar(fill = "black"))},
                        function(x, y, w, h) {
                            grid.circle(x = x, y = y, r = sqrt(0.5) * dotsize,
                                        gp = gpar(fill = "black"))},
                        function(x, y, w, h) {
                            grid.circle(x = x, y = y, r = sqrt(0.75) * dotsize,
                                        gp = gpar(fill = "black"))}
                    ),
                    grid_width = 2*dotsize,
                    grid_height = 2*dotsize,
                    ncol = ifelse(legend_side=='bottom',
                                  5, 1),
                    direction = legend_direction
            ))
        rect_gp <- gpar(type = 'none')
    }
    if (flipXY) {
        ggMat <- t(ggMat)
        ggProp <- t(ggProp)
        
        if (plotAllCells) {
            group <- ggData[[inpGrp]][match(rownames(ggMat), ggData$grpBy)]
            anno <- rowAnnotation(
                group = group,
                show_legend = FALSE,
                show_annotation_name = FALSE
            )
            ht_list <- Heatmap(
                ggMat,
                name = legendTitle,
                col = col_fun,
                heatmap_legend_param =
                    list(
                        title = legendTitle,
                        direction = legend_direction,
                        title_position = title_position
                    ),
                cluster_rows = TRUE,
                cluster_row_slices = inpCol,
                cluster_columns = cluster_rows,
                show_row_names = !plotAllCells,
                row_split = group,
                right_annotation = anno,
                row_title_rot = 0,
                column_names_rot = 45,
                layer_fun = layer_fun,
                rect_gp = rect_gp,
                use_raster = TRUE,
                row_title_gp = font_gp,
                column_title_gp = font_gp,
                row_names_gp = font_gp,
                column_names_gp = font_gp,
                row_dend_side = ifelse(column_dend_side == "right",
                                       "right", "left"),
                row_dend_width = column_dend_height,
                column_dend_side = ifelse(row_dend_side=="top",
                                          "top", "bottom"),
                column_dend_height = row_dend_width,
                row_names_side = ifelse(column_dend_side == "right",
                                        "left", "right"),
                row_title_side = ifelse(column_dend_side == "right",
                                        "left", "right"),
                column_title_side = ifelse(row_dend_side=="top",
                                           "bottom", "top")
            )
        } else{
            ht_list <- Heatmap(
                ggMat,
                name = legendTitle,
                col = col_fun,
                heatmap_legend_param =
                    list(
                        title = legendTitle,
                        direction = legend_direction,
                        title_position = title_position
                    ),
                cluster_rows = cluster_columns,
                cluster_columns = cluster_rows,
                show_row_names = !plotAllCells,
                column_names_rot = 45,
                layer_fun = layer_fun,
                rect_gp = rect_gp,
                row_split = if(inpCol) NULL else ggMatSplit,
                row_title_gp = font_gp,
                column_title_gp = font_gp,
                row_names_gp = font_gp,
                column_names_gp = font_gp,
                row_dend_side = ifelse(column_dend_side == "right",
                                       "right", "left"),
                row_dend_width = column_dend_height,
                column_dend_side = ifelse(row_dend_side=="top",
                                          "top", "bottom"),
                column_dend_height = row_dend_width,
                row_names_side = ifelse(column_dend_side == "right",
                                        "left", "right"),
                column_names_side = ifelse(row_dend_side=="top",
                                           "bottom", "top")
            )
        }
    } else{
        if (plotAllCells) {
            group <- ggData[[inpGrp]][match(colnames(ggMat), ggData$grpBy)]
            anno <- HeatmapAnnotation(
                group = group,
                show_legend = FALSE,
                show_annotation_name = FALSE
            )
            ht_list <- Heatmap(
                ggMat,
                name = legendTitle,
                col = col_fun,
                heatmap_legend_param =
                    list(
                        title = legendTitle,
                        direction = legend_direction,
                        title_position = title_position
                    ),
                cluster_rows = cluster_rows,
                cluster_columns = TRUE,
                cluster_column_slices = inpCol,
                show_column_names = !plotAllCells,
                column_split = group,
                bottom_annotation = anno,
                layer_fun = layer_fun,
                rect_gp = rect_gp,
                use_raster = TRUE,
                row_title_gp = font_gp,
                column_title_gp = font_gp,
                row_names_gp = font_gp,
                column_names_gp = font_gp,
                row_dend_side = ifelse(row_dend_side == "right",
                                       "right", "left"),
                row_dend_width = row_dend_width,
                column_dend_side = ifelse(column_dend_side=="top",
                                          "top", "bottom"),,
                column_dend_height = column_dend_height,
                row_names_side = ifelse(row_dend_side == "right",
                                        "left", "right"),
                column_title_side = ifelse(column_dend_side=="top",
                                           "bottom", "top"),
                column_title_rot = 45
            )
        } else{
            ht_list <- Heatmap(
                ggMat,
                name = legendTitle,
                col = col_fun,
                heatmap_legend_param =
                    list(
                        title = legendTitle,
                        direction = legend_direction,
                        title_position = title_position
                    ),
                cluster_rows = cluster_rows,
                cluster_columns = cluster_columns,
                show_column_names = !plotAllCells,
                layer_fun = layer_fun,
                rect_gp = rect_gp,
                column_split = if(inpCol) NULL else ggMatSplit,
                row_title_gp = font_gp,
                column_title_gp = font_gp,
                row_names_gp = font_gp,
                column_names_gp = font_gp,
                row_dend_side = ifelse(row_dend_side == "right",
                                       "right", "left"),
                row_dend_width = row_dend_width,
                column_dend_side = ifelse(column_dend_side=="top",
                                          "top", "bottom"),
                column_dend_height = column_dend_height,
                row_names_side = ifelse(row_dend_side == "right",
                                        "left", "right"),
                column_names_side = ifelse(column_dend_side=="top",
                                           "bottom", "top")
            )
        }
    }
    plot_axis <- function(heatmap, code,
                          envir = new.env(parent = parent.frame())){
        current_vp = current.viewport()$name
        if (current_vp == "ROOT") {
            current_vp = "global"
        }
        vp_name = paste0(heatmap, "_heatmap_body_wrap")
        seekViewport(vp_name)
        eval(substitute(code), envir = envir)
        seekViewport(current_vp)
    }
    if (inpPlt == "Bubbleplot") {
        return({
            draw(
            ht_list,
            heatmap_legend_side = legend_side,
            annotation_legend_side = legend_side,
            annotation_legend_list = lgd_list)
            plot_axis(
                legendTitle,
                {
                    xmain <- ifelse(flipXY,
                                    row_dend_side=='top', 
                                    column_dend_side == "top")
                    ymain <- ifelse(flipXY,
                                    column_dend_side == "right",
                                    row_dend_side == "right")
                    grid.lines(c(0, 1), as.numeric(!xmain))
                    grid.xaxis(at = (seq.int(ncol(ggMat))-.5)/ncol(ggMat),
                               label = FALSE,
                               main = xmain)
                    grid.lines(as.numeric(!ymain), c(0, 1))
                    grid.yaxis(at = (seq.int(nrow(ggMat))-.5)/nrow(ggMat),
                               label = FALSE,
                               main = ymain)
                })
        })
    }else{
        return(
            draw(
            ht_list,
            heatmap_legend_side = legend_side,
            annotation_legend_side = legend_side
            )
        )
    }
}
