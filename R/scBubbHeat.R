# Plot gene expression bubbleplot / heatmap
#' @importFrom stats as.dendrogram dist hclust quantile as.formula
#' @importFrom ComplexHeatmap merge_dendrogram Heatmap draw rowAnnotation
#' HeatmapAnnotation Legend
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar grid.circle unit.c
#' @importFrom rhdf5 h5read
#' @importFrom data.table rbindlist dcast.data.table data.table := uniqueN
#' @importFrom ggdendro dendro_data
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom ggplot2 scale_x_discrete scale_size_continuous .data
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
        inpCol,
        inpcols,
        pointSize,
        labelsFontsize,
        flipXY,
        plotAllCells = FALSE,
        save = FALSE,
        colorBreaks,
        legendTitle = "expression",
        returnColorRange = FALSE,
        reorder=FALSE,
        orderX) {
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
            paste('Ploting expression data for too many genes.',
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
    for (iGene in geneList$gene) {
        tmp$geneName <- iGene
        tmp$val <- vals[iGene, ]
        ggData <- rbindlist(list(ggData, tmp))
    }
    ggData <- subGrp(ggData, grpKey, grpVal, inpConf)
    
    if (inpPlt == "Violin") {
        if(reorder){
            ggData$grpBy <- factor(ggData$grpBy, levels=orderX)
        }
        ggCol <- relevelCol(inpConf, inpGrp, ggData, "grpBy")
        ggOut <- ggplot(ggData, aes(
            .data[["grpBy"]], .data[["val"]], fill = .data[["grpBy"]])) +
            geom_violin(scale = "width") +
            facet_grid(rows=as.formula("geneName ~ ."), scales = "free")
        if(pointSize>0) ggOut <- ggOut +
            geom_jitter(size = pointSize, shape = 16)
        ggOut <- ggOut + 
            xlab(inpGrp) + ylab("expression") +
            sctheme(
                base_size = .globals$sList[labelsFontsize],
                Xang = 45,
                XjusH = 1) +
            scale_fill_manual("", values = ggCol) +
            theme(legend.position = "none")
        return(plot(ggOut))
    }
    
    # Aggregate
    ggData$val <- expm1(ggData$val)
    ggData$val[is.infinite(ggData$val)] <-
        max(ggData$val[!is.infinite(ggData$val)], na.rm = TRUE)
    if (!plotAllCells) {
        ggData <- ggData[, list(
            val = mean(.SD$val[.SD$val >= inpGrp1c]),
            prop = sum(.SD$val > 0) / length(.SD$sampleID)
        ),
        by = c("geneName", "grpBy")]
        ggDataAvg <- NULL
    } else{
        ggDataAvg <- ggData[, list(
            val = mean(.SD$val[.SD$val >= inpGrp1c]),
            prop = sum(.SD$val > 0) / length(.SD$sampleID)
        ),
        by = c("geneName", "grpBy")]
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
            ggData$grpBy <- factor(ggData$grpBy, levels=orderX)
        }
    }
    reshapeMat <- function(value.var) {
        ggMatrix <- dcast.data.table(
            ggData,
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
    ggMat <- reshapeMat(value.var = "val")
    
    cluster_rows <- inpRow
    if (inpRow) {
        cluster_rows <- as.dendrogram(hclust(dist(ggMat)))
    }
    cluster_columns <- inpCol
    if (inpCol) {
        cluster_columns <- as.dendrogram(hclust(dist(t(ggMat))))
    }
    
    if (inpPlt == "Bubbleplot") {
        ## giveup complexHeatmap becase the size if not fixed
        axis_fontsize <-
            round(min(c(500 / nrow(geneList), 12), na.rm = TRUE), digits = 1)
        bulb_pointsize <-
            min(c(round(400 / nrow(geneList)), 8), na.rm = TRUE)
        if (inpRow) {
            hcRow <- dendro_data(cluster_rows)
            ggRow <- ggplot() + coord_flip() +
                geom_segment(
                    data = hcRow$segments,
                    aes(
                        x = .data$x,
                        y = .data$y,
                        xend = .data$xend,
                        yend = .data$yend
                    )
                ) +
                scale_y_continuous(
                    breaks = rep(0, uniqueN(ggData$grpBy)),
                    labels = unique(ggData$grpBy),
                    expand = c(0, 0)
                ) +
                scale_x_continuous(
                    breaks = seq_along(hcRow$labels$label),
                    labels = hcRow$labels$label,
                    expand = c(0, 0.5)
                ) +
                sctheme(base_size = .globals$sList[labelsFontsize]) +
                theme(
                    axis.title = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(
                        color = "white",
                        angle = 45,
                        hjust = 1
                    )
                )
            ggData$geneName <-
                factor(ggData$geneName, levels = hcRow$labels$label)
        } else {
            ggData$geneName <-
                factor(ggData$geneName, levels = rev(geneList$gene))
        }
        if (inpCol) {
            hcCol <- dendro_data(cluster_columns)
            ggCol <- ggplot() +
                geom_segment(
                    data = hcCol$segments,
                    aes(
                        x = .data$x,
                        y = .data$y,
                        xend = .data$xend,
                        yend = .data$yend
                    )
                ) +
                scale_x_continuous(
                    breaks = seq_along(hcCol$labels$label),
                    labels = hcCol$labels$label,
                    expand = c(0.05, 0)
                ) +
                scale_y_continuous(
                    breaks = rep(0, uniqueN(ggData$geneName)),
                    labels = unique(ggData$geneName),
                    expand = c(0, 0)
                ) +
                sctheme(base_size = axis_fontsize,
                        Xang = 45,
                        XjusH = 1) +
                theme(
                    axis.title = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(color = "white")
                )
            ggData$grpBy <-
                factor(ggData$grpBy, levels = hcCol$labels$label)
        } else{
            if(reorder){
                ggData$grpBy <- factor(ggData$grpBy, levels=orderX)
            }else{
                ggData$grpBy <-
                    factor(
                        as.character(ggData$grpBy),
                        levels = sortLevels(sort(as.character(
                            unique(ggData$grpBy)
                        ))))
            }
        }
        ggOut <- ggplot(
            ggData,
            aes(
                x = .data$grpBy,
                y = .data$geneName,
                color = .data$val,
                size = .data$prop
            )
        ) +
            geom_point() +
            sctheme(base_size = .globals$sList[labelsFontsize],
                    Xang = 45,
                    XjusH = 1) +
            scale_x_discrete(expand = c(0.05, 0)) +
            scale_y_discrete(expand = c(0, 0.5)) +
            scale_size_continuous(
                "proportion",
                range = c(0, bulb_pointsize),
                limits = c(0, 1),
                breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)
            ) +
            scale_color_gradientn(
                legendTitle,
                limits = colRange,
                colours = .globals$cList[[inpcols]]) +
            guides(color = guide_colorbar(barwidth = 15)) +
            theme(
                axis.title = element_blank(),
                axis.text.y = element_text(size = axis_fontsize),
                legend.box = "vertical"
            )
        ggLeg <- g_legend(ggOut)
        ggOut <- ggOut + theme(legend.position = "none")
        if (flipXY) {
            ggOut <- ggOut + coord_flip()
            ggOut <-
                grid.arrange(
                    ggOut,
                    ggLeg,
                    heights = c(7, 2),
                    layout_matrix = rbind(c(1), c(2))
                )
        } else{
            FUN <- if (save)
                arrangeGrob
            else
                grid.arrange
            if (inpRow & inpCol) {
                ggOut <- FUN(
                    ggOut,
                    ggLeg,
                    ggCol,
                    ggRow,
                    widths = c(7, 1),
                    heights = c(1, 7, 2),
                    layout_matrix = rbind(c(3, NA), c(1, 4), c(2, NA))
                )
            } else if (inpRow) {
                ggOut <- FUN(
                    ggOut,
                    ggLeg,
                    ggRow,
                    widths = c(7, 1),
                    heights = c(7, 2),
                    layout_matrix = rbind(c(1, 3), c(2, NA))
                )
            } else if (inpCol) {
                ggOut <- FUN(
                    ggOut,
                    ggLeg,
                    ggCol,
                    heights = c(1, 7, 2),
                    layout_matrix = rbind(c(3), c(1), c(2))
                )
            } else {
                ggOut <- FUN(
                    ggOut,
                    ggLeg,
                    heights = c(7, 2),
                    layout_matrix = rbind(c(1), c(2))
                )
            }
        }
        return(ggOut)
    }
    
    layer_fun <- NULL
    rect_gp <- gpar(col = NA)
    if (flipXY) {
        ggMat <- t(ggMat)
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
                        direction = "horizontal",
                        title_position = "lefttop"
                    ),
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
                use_raster = TRUE
            )
        } else{
            ht_list <- Heatmap(
                ggMat,
                name = legendTitle,
                col = col_fun,
                heatmap_legend_param =
                    list(
                        title = legendTitle,
                        direction = "horizontal",
                        title_position = "lefttop"
                    ),
                cluster_rows = cluster_columns,
                cluster_columns = cluster_rows,
                show_row_names = !plotAllCells,
                column_names_rot = 45,
                layer_fun = layer_fun,
                rect_gp = rect_gp
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
                        direction = "horizontal",
                        title_position = "lefttop"
                    ),
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
                use_raster = TRUE
            )
        } else{
            ht_list <- Heatmap(
                ggMat,
                name = legendTitle,
                col = col_fun,
                heatmap_legend_param =
                    list(
                        title = legendTitle,
                        direction = "horizontal",
                        title_position = "lefttop"
                    ),
                cluster_rows = cluster_rows,
                cluster_columns = cluster_columns,
                show_column_names = !plotAllCells,
                column_names_rot = 45,
                layer_fun = layer_fun,
                rect_gp = rect_gp
            )
        }
    }
    return(draw(
        ht_list,
        heatmap_legend_side = "bottom",
        annotation_legend_side = "bottom"
    ))
}
