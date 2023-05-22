#' waffle plot for single cell expressions
#' @noRd
#' @importFrom stats as.formula
#' @importFrom ggplot2 coord_equal facet_grid unit scale_fill_gradientn .data
#' @importFrom grDevices hcl.colors
#' @param expr expression table
#' @param groupCol The column name for `facet_grid`
#' @param gradientCol Color sets
#' @param xyMaxRatio The ratio of x,y. The parameter is used to avoid long plots
#' Not used yet.
scWafflePlot <- function(
        expr,
        groupCol = 'splitBy',
        gradientCol = rev(hcl.colors(20, "RdYlGn")),
        xyMaxRatio = 5) {
    if (length(gradientCol) == 1) {
        gradientCol <- .globals$cList[[gradientCol]]
    }
    if (groupCol[1] %in% colnames(expr)) {
        if (!all(
            as.character(expr[[groupCol]]) ==
            as.character(expr$grpBy), na.rm = TRUE)) {
            expr$geneName <- paste(
                as.character(expr$geneName),
                as.character(expr[[groupCol]]))
        }
    }
    data <- expr[, c("geneName", "grpBy", "val"), with = FALSE]
    data <- data[order(expr$geneName, expr$grpBy, -1 * expr$val),]
    data <- data[!is.na(data$val),]
    groupMax <- data[, {
        list(N = max(.SD[, list(N = .N), by = 'grpBy']$N))
    }, by = 'geneName']
    data <- merge(data, groupMax)
    ggData <- data[, {
        this_N <- max(100, .SD$N[1])
        .SD[, {
            list(idx <- as.numeric(
                cut(seq.int(this_N), 100))[seq.int(.N)])
            .SD[, {
                list(exprs = mean(.SD$val, na.rm = TRUE))
            }, by = 'idx']
        }, by = 'grpBy']
    }, by = 'geneName']
    
    ggData$x <- (ggData$idx - 1) %% 10 + 1
    ggData$y <- floor((ggData$idx - 1) / 10) + 1
    
    # len_cols <- length(unique(data$grpBy))
    # len_rows <- length(unique(data$geneName))
    # xyRatio <- len_rows / len_cols
    # if (xyRatio < 1 / xyMaxRatio) {
    #     ## plot is too wider
    #     ## change the geneName
    #     ## add blank after geneName by grpBy group
    # } else{
    #     if (xyRatio > xyMaxRatio) {
    #         ## plot is too long
    #         ## change the grpBy
    #         ## add blank after grpBy
    #     }
    # }
    
    p <- ggplot(ggData, aes(
        .data[["x"]], .data[["y"]], fill = .data[["exprs"]])) +
        geom_tile(color = 'white') + coord_equal() +
        facet_grid(as.formula("geneName ~ grpBy")) +
        theme_minimal() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
        guides(fill = guide_colorbar(barwidth = unit(3, "mm"))) +
        xlab("") + ylab("") +
        scale_fill_gradientn(colors = gradientCol)
    return(p)
}


scDRwafflePlot <- function(
        dataset,
        geneIdMap,
        inpConf,
        inpMeta,
        gene,
        groupBy,
        splitBy,
        gradientCol,
        grpKey,
        grpVal) {
    if (isQuote(gene)) {
        genenames <- geneIdMap[
            names(geneIdMap) %in% removeQuote(gene)]
    } else{
        if (isAsterisk(gene)) {
            gene <- isAsterisk(gene, transform = TRUE)
            genenames <-
                geneIdMap[grepl(gene, names(geneIdMap), ignore.case = TRUE)]
        } else{
            geneList <- scGeneList(gene, geneIdMap)
            geneList <- geneList[geneList$present == TRUE]
            genenames <- geneList$gene
        }
    }
    if (length(genenames) > 0) {
        #waffle plot
        if (missing(groupBy)) {
            groupBy <- getCelltypeCol(
                inpConf,
                celltypePattern =
                    .globals$groupColPattern)
        }
        if (missing(splitBy)) {
            splitBy <- NA
        }
        ggData <-
            read_exprs(
                dataset,
                geneIdMap[genenames],
                inpMeta,
                inpConf,
                groupBy,
                splitBy,
                valueOnly = FALSE)
        ggData[ggData$val < 0]$val <- 0
        
        ggData <- subGrp(ggData, grpKey, grpVal, inpConf)
        if("filter" %in% colnames(ggData)){
            ggData$filter <- NULL
        }
        return(scWafflePlot(
            ggData, 'splitBy', gradientCol))
    } else{
        return(ggplot())
    }
}
