#' @importFrom grDevices rgb
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes .data xlab ylab coord_fixed scale_x_continuous scale_y_continuous geom_tile
scDRcoexLeg <- function(gene1, gene2, colorPairs, labelsFontsize = 24,
                        labelsFontFamily = 'Helvetica', geneType='gene') {
    # Generate coex color palette
    nTot <- getTotalNumber(nGrid = 16, nPad = 2)
    gg <- getCoexpCol(colorPairs, nGrid = 16, nPad = 2)
    
    # Actual ggplot
    xlab <- ifelse(geneType=='gene', gene1, paste(gene2, 'expr'))
    ylab <- ifelse(geneType=='gene', gene2, paste(gene1, 'score'))
    ggOut <- ggplot(gg, aes(.data[["v1"]], .data[["v2"]])) +
        geom_tile(fill = gg$cMix) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(
            breaks = c(0, nTot),
            labels = c("low", "high")) +
        scale_y_continuous(
            breaks = c(0, nTot),
            labels = c("low", "high")) +
        sctheme(base_size = labelsFontsize,
                family = labelsFontFamily, XYval = TRUE)
    
    ggOut$meta$fixCoord <- list(aspectRatio='Fixed', ratio=1)
    return(ggOut)
}
