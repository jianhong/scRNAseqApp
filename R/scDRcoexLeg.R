#' @importFrom grDevices rgb
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes .data xlab ylab coord_fixed
#' scale_x_continuous scale_y_continuous geom_tile
scDRcoexLeg <- function(gene1, gene2, colorPairs, labelsFontsize = 24,
                        labelsFontFamily = 'Helvetica') {
    # Generate coex color palette
    nTot <- getTotalNumber(nGrid = 16, nPad = 2)
    gg <- getCoexpCol(colorPairs, nGrid = 16, nPad = 2)
    
    # Actual ggplot
    ggOut <- ggplot(gg, aes(.data[["v1"]], .data[["v2"]])) +
        geom_tile(fill = gg$cMix) +
        xlab(gene1) + ylab(gene2) + coord_fixed(ratio = 1) +
        scale_x_continuous(
            breaks = c(0, nTot),
            labels = c("low", "high")) +
        scale_y_continuous(
            breaks = c(0, nTot),
            labels = c("low", "high")) +
        sctheme(base_size = labelsFontsize,
                family = labelsFontFamily, XYval = TRUE)
    return(ggOut)
}
