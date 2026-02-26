# Plot gene expression on dimred
#' @importFrom ggplot2 ggplot aes .data geom_point xlab ylab guides
#' guide_colorbar scale_colour_manual coord_fixed scale_y_discrete
#' scale_x_continuous xlim
#' @importFrom ggridges geom_density_ridges theme_ridges
scDRmolecule <- function(
        genes,
        molecules,
        fov,
        pointSize,
        gradientCol,
        labelsFontsize,
        labelsFontFamily,
        plotAspectRatio,
        keepXYlables,
        xlim=NULL,ylim=NULL,
        ...) {
    if(length(genes)==0){
        return(ggplot())
        }
    if (genes[1] == "") {
        return(ggplot())
    }
    if(length(genes)>9){
        showNotification(
            "The maximal gene number is 9. Only first 9 genes will be plot!",
            type = "warning"
        )
        genes <- genes[seq.int(9)]
    }
    ggData <- molecules[[fov]]
    ggData <- ggData[ggData$molecule %in% genes, , drop=FALSE]
    colnames(ggData) <- c("X", "Y", "val")
    rat <- getRatio(ggData)
    
    # Actual ggplot
    ggOut <- ggXYplot(ggData)
    ggOut <- pointPlot(
        ggOut = ggOut,
        pointSize = pointSize,
        fontSize = labelsFontsize,
        labelsFontFamily = labelsFontFamily,
        dimRedX = '',
        dimRedY = '',
        keepXYlables = keepXYlables) +
        scale_colour_manual(
            name = 'molecues',
            values = availableThemes(gradientCol)) +
        theme(legend.text = element_text(size = labelsFontsize,
                                         family = labelsFontFamily)) +
        guides(color = guide_legend(
            override.aes = list(size = 5),
            nrow = ceiling(length(genes)/5)
        ))
    ggOut <- fixCoord(ggOut, plotAspectRatio, rat, xlim, ylim)
    
    return(ggOut)
}
