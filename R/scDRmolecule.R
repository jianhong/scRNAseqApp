# Plot gene expression on dimred
#' @importFrom ggplot2 ggplot aes .data geom_point xlab ylab guides guide_colorbar scale_colour_manual coord_fixed scale_y_discrete scale_x_continuous xlim
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
        inpCellBorder=FALSE,# stereo-seq cell borders
        cellborderFilename='',
        cellSegAlpha=1,
        cellSegColor=NA,
        cellColor=NULL,
        inpBgImg=FALSE,
        backgroundImage='',
        ...) {
    cellborder <- NULL
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
    
    dimRedX <- paste0(fov, 1)
    dimRedY <- paste0(fov, 2)
    checkCellSegmentationAvailability(environment())
    backgroundAlignFun <- checkBgImgAvailability(environment())
    
    # Actual ggplot
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
    
    ggCol <- NULL
    if(inpCellBorder){
        # assign values for cells
        if(!is.null(cellColor)){
            if(is.list(cellColor) &&
               all(c('cellinfoID', 'inpConf', 'inpMeta') %in%
                   names(cellColor))){
                cellinfo <- 
                    cellColor$inpMeta[, c('sampleID', 
                                      cellColor$inpConf[
                                          cellColor$inpConf$UI == 
                                              cellColor$cellinfoID]$ID),
                                      with = FALSE]
                colnames(cellinfo)[2] <- 'val'
                ggCol <- relevelCol(cellColor$inpConf, cellColor$cellinfoID,
                                    cellinfo, 'val')
                cellborder$val <- 
                    cellinfo[[2]][
                        match(cellborder$sampleID,
                              cellinfo$sampleID)]
            }
        }
        if(isTRUE(length(cellborder$val)!=nrow(cellborder))){
            cellborder$val <- 'NA'
            if(isTRUE(is.na(cellSegColor))){
                ggCol=c('NA'='#EEEEEE')
            }else{
                ggCol=c('NA'=cellSegColor)
            }
        }
    }
    
    ggOut <- pointPlot(
        ggOut = ggOut,
        pointSize = pointSize,
        fontSize = labelsFontsize,
        labelsFontFamily = labelsFontFamily,
        dimRedX = '',
        dimRedY = '',
        keepXYlables = keepXYlables,
        inpCellBorder = inpCellBorder,
        cellborder = cellborder,
        cellSegColor = cellSegColor,
        cellSegAlpha = cellSegAlpha) +
        scale_fill_manual(values = ggCol, guide = 'none') +
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
