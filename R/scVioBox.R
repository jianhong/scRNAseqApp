# Plot violin / boxplot
#' @importFrom stats rnorm
#' @importFrom ggplot2 ggplot aes .data geom_violin geom_boxplot
#' geom_jitter xlab ylab scale_fill_manual theme
scVioBox <- function(
        inpConf,
        inpMeta,
        infoX,
        infoY,
        subsetCellKey,
        subsetCellVal,
        valueFilterKey,
        valueFilterCutoff,
        valueFilterCutoff2,
        dataset,
        geneIdMap,
        inptyp,
        inppts,
        pointSize,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        reorder=FALSE,
        orderX,
        splitBy,
        sreorder=FALSE,
        orderS,
        addnoise=TRUE) {
    # Prepare ggData
    ggData <- inpMeta[, c(
        inpConf[inpConf$UI == infoX]$ID,
        inpConf[inpConf$grp == TRUE]$ID), ## will include all subsetCellKey
        with = FALSE]
    colnames(ggData)[1] <- c("X")
    # Load in either cell meta or gene expr
    if (infoY %in% inpConf$UI) {
        ggData$val <- inpMeta[[inpConf[inpConf$UI == infoY]$ID]]
    } else {
        ggData$val <- read_exprs(dataset, geneIdMap[infoY], valueOnly = TRUE)
        ggData[ggData$val < 0]$val <- 0
        if(addnoise){
            tmpNoise <-
                rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
            ggData$val <- ggData$val + tmpNoise
        }
    }
    # Load splitBy
    if(!missing(splitBy)){
        if(splitBy!=""){
            if(splitBy %in% inpConf$UI){
                ggData$splitBy <- inpMeta[[inpConf[inpConf$UI == splitBy]$ID]]
                if(sreorder){
                    ggData$splitBy <- factor(as.character(ggData$splitBy),
                                             levels = orderS)
                }
            }
        }
    }
    # filter the cell
    subFilterColname <- 'filter'
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
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff,
        valueFilterCutoff2,
        inpConf)
    ggData <- ggData[keep]
    
    # Do factoring
    if(reorder){
        ggData$X <- factor(ggData$X, levels=orderX)
    }else{
        ggData <- relevelData(ggData, "X")
    }
    if('splitBy' %in% colnames(ggData)){
        ggCol <- relevelCol(inpConf, splitBy, ggData, "splitBy")
    }else{
        ggCol <- relevelCol(inpConf, infoX, ggData, "X")
    }
    
    
    # Actual ggplot
    showLegend <- FALSE
    if (inptyp == "violin") {
        if('splitBy' %in% colnames(ggData)){
            if(length(unique(ggData$splitBy))==2){
                ggOut <- ggplot(ggData, aes(
                    .data[["X"]], .data[["val"]], fill = .data[["splitBy"]])) +
                    geom_split_violin(scale = "width")
            }else{
                ggOut <- ggplot(ggData, aes(
                    .data[["X"]], .data[["val"]], fill = .data[["splitBy"]])) +
                    geom_violin(scale = "width")
            }
            showLegend <- TRUE
        }else{
            ggOut <- ggplot(ggData, aes(
                .data[["X"]], .data[["val"]], fill = .data[["X"]])) +
                geom_violin(scale = "width")
        }
    } else {
        if('splitBy' %in% colnames(ggData)){
            ggOut <- ggplot(ggData, aes(
                .data[["X"]], .data[["val"]], fill = .data[["splitBy"]])) +
                geom_boxplot()
            showLegend <- TRUE
        }else{
            ggOut <- ggplot(ggData, aes(
                .data[["X"]], .data[["val"]], fill = .data[["X"]])) +
                geom_boxplot()
        }
        
    }
    if (inppts) {
        ggOut <- ggOut + geom_jitter(size = pointSize, shape = 16)
    }
    ggOut <- ggOut + xlab(infoX) + ylab(infoY) +
        sctheme(
            base_size = labelsFontsize,
            family = labelsFontFamily,
            Xang = 45,
            XjusH = 1) +
        scale_fill_manual("", values = ggCol)
    if(showLegend){
        ggOut <- ggOut +
            theme(legend.position = "bottom")
    }else{
        ggOut <- ggOut +
            theme(legend.position = "none")
    }
    return(ggOut)
}
