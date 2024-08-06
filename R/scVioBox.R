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
        filterKey,
        filterVal,
        dataset,
        inpGene,
        inptyp,
        inppts,
        pointSize,
        labelsFontsize,
        reorder=FALSE,
        orderX,
        splitBy,
        sreorder=FALSE,
        orderS) {
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
        ggData$val <- read_exprs(dataset, inpGene[infoY], valueOnly = TRUE)
        ggData[ggData$val < 0]$val <- 0
        tmpNoise <-
            rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
        ggData$val <- ggData$val + tmpNoise
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
    if (filterKey %in% inpConf$UI) {
        ggData$filter <- inpMeta[[inpConf[inpConf$UI == filterKey]$ID]]
        if (length(filterVal)) {
            ggData <- ggData[ggData$filter >= filterVal[1], , drop = FALSE]
        }
    } else {
        ggData$filter <-
            read_exprs(dataset, inpGene[filterKey], valueOnly = TRUE)
        if (length(filterVal)) {
            ggData <- ggData[ggData$filter >= filterVal[1], , drop = FALSE]
        }
    }
    
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    ggData <- subGrp(ggData, subsetCellKey, subsetCellVal, inpConf)
    
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
            base_size = .globals$sList[labelsFontsize],
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
