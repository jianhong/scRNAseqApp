# Plot violin / boxplot
#' @importFrom stats rnorm
#' @importFrom ggplot2 ggplot aes .data geom_violin geom_boxplot
#' geom_jitter xlab ylab scale_fill_manual theme
scVioBox <- function(
        inpConf,
        inpMeta,
        infoX,
        infoY,
        grpKey,
        grpVal,
        filterKey,
        filterVal,
        dataset,
        inpGene,
        inptyp,
        inppts,
        pointSize,
        labelsFontsize,
        reorder=FALSE,
        orderX) {
    # Prepare ggData
    ggData <- inpMeta[, c(
        inpConf[inpConf$UI == infoX]$ID,
        inpConf[inpConf$grp == TRUE]$ID),
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
    
    ggData <- subGrp(ggData, grpKey, grpVal, inpConf)
    
    # Do factoring
    if(reorder){
        ggData$X <- factor(ggData$X, levels=orderX)
    }else{
        ggData <- relevelData(ggData, "X")
    }
    ggCol <- relevelCol(inpConf, infoX, ggData, "X")
    
    # Actual ggplot
    if (inptyp == "violin") {
        ggOut <- ggplot(ggData, aes(
            .data[["X"]], .data[["val"]], fill = .data[["X"]])) +
            geom_violin(scale = "width")
    } else {
        ggOut <- ggplot(ggData, aes(
            .data[["X"]], .data[["val"]], fill = .data[["X"]])) +
            geom_boxplot()
    }
    if (inppts) {
        ggOut <- ggOut + geom_jitter(size = pointSize, shape = 16)
    }
    ggOut <- ggOut + xlab(infoX) + ylab(infoY) +
        sctheme(
            base_size = .globals$sList[labelsFontsize],
            Xang = 45,
            XjusH = 1) +
        scale_fill_manual("", values = ggCol) +
        theme(legend.position = "none")
    return(ggOut)
}
