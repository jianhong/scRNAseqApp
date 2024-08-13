# Plot proportion plot
#' @importFrom ggplot2 ggplot aes .data geom_col ylab coord_flip xlab
#' scale_fill_manual theme
#' @importFrom data.table .SD .N
scProp <- function(
        inpConf,
        inpMeta,
        infoX,
        infoY,
        subsetCellKey,
        subsetCellVal,
        inptyp,
        flipXY,
        labelsFontsize = 24,
        labelsFontFamily = 'Helvetica',
        dataset,
        geneIdMap,
        valueFilterKey,
        valueFilterCutoff,
        reorder=FALSE,
        orderX,
        orderY) {
    # Prepare ggData
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    if (sum(lengths(subsetCellVal))) {
        colN <- unique(c(
            inpConf[inpConf$UI == infoX]$ID,
            inpConf[inpConf$UI == infoY]$ID,
            inpConf[inpConf$UI %in% subsetCellKey]$ID))
    } else{
        colN <- c(
            inpConf[inpConf$UI == infoX]$ID,
            inpConf[inpConf$UI == infoY]$ID)
    }
    ggData <- inpMeta[, colN, with = FALSE]
    ggData <- subGrp(ggData, subsetCellKey, subsetCellVal, inpConf)
    subFilterColname <- 'subValue'
    ggData <-
        cbindFilterValues(
            ggData,
            inpConf,
            inpMeta,
            subFilterColname,
            geneIdMap,
            dataset,
            valueFilterKey,
            valueFilterCutoff
        )
    keep <- filterCells(
        ggData,
        valueFilterKey = subFilterColname,
        valueFilterCutoff = valueFilterCutoff,
        inpConf=inpConf)
    
    if(ncol(ggData)==1){
        ggData <- cbind(ggData, ggData) ## X and color grp are same.
    }
    ggData <- ggData[keep, c(1, 2), drop = FALSE]
    colnames(ggData) <- c("X", "grp")
    ggData <- ggData[, list(nCells = .N), by = c("X", "grp")]
    ggData <- ggData[, {
        tot <- sum(.SD$nCells)
        .SD[, list(
            pctCells = 100 * sum(.SD$nCells) / tot,
            nCells = .SD$nCells), by = "grp"]
    },
    by = "X"]
    
    # Do factoring
    if(reorder){
        ggData$X <- factor(ggData$X, levels=orderX)
        ggData$grp <- factor(ggData$grp, levels=orderY)
    }else{
        ggData <- relevelData(ggData, "grp")
        ggData <- relevelData(ggData, "X")
    }
    ggCol <- relevelCol(inpConf, infoY, ggData, "grp")
    
    # Actual ggplot
    if (inptyp == "Proportion") {
        ggOut <- ggplot(ggData, aes(
            .data[["X"]], .data[["pctCells"]], fill = .data[["grp"]])) +
            geom_col() + ylab("Cell Proportion (%)")
    } else {
        ggOut <- ggplot(ggData, aes(
            .data[["X"]], .data[["nCells"]], fill = .data[["grp"]])) +
            geom_col() + ylab("Number of Cells")
    }
    if (flipXY) {
        ggOut <- ggOut + coord_flip()
    }
    ggOut <- ggOut + xlab(infoX) +
        sctheme(
            base_size = labelsFontsize,
            family = labelsFontFamily,
            Xang = 45,
            XjusH = 1) +
        scale_fill_manual("", values = ggCol) +
        theme(legend.position = "right")
    return(ggOut)
}
