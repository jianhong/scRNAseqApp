#' @importFrom data.table .N
scDRcoexNum <- function(
        inpConf,
        inpMeta,
        gene1,
        gene2,
        subsetCellKey,
        subsetCellVal,
        dataset,
        geneIdMap) {
    # Prepare ggData
    ggData <-
        inpMeta[, c(inpConf[inpConf$UI %in% subsetCellKey]$ID), with = FALSE]
    ggData <- getCoexpVal(ggData, dataset, geneIdMap, gene1, gene2)
    
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        inpConf=inpConf)
    ggData <- ggData[keep]
    
    # Actual data.table
    ggData$express <- "none"
    ggData[ggData$val1 > 0]$express <- gene1
    ggData[ggData$val2 > 0]$express <- gene2
    ggData[ggData$val1 > 0 & ggData$val2 > 0]$express <- "both"
    ggData$express <- factor(
        ggData$express,
        levels = unique(c("both", gene1, gene2, "none")))
    ggData <- ggData[, list(nCells = .N), by = "express"]
    ggData$percent <- 100 * ggData$nCells / sum(ggData$nCells)
    ggData <- ggData[order(ggData$express)]
    colnames(ggData)[1] <- "expression > 0"
    return(ggData)
}
