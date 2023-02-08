#' @importFrom data.table .N
scDRnum <- function(
        inpConf,
        inpMeta,
        inpCellInfo,
        inpGeneName,
        inpsubName,
        inpsubValue,
        dataset,
        inpGene,
        inpsplt) {
    if (length(inpCellInfo) > 1) {
        inpCellInfoSubGroup <- inpCellInfo[-1]
        inpCellInfo <- inpCellInfo[1]
    } else{
        inpCellInfoSubGroup <- NULL
    }
    # Prepare ggData
    ggData <- inpMeta[, c(
        inpConf[inpConf$UI == inpCellInfo]$ID,
        inpConf[inpConf$UI == inpsubName]$ID),
        with = FALSE, drop = FALSE]
    if (nrow(ggData) < 1) {
        dt <- data.frame(
            "group" = numeric(),
            "nCells" = numeric(),
            "nExpress" = numeric(),
            "pctExpress" = numeric()
        )
        return(dt)
    }
    colnames(ggData) <- c("group", "sub")
    ggData$val2 <-
        read_exprs(dataset, inpGene[inpGeneName], valueOnly = TRUE)
    ggData[ggData$val2 < 0]$val2 <- 0
    if (length(inpsubValue) != 0 &
        length(inpsubValue) != nlevels(ggData$sub)) {
        ggData <- ggData[ggData$sub %in% inpsubValue]
    }
    if (length(inpCellInfoSubGroup) != 0 &
        length(inpCellInfoSubGroup) != nlevels(ggData$group)) {
        ggData <- ggData[ggData$group %in% inpCellInfoSubGroup]
    }
    
    # Split inpCellInfo if necessary
    if (is.na(inpConf[inpConf$UI == inpCellInfo]$fCL)) {
        if (inpsplt == "Quartile") {
            nBk <- 4
        }
        if (inpsplt == "Decile") {
            nBk <- 10
        }
        ggData$group <- cut(ggData$group, breaks = nBk)
    }
    
    # Actual data.table
    ggData$express <- FALSE
    ggData[ggData$val2 > 0]$express <- TRUE
    ggData1 <-
        ggData[ggData$express == TRUE, list(nExpress = .N), by = "group"]
    ggData <- ggData[, list(nCells = .N), by = "group"]
    ggData <- ggData1[ggData, on = "group"]
    ggData <- ggData[, c("group", "nCells", "nExpress"), with = FALSE]
    ggData[is.na(ggData$nExpress)]$nExpress <- 0
    ggData$pctExpress <- 100 * ggData$nExpress / ggData$nCells
    ggData <- ggData[order(ggData$group)]
    colnames(ggData)[3] <-
        paste0(colnames(ggData)[3], "_", inpGeneName)
    return(ggData)
}
