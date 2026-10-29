# Get gene list
#' @importFrom data.table data.table
scGeneList <- function(inp, geneIdMap) {
    geneList <- data.table(gene = unique(trimws(strsplit(
        inp,
        ",|;|\n|\r|\\s+"
    )[[1]])),
    present = TRUE)
    geneList <- geneList[geneList$gene !="", ]
    geneList[!geneList$gene %in% names(geneIdMap)]$present <- FALSE
    return(geneList)
}

scGenePairs <- function(inp, geneIdMap){
    geneList <- scGeneList(inp, geneIdMap)
    geneList$pairId <- rep(seq.int(nrow(geneList)), each=2)[seq.int(nrow(geneList))]
    geneList <- geneList[geneList$present == TRUE]
    pId <- table(geneList$pairId)
    geneList <- geneList[geneList$pairId %in% names(pId)[pId==2]]
    geneList[order(geneList$pairId, geneList$gene), ]
}
