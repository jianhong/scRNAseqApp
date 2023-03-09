# Get gene list
#' @importFrom data.table data.table
scGeneList <- function(inp, inpGene) {
    geneList <- data.table(gene = unique(trimws(strsplit(
        inp,
        ",|;|\n|\r|\\s+"
    )[[1]])),
    present = TRUE)
    geneList <- geneList[geneList$gene !="", ]
    geneList[!geneList$gene %in% names(inpGene)]$present <- FALSE
    return(geneList)
}
