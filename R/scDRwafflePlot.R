scDRwafflePlot <- function(dataset,
                           geneIdMap,
                           inpConf,
                           inpMeta,
                           gene,
                           groupBy,
                           gradientCol){
  saveRDS(as.list(environment()), "tmp.rds")

  if(isQuote(gene)){
    genenames <- geneIdMap[names(geneIdMap) %in%
                           removeQuote(gene)]
  }else{
    if(isAsterisk(gene)){
      gene <- isAsterisk(gene, transform = TRUE)
      genenames <- geneIdMap[grepl(gene, names(geneIdMap), ignore.case = TRUE)]
    }else{
      geneList <- scGeneList(gene, geneIdMap)
      geneList <- geneList[geneList$present == TRUE]
      genenames <- geneList$gene
    }
  }
  if(length(genenames)>0){
    #waffle plot
    if(missing(groupBy)){
      groupBy <- getCelltypeCol(inpConf,
                                celltypePattern =
                                  .globals$groupColPattern)
    }
    ggData <-
      read_exprs(dataset,
                 geneIdMap[genenames],
                 inpMeta,
                 inpConf,
                 groupBy,
                 valueOnly=FALSE)
    ggData[ggData$val < 0]$val <- 0
    return(scWafflePlot(ggData, groupBy, gradientCol))
  }else{
    return(ggplot())
  }
}
