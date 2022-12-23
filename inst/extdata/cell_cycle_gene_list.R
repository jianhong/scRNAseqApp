library(biomaRt)
orgs <- c("Bos taurus",
          "Caenorhabditis elegans",
          "Drosophila melanogaster",
          "Danio rerio",
          "Gallus gallus",
          "Homo sapiens",
          "Mus musculus",
          "Macaca mulatta",
          "Pan troglodytes",
          "Rattus norvegicus",
          "Saccharomyces cerevisiae",
          "Sus scrofa")
orgs1 <- do.call(rbind, strsplit(orgs, " "))
orgs1 <- orgs1[, c(1, 2)]
homologN <- paste0(tolower(substr(orgs1[, 1], start = 1, stop = 1)), orgs1[, 2])
attrs <- lapply(homologN, function(.ele) paste0(.ele, c("_homolog_ensembl_gene","_homolog_associated_gene_name")))
names(attrs) <- orgs
s.genes <- cc.genes.updated.2019$s.genes
g2m.genes <- cc.genes.updated.2019$g2m.genes

mart <- useMart("ensembl", "hsapiens_gene_ensembl")
genes <- c(s.genes, g2m.genes)
genes <- getBM(attributes = c("ensembl_gene_id", "hgnc_symbol"),
               filters = "hgnc_symbol",
               values = genes, mart = mart)
genemap <- list()
for(i in seq_along(attrs)){
  tryCatch(
    {
      .genemap <- getBM(attributes = c("ensembl_gene_id",
                                       attrs[[i]]),
                        filters = "ensembl_gene_id",
                        values = genes$ensembl_gene_id, mart = mart)
      genemap[[names(attrs)[i]]] <- merge(genes, .genemap)
    },
    error = function(.e) message(.e)
  )
}
genemap[["Homo sapiens"]] <- cbind(genes, genes)
colnames(genemap[["Homo sapiens"]])[3:4] <- attrs[["Homo sapiens"]]
cc.genes.list.ensembl <- mapply(genemap, attrs[names(genemap)], FUN=function(.ele, .hn){
  s_genes <- .ele[.ele$hgnc_symbol %in% s.genes, .hn[1]]
  s_genes <- s_genes[s_genes!=""]
  g2m_genes <- .ele[.ele$hgnc_symbol %in% g2m.genes, .hn[1]]
  g2m_genes <- g2m_genes[g2m_genes!=""]
  list(s.genes=s_genes, g2m.genes=g2m_genes)
}, SIMPLIFY = FALSE)
cc.genes.list.symbol <- mapply(genemap, attrs[names(genemap)], FUN=function(.ele, .hn){
  s_genes <- .ele[.ele$hgnc_symbol %in% s.genes, .hn[2]]
  s_genes <- s_genes[s_genes!=""]
  g2m_genes <- .ele[.ele$hgnc_symbol %in% g2m.genes, .hn[2]]
  g2m_genes <- g2m_genes[g2m_genes!=""]
  list(s.genes=s_genes, g2m.genes=g2m_genes)
}, SIMPLIFY = FALSE)

cc.genes.list <- list("ensembl"=cc.genes.list.ensembl,
                      "symbol"=cc.genes.list.symbol)
cc.genes.list <- ChIPpeakAnno:::swapList(cc.genes.list)

saveRDS(cc.genes.list, "../inst/extdata/cc.genes.list.rds")
