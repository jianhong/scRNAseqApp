pkgs <- available.packages(repos = repositories())
orgPkgs <- rownames(pkgs)[grepl("org.", rownames(pkgs), fixed = TRUE)]
BiocManager::install(orgPkgs)
orgPkgs <- c("org.Ce.eg.db", "org.Cf.eg.db", "org.Dm.eg.db", "org.Dr.eg.db",
             "org.Gg.eg.db", "org.Hs.eg.db", "org.Mm.eg.db", "org.Mmu.eg.db",
             "org.Pt.eg.db", "org.Rn.eg.db", "org.Ss.eg.db", "org.Xl.eg.db")
gns <- lapply(orgPkgs, function(org){
  library(org, character.only = TRUE)
  symbol <- as.list(get(sub(".db", "SYMBOL", org)))
  symbol <- vapply(symbol, `[`, FUN.VALUE = character(1L), i=1)
  genename <- as.list(get(sub(".db", "GENENAME", org)))
  names(genename) <- symbol[names(genename)]
  gn <- rep(names(genename), lengths(genename))
  names(gn) <- unlist(genename)
  gn
})

gnn <- unlist(lapply(gns, names))
sym <- unlist(gns)
gnn <- tolower(gnn)
sym <- tolower(sym)
gn <- split(sym, gnn)
gn <- lapply(gn, unique)
gn <- lapply(gn, function(.ele) .ele[!grepl("^(loc|gm)\\d+", .ele)])
table(lengths(gn))
gn1 <- unlist(gn[lengths(gn)==1])
gn2 <- gn[lengths(gn)>1]
res <- list('unique'=gn1, 'multiple'=gn2)
saveRDS(res, 'inst/extdata/gn2sym.rds')
