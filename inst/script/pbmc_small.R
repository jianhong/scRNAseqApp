library(scRNAseqApp)
library(Seurat)
appconf <- createAppConfig(
  title="pbmc_small",
  destinationFolder = "pbmc_small",
  species = "Homo sapiens",
  doi="10.1038/nbt.3192",
  datatype = "scRNAseq",
  keywords = c("pbmc", "testdata"))
target='inst/extdata/data'
dir.create(target)
createDataSet(appconf, pbmc_small, datafolder=target)
