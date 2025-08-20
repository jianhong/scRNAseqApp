test_that("createDataset works not correct", {
  appconf <- createAppConfig(
    title="pbmc_small",
    destinationFolder = "pbmc_small",
    species = "Homo sapiens",
    doi="10.1038/nbt.3192",
    datatype = "scRNAseq",
    keywords = c("pbmc", "testdata"))
  app_path <- file.path(tempdir(), 'test_createDataset')
  dir.create(app_path, recursive = TRUE)
  on.exit(unlink(app_path, recursive = TRUE))
  createDataSet(appconf, pbmc_small, datafolder=app_path)
  p <- system.file("extdata", "data", "pbmc_small",
                   package = "scRNAseqApp")
  for(f in dir(p)){
    expect_true(file.exists(file.path(app_path, 'pbmc_small', f)))
  }
})
