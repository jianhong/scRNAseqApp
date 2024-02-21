test_that("char2numeric works not correct", {
  ch <- c('CD16+ NK', '123', '1e6', '1E+7', '2e-3', '123,456', '789 789', '123-456', '0.02')
  x <- sum(is.na(scRNAseqApp:::char2numeric(ch)))
  expect_true(x==1)
})
