test_that("utilities works not correct", {
  app_path=tempdir()
  scInit(app_path=app_path)
  expect_true(file.exists(file.path(app_path, 'doc.txt')))
  expect_true(file.exists(file.path(app_path, .globals$credential_path)))
  expect_true(file.exists(file.path(app_path, 'www', 'counter.tsv')))
  setwd(app_path)
  
  # app <- scRNAseqApp()
  # env <- environment(app$serverFuncSource)
  # testServer(env$server, {
  #     session$setInputs(selectedDatasets = 'pbmc_small')
  # })
  # created by record_test()
  # review by testthat::snapshot_review('scRNAseqApp/')
  app <- shinytest2::AppDriver$new(app_dir='.', name = "testAllFun",
                                   variant='pbmc_small',
                                   width = 1080,
                                   height = 960)
  app$set_inputs(remote_addr = '127.0.0.1', allow_no_input_binding_ = TRUE)
  ## select the default dataset
  app$set_inputs(selectedDatasets = 'pbmc_small')
  app$set_inputs(`cellInfoGeneExpr-GeneName2` = "PPBP")
  app$set_inputs(`geneExprGeneExpr-GeneName1` = "PPBP")
  app$set_inputs(`geneExprGeneExpr-GeneName2` = "IGLL5")
  app$set_inputs(`coExpr-GeneName1` = "PPBP")
  app$set_inputs(`coExpr-GeneName2` = "IGLL5")
  app$set_inputs(`coExpr3d-GeneName1` = "PPBP")
  app$set_inputs(`coExpr3d-GeneName2` = "IGLL5")
  app$set_inputs(`sunburst-filterCell` = "nCount_RNA")
  app$set_inputs(`subsetGeneExpr-GeneName` = "PPBP")
  app$set_inputs(`subsetGeneExpr-filterCell` = "nCount_RNA")
  app$set_inputs(`vioBoxPlot-filterCell` = "nCount_RNA")
  app$set_inputs(`explorer-filterCell` = "nCount_RNA")
  ## check cellInfoGeneExpr
  app$set_inputs(topnav = "cellInfoGeneExpr")
  app$set_inputs(`cellInfoGeneExpr-GeneExproup.clk2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoGeneExpr-GeneExproup.dbl2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoGeneExpr-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoGeneExpr-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoGeneExpr-CellInfo1` = "letters")
  app$click("cellInfoGeneExpr-CellInfotog1")
  app$set_inputs(`cellInfoGeneExpr-CellInfocol1` = "Blue-Yellow-Red")
  app$set_inputs(`cellInfoGeneExpr-CellInfoname1` = "groups")
  app$set_inputs(`cellInfoGeneExpr-CellInfo1` = "groups")
  app$set_inputs(`cellInfoGeneExpr-CellInfo1` = "celltype")
  app$set_inputs(`cellInfoGeneExpr-GeneName2` = "CST3")
  app$set_inputs(`cellInfoGeneExpr-GeneExprdrX` = "PC1")
  # app$expect_screenshot()
  ## check cellInfoCellInfo
  app$set_inputs(topnav = "cellInfoCellInfo")
  app$set_inputs(`cellInfoCellInfo-GeneExproup.clk2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoCellInfo-GeneExproup.dbl2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoCellInfo-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoCellInfo-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`cellInfoCellInfo-subsetCell` = "letters")
  app$set_inputs(`cellInfoCellInfo-subsetCellValletters` = "B")
  app$set_inputs(`cellInfoCellInfo-CellInfo1` = "letters")
  app$set_inputs(`cellInfoCellInfo-CellInfo2` = "groups")
  app$click("cellInfoCellInfo-graphicTog")
  app$set_inputs(`cellInfoCellInfo-GeneExprfsz` = 12)
  # app$expect_screenshot()
  ## check subsetGeneExpr
  app$set_inputs(topnav = "subsetGeneExpr")
  app$set_inputs(`subsetGeneExpr-GeneExproup.clk2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`subsetGeneExpr-GeneExproup.dbl2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`subsetGeneExpr-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`subsetGeneExpr-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`subsetGeneExpr-filterCell` = "tSNE1")
  app$set_inputs(`subsetGeneExpr-filterCellVal` = c(-36, 16))
  app$set_inputs(`subsetGeneExpr-filterCellVal2` = c(-7, 24))
  app$click("subsetGeneExpr-GeneExprtog1")
  app$set_inputs(`subsetGeneExpr-GeneExprhid1` = TRUE)
  app$click("subsetGeneExpr-GeneExprtog2")
  app$set_inputs(`subsetGeneExpr-GeneExprhid2` = TRUE)
  app$click("subsetGeneExpr-CellInfoTableTog1")
  app$set_inputs(`subsetGeneExpr-GeneExpr.dt1_rows_current` = c(1, 2),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`subsetGeneExpr-GeneExpr.dt1_rows_all` = c(1, 2),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`subsetGeneExpr-GeneExpr.dt1_state` = 
                     c(1753799453390, 0, -1, "", 
                       TRUE, FALSE, TRUE, 
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE)), 
                 allow_no_input_binding_ = TRUE)
  #app$expect_download("subsetGeneExpr-GeneExproup.dwn1")
  # app$expect_screenshot()
  ## check geneExprGeneExpr
  app$set_inputs(topnav = "geneExprGeneExpr")
  app$set_inputs(`geneExprGeneExpr-GeneExproup.clk2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`geneExprGeneExpr-GeneExproup.dbl2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`geneExprGeneExpr-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`geneExprGeneExpr-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$click("geneExprGeneExpr-graphicTog")
  app$click("geneExprGeneExpr-graphicTog")
  app$click("geneExprGeneExpr-GeneExprtog2")
  app$set_inputs(`geneExprGeneExpr-GeneExprtype2` = "Ridgeplot")
  app$click("geneExprGeneExpr-GeneExprtog1")
  app$click("geneExprGeneExpr-GeneExprrgb1")
  app$set_inputs(`geneExprGeneExpr-GeneExprrg1` = 8)
  # app$expect_screenshot()
  ## check coExpr
  app$set_inputs(topnav = "coExpr")
  app$set_inputs(`coExpr-coExpr.dt_rows_current` = c(1, 2, 3),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-coExpr.dt_rows_all` = c(1, 2, 3),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-coExpr.dt_state` = 
                     c(1753799510042, 0, -1, "", TRUE, FALSE, TRUE,
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE)),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-GeneExproup.clk2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-GeneExproup.dbl2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-GeneName1` = "COTL1")
  app$set_inputs(`coExpr-coExpr.dt_rows_current` = c(1, 2, 3, 4),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-coExpr.dt_rows_all` = c(1, 2, 3, 4),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`coExpr-coExpr.dt_state` = 
                     c(1753799513828, 0, -1, "", TRUE, FALSE, TRUE,
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE)),
                 allow_no_input_binding_ = TRUE)
  # app$expect_screenshot()
  ## check coExpr3d
  app$set_inputs(topnav = "coExpr3d")
  app$set_inputs(`plotly_hover-A` = character(0),
                 allow_no_input_binding_ = TRUE, 
                 priority_ = "event")
  app$set_inputs(topnav = "sunburst")
  app$set_inputs(`sunburst-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`sunburst-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`sunburst-genelist` = "PPBP, IGLL5, VDAC3")
  app$set_inputs(`sunburst-CoExpred` = FALSE)
  app$set_inputs(`sunburst-filterCell` = "tSNE1")
  app$set_inputs(`sunburst-filterCellVal` = c(27, 46))
  app$set_inputs(`sunburst-filterCellVal2` = c(-3, 24))
  app$click("sunburst-graphicTog")
  app$set_inputs(`sunburst-GeneExprtxt` = TRUE)
  app$set_inputs(`sunburst-filterCellVal2` = c(6, 24))
  app$set_inputs(`sunburst-filterCellVal2` = c(9, 24))
  # app$expect_screenshot()
  ## check vioBoxPlot
  app$set_inputs(topnav = "vioBoxPlot")
  app$set_inputs(`vioBoxPlot-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`vioBoxPlot-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`vioBoxPlot-CellInfoY` = "letters")
  app$set_inputs(`vioBoxPlot-subsetCell` = "celltype")
  app$set_inputs(`vioBoxPlot-filterCellVal` = c(41, 506))
  app$set_inputs(`vioBoxPlot-plottyp` = "boxplot")
  app$set_inputs(`vioBoxPlot-plotpts` = TRUE)
  app$set_inputs(`vioBoxPlot-addnoise` = FALSE)
  app$set_inputs(`vioBoxPlot-plotord` = TRUE)
  app$set_inputs(`vioBoxPlot-cellinfoXorder` = c("g1", "g2"),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`vioBoxPlot-cellinfoXorder` = c("g2", "g1"),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`vioBoxPlot-plotsord` = TRUE)
  app$set_inputs(`vioBoxPlot-cellinfoSorder` = c("A", "B"),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`vioBoxPlot-cellinfoSorder` = c("B", "A"),
                 allow_no_input_binding_ = TRUE)
  # app$expect_screenshot()
  ## check proportion
  app$set_inputs(topnav = "proportion")
  app$set_inputs(`proportion-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`proportion-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`proportion-CellInfoY` = "letters")
  app$set_inputs(`proportion-plottyp` = "CellNumbers")
  app$set_inputs(`proportion-plotflp` = TRUE)
  app$set_inputs(`proportion-plotord` = TRUE)
  app$set_inputs(`proportion-cellinfoYorder` = 
                     c("A", "B"), allow_no_input_binding_ = TRUE)
  app$set_inputs(`proportion-cellinfoXorder` = 
                     c("CD16 Mono", "CD14 Mono", "CD16+ NK", 
                       "CD1c DC", "CD4 Memory", "CD4 Naive", "CD8 Effector 1",
                       "CD8 Effector 2", "CD8 Memory", 
                       "CD8 Naive", "Intermediate Mono", "MAIT",
                       "Memory B", "Naive B", "Platelets"), 
                 allow_no_input_binding_ = TRUE)
  app$click("proportion-statstog")
  app$set_inputs(`proportion-proportion.dt_rows_current` = 
                     c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`proportion-proportion.dt_rows_all` = 
                     c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`proportion-proportion.dt_state` = 
                     c(1753799688415, 0, -1, "", TRUE, FALSE, TRUE,
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE),
                       c(TRUE, "", TRUE, FALSE, TRUE)),
                 allow_no_input_binding_ = TRUE)
  # app$expect_screenshot()
  app$click("proportion-pearsontog")
  app$set_inputs(`proportion-GeneExproup.clk2` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`proportion-GeneExproup.dbl2` = character(0),
                 allow_no_input_binding_ = TRUE)
  # app$expect_screenshot()
  ## check bubleHeatmap
  app$set_inputs(topnav = "bubbleHeatmap")
  app$set_inputs(`bubbleHeatmap-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`bubbleHeatmap-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$click("bubbleHeatmap-plottog")
  app$set_inputs(`bubbleHeatmap-plotpsz` = "Large")
  app$set_inputs(`bubbleHeatmap-plotcols` = "Viridis")
  app$set_inputs(`bubbleHeatmap-row_dend_width` = 20.2)
  app$set_inputs(`bubbleHeatmap-legend_side` = "right")
  app$set_inputs(`bubbleHeatmap-row_dend_side` = "right")
  app$set_inputs(`bubbleHeatmap-plotsiz` = 4)
  app$set_inputs(`bubbleHeatmap-plotflp` = TRUE)
  app$set_inputs(`bubbleHeatmap-plotcol` = TRUE)
  app$set_inputs(`bubbleHeatmap-plottyp` = "Heatmap")
  app$set_inputs(`bubbleHeatmap-plottyp` = "Violin")
  app$set_inputs(`bubbleHeatmap-addnoise` = FALSE)
  app$set_inputs(`bubbleHeatmap-plottyp` = "Heatmap")
  app$set_inputs(`bubbleHeatmap-CellInfoY` = "letters")
  # app$expect_screenshot()
  ## check waffle
  app$set_inputs(topnav = "waffle")
  app$set_inputs(`waffle-GeneExproup.clk1` = character(0),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`waffle-GeneExproup.dbl1` = character(0),
                 allow_no_input_binding_ = TRUE)
  # app$expect_screenshot()
  ## check explorer
  app$set_inputs(topnav = "explorer")
  app$click("explorer-newModule")
  app$set_inputs(`explorer-moduleName` = "gene expression")
  app$click("explorer-newModule")
  app$set_inputs(`explorer-plot_2-GeneName1` = "PPBP")
  app$set_inputs(`explorer-moduleName` = "proportion")
  app$click("explorer-newModule")
  app$set_inputs(`explorer-plot_2-GeneName1` = "PPBP")
  app$set_inputs(`explorer-moduleName` = "violin/box plot")
  app$click("explorer-newModule")
  app$set_inputs(`explorer-plot_2-GeneName1` = "PPBP")
  app$click("explorer-plot_2-moveup")
  app$set_inputs(`explorer-plot_2-GeneName1` = "PPBP")
  app$set_inputs(`plotly_hover-A` = 
                     paste0("[{\"curveNumber\":1,",
                            "\"pointNumber\":32,",
                            "\"x\":44.68867319807703,",
                            "\"y\":21.35127489929749,",
                            "\"z\":3.169925001442312,",
                            "\"customdata\":\"GTCATACTTCGCCT\"}]"), 
                 allow_no_input_binding_ = TRUE, priority_ = "event")
  # app$expect_screenshot()
})
