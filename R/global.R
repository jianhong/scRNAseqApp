### Useful stuff
globalVariables(c(".", "Freq", "UI", "Var1", "X", "Y",
                  "fID", "gene", "geneName", "group", "grp", "grpBy",
                  "nCells", "nExpress", "pctCells", "sampleID",
                  "sub4", "v0", "v1", "v2",
                  "val", "val1", "val2", "vv1", "vv2",
                  "x", "xend", "y", "yend",
                  "present", "prop", "dimred", "express"))
# global variables
.globals <- new.env(parent = emptyenv())
# Colour palette
.globals$cList <- list(
  "White-Red" =
    c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
      "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"),
  "Blue-Yellow-Red" =
    c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF",
      "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)],
  "Yellow-Green-Purple"=
    c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C",
      "#2C728E","#3B528B","#472D7B","#440154"),
  "Blue-DarkOrange" =
    c("#1E8E99", "#51C3CC", "#99F9FF", "#B2FCFF",
      "#CCFEFF", "#E5FFFF", "#FFE5CC", "#FFCA99",
      "#FFAD65", "#FF8E32", "#CC5800", "#993F00"),
  "Green-White-Magenta" =
    c("#005000", "#008600", "#00BB00", "#00F100",
      "#50FF50", "#86FF86", "#BBFFBB", "#FFFFFF",
      "#FFF1FF", "#FFBBFF", "#FF86FF", "#FF50FF",
      "#F100F1", "#BB00BB", "#860086", "#500050"),
  "Viridis" =
    c('#440154FF','#472D7BFF','#3B528BFF','#2C728EFF','#21908CFF',
      '#27AD81FF','#5DC863FF','#AADC32FF','#FDE725FF'))


# Panel sizes
.globals$pList1 <- c("Small"="400px", "Medium"="600px", "Large"="800px")
.globals$pList2 <- c("Small"="500px", "Medium"="700px", "Large"="900px")
.globals$pList3 <- c("Small"="600px", "Medium"="800px", "Large"="1000px")
.globals$sList <- c("Small"=18, "Medium"=24, "Large"=30)
.globals$lList <- c("Small"=5, "Medium"=6, "Large"=7)

# supported organisms
.globals$supported_organisms <- c("Bos taurus",
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
# tab terms
.globals$terms <- list(
  scRNAseq=c(
    GeneExpr="GeneExpr",
    coexpression="coexpression",
    expression="expression"
  ),
  scATACseq=c(
    GeneExpr="GeneAccs",
    coexpression="co-accessibility",
    expression="accessibility"
  )
)


