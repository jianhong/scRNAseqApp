### Useful stuff
globalVariables(c(".", "Freq", "UI", "Var1", "X", "Y",
                  "fID", "gene", "geneName", "group", "grp", "grpBy",
                  "nCells", "nExpress", "pctCells", "sampleID",
                  "sub4", "v0", "v1", "v2",
                  "val", "val1", "val2", "vv1", "vv2",
                  "x", "xend", "y", "yend",
                  "present", "prop", "dimred", "express"))
# Colour palette
cList = list(c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"),
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF",
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)],
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C",
               "#2C728E","#3B528B","#472D7B","#440154"),
             c("#1E8E99", "#51C3CC", "#99F9FF", "#B2FCFF",
               "#CCFEFF", "#E5FFFF", "#FFE5CC", "#FFCA99",
               "#FFAD65", "#FF8E32", "#CC5800", "#993F00"),
             c("#005000", "#008600", "#00BB00", "#00F100",
               "#50FF50", "#86FF86", "#BBFFBB", "#FFFFFF",
               "#FFF1FF", "#FFBBFF", "#FF86FF", "#FF50FF",
               "#F100F1", "#BB00BB", "#860086", "#500050"),
             c('#440154FF','#472D7BFF','#3B528BFF','#2C728EFF','#21908CFF',
               '#27AD81FF','#5DC863FF','#AADC32FF','#FDE725FF'))
names(cList) = c("White-Red",
                 "Blue-Yellow-Red",
                 "Yellow-Green-Purple",
                 "Blue-DarkOrange",
                 "Green-White-Magenta",
                 "Viridis")

# Panel sizes
pList = c("400px", "600px", "800px")
names(pList) = c("Small", "Medium", "Large")
pList2 = c("500px", "700px", "900px")
names(pList2) = c("Small", "Medium", "Large")
pList3 = c("600px", "800px", "1000px")
names(pList3) = c("Small", "Medium", "Large")
sList = c(18,24,30)
names(sList) = c("Small", "Medium", "Large")
lList = c(5,6,7)
names(lList) = c("Small", "Medium", "Large")
