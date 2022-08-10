# Plot gene expression on dimred
scDRgene <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
                     dataset, inpH5, inpGene,
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt,
                     inpPlt="Dotplot", inpXlim, inpColRange=0){
  if(inp1[1]==""){
    return(ggplot())
  }
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
                       inpConf[UI == inpsub1]$ID),
                   with = FALSE]
  colnames(ggData) = c("X", "Y", "sub")
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))

  h5file <- H5File$new(file.path(datafolder, dataset, inpH5), mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val = h5data$read(args = list(inpGene[inp1], quote(expr=)))
  ggData[val < 0]$val = 0
  h5file$close_all()
  bgCells = FALSE
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    bgCells = TRUE
    ggData2 = ggData[!sub %in% inpsub2]
    ggData = ggData[sub %in% inpsub2]
  }
  if(inpord == "Max-1st"){
    ggData = ggData[order(val)]
  } else if(inpord == "Min-1st"){
    ggData = ggData[order(-val)]
  } else if(inpord == "Random"){
    ggData = ggData[sample(nrow(ggData))]
  }
  # Actual ggplot
  if(inpPlt == "Dotplot"){
    if(inpColRange>0){
      ggData[ggData$val>inpColRange, "val"] <- inpColRange
    }
    ggOut = ggplot(ggData, aes(X, Y, color = val))
    if(bgCells){
      ggOut = ggOut +
        geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16)
    }
    ggOut = ggOut +
      geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) +
      sctheme(base_size = sList[inpfsz], XYval = inptxt) +
      guides(color = guide_colorbar(barwidth = 15))
    if(inpColRange>0){
      ggOut = ggOut +
        scale_color_gradientn(inp1, colours = cList[[inpcol]], limits=c(0, inpColRange))
    }else{
      ggOut = ggOut +
        scale_color_gradientn(inp1, colours = cList[[inpcol]])
    }
    if(inpasp == "Square") {
      ggOut = ggOut + coord_fixed(ratio = rat)
    } else if(inpasp == "Fixed") {
      ggOut = ggOut + coord_fixed()
    }
  }else{## ridgePlot
    #print(inpXlim)
    ggData$sub <- factor(ggData$sub,
                         levels=rev(sortLevels(as.character(unique(ggData$sub)))))
    ggOut = ggplot(ggData, aes(x = val, y = sub, fill = sub)) +
      geom_density_ridges(scale = 4, show.legend = FALSE) +
      theme_ridges() +
      scale_y_discrete(expand = c(0.01, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      ylab("Subsets") +
      xlab("Expression Level")
    if(length(inpXlim)==2){
      ggOut <- ggOut + xlim(inpXlim)
    }
  }

  return(ggOut)
}
