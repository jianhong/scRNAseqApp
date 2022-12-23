#' waffle plot for single cell expressions
#' @noRd
#' @importFrom stats as.formula
#' @importFrom ggplot2 coord_equal facet_grid unit scale_fill_gradientn
#' @importFrom grDevices hcl.colors
#' @param expr expression table
#' @param groupCol The column name to for `facet_grid`
#' @param gradientCol Color sets
#' @param xyMaxRatio The ratio of x,y. The parameter is used to avoid long plots
scWafflePlot <- function(expr, groupCol='treatment',
                         gradientCol = rev(hcl.colors(20, "RdYlGn")),
                         xyMaxRatio = 5){
  if(length(gradientCol)==1){
    gradientCol <- .globals$cList[[gradientCol]]
  }
  if(groupCol[1] %in% colnames(expr)){
    if(!all(as.character(expr[[groupCol]]) ==
            as.character(expr$grpBy), na.rm = TRUE)){
      expr$geneName <- paste(as.character(expr$geneName),
                             as.character(expr[[groupCol]]))
    }
  }
  data <- expr[order(expr$geneName, expr$grpBy, -1*expr$val),
               c("geneName", "grpBy", "val"), with=FALSE]
  data <- data[!is.na(data$val), ]
  groupMax <- data[, {
    list(N=max(.SD[, list(N=.N), by='grpBy']$N))
  }, by='geneName']
  data <- merge(data, groupMax)
  ggData <- data[, {
    cellPerGridCell <- ceiling(.SD$N[1]/100)
    .SD[, {
      list(idx <- rep(seq.int(ceiling(.N/cellPerGridCell)),
                      each=cellPerGridCell)[seq.int(.N)])
      .SD[, {
        list(exprs=mean(.SD$val, na.rm=TRUE))
      }, by='idx']
    }, by='grpBy']
  }, by='geneName']

  ggData$x <- (ggData$idx-1)%%10+1
  ggData$y <- floor((ggData$idx-1)/10)+1

  xyRatio <- length(unique(data$geneName))/length(unique(data$grpBy))
  if(xyRatio<1/xyMaxRatio){ ## plot is too wider
    ## change the geneName
    ## add blank after geneName by grpBy group

  }else{
    if(xyRatio>xyMaxRatio){ ## plot is too long
      ## change the grpBy
      ## add blank after grpBy
    }
  }

  p <- ggplot(ggData, aes_string("x", "y", fill="exprs")) +
    geom_tile(color='white') + coord_equal() +
    facet_grid(as.formula("geneName ~ grpBy")) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    guides(fill = guide_colorbar(barwidth = unit(3, "mm"))) +
    xlab("") + ylab("") +
    scale_fill_gradientn(colors = gradientCol)
  return(p)
}


scDRwafflePlot <- function(dataset,
                           geneIdMap,
                           inpConf,
                           inpMeta,
                           gene,
                           groupBy,
                           gradientCol){
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
