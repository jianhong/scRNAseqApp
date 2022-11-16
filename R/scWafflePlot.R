#' waffle plot for single cell expressions
#' @noRd
#' @importFrom stats as.formula
#' @importFrom ggplot2 coord_equal facet_grid unit scale_fill_gradientn
#' @importFrom grDevices hcl.colors
scWafflePlot <- function(expr){
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
    scale_fill_gradientn(colors = rev(hcl.colors(20, "RdYlGn")))
  return(p)
}


