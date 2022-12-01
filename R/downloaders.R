outputFileName <- function(ext, ...){
  paste0(paste(..., sep="_"),".", ext)
}
#' @noRd
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler
plotsDownloadHandler <- function(device, width, height, plot, ...){
  downloadHandler(
    filename = function() {
      outputFileName(device, ...)
    },
    content = function(file) {
      if(device=="pdf"){
        ggsave(
          file,
          device = device,
          height = height,
          width = width,
          useDingbats = FALSE,
          plot = plot)
      }else{
        ggsave(
          file,
          device = device,
          height = height,
          width = width,
          plot = plot)
      }
    }
  )
}
#' @importFrom grDevices dev.off pdf
heatmapDownloadHandler <- function(device, width, height, plot, ...){
  downloadHandler(
    filename = function() {
      outputFileName(device, ...)
    },
    content = function(file) {
      if(device=="pdf"){
        pdf(
          file,
          height = height,
          width = width,
          useDingbats = FALSE)
      }else{
        get(device)(
          file,
          height = height,
          width = width)
      }
      print(plot)
      dev.off()
    }
  )
}
#' @importFrom plotly event_data
exprDownloadHandler <- function(geneIdMap, dataset, meta){
  downloadHandler(
    filename = function(){
      paste0('exprdata-', dataset, '.csv')
    },
    content = function(file){
      d <- event_data("plotly_click")
      expr <- NULL
      if (!is.null(d)){
        cell <- which(meta$sampleID==d$customdata)
        if(!is.null(cell)){
          expr <- read_exprs(h5f=dataset, valueOnly=TRUE, cell=cell)
          names(expr) <- names(geneIdMap[order(geneIdMap)])
          expr <- t(t(expr))
          colnames(expr) <- d$customdata
        }
      }
      write.csv(expr, file)
    }
  )
}
