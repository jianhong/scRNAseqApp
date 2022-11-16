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

