outputFileName <- function(ext, ...){
  paste0(paste(..., sep="_"),".", ext)
}
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
          plot = plot )
      }else{
        ggsave(
          file,
          device = device,
          height = height,
          width = width,
          plot = plot )
      }
    }
  )
}
