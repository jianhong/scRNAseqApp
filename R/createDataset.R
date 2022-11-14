#' Create a dataset
#' Create a dataset from an object
createDataSet <- function(..., species, pmid, type, markers, keywords){

}

#' Create a metadata to describe the dataset
#' 1. must contain condition, cell type, tissue, species information
#' 2. the conditions must be a keyword in database, used for whole database comparison
#' 3. cell type must be standard words
#'
#'
createMetadata <- function(){

}

#' Add slingshot lineages to the dataset
#' @importFrom slingshot getLineages
#' @param dimred reduced dim
#' @param clusterings A named list with cluster labels
#' @noRd
addSlingshot <- function(dimred, clusterings){
  stopifnot(length(names(clusterings))>0)
  lapply(clusterings, function(clustering){
    slingshot::getLineages(data = dimred, clusterLabels = clustering)
  })
}
#' co-expression data by fcoex
#' pseudotime analysis
#' assign cell type
#' assign cell cycle
#' trajectories
#' cell communications
#'
#'
#' How to summarize the data?
#' radc.rush.edu
#' ROSMAP
#' compare the expression for each cell type, tissue, species for two condiction,
#' injured vs uninjured
#' GO enrichment analysis
#'
#' integrated by corralm for one species
#'
