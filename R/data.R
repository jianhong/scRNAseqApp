datafolder <- "data"
defaultDataset <- "GSM5023610_glial_app"

## datasets is a named vector with data folder names
## refs_xxxx is names vector of reference informations.
#datasets <- c("GSE150871_spinal_cord_YiLi_etal_2020"="GSE150871_microglia")
#refs_authors <- c("GSE150871"="Li Y, He X, Kawaguchi R, Zhang Y, Wang Q, Monavarfeshani A, Yang Z, Chen B, Shi Z, Meng H, Zhou S, Zhu J, Jacobi A, Swarup V, Popovich PG, Geschwind DH, He Z.")
#refs_titles <- c("GSE150871"="Microglia-organized scar-free spinal cord repair in neonatal mice")
#refs_journals <- c("GSE150871"="Nature")
#refs_years <- c("GSE150871"="(2020) ")
#refs_pmids <- c("GSE150871"="33029008")

## the datalist file is described in data.R.
# source(file.path(datafolder, "datalist.R"))
# if(file.exists(file.path(datafolder, "token.R"))){
#   source(file.path(datafolder, "token.R"))
# }
#' @include datalist.R
#' @include token.R
if(!defaultDataset %in% datasets){
  defaultDataset <- datasets[1]
}

loadData <- function(dataSource){
  dataSource$sc1conf = readRDS(file.path(datafolder, dataSource$dataset, "sc1conf.rds"))
  dataSource$sc1def  = readRDS(file.path(datafolder, dataSource$dataset, "sc1def.rds"))
  dataSource$sc1gene = readRDS(file.path(datafolder, dataSource$dataset, "sc1gene.rds"))
  dataSource$sc1meta = readRDS(file.path(datafolder, dataSource$dataset, "sc1meta.rds"))
  return(dataSource)
}
