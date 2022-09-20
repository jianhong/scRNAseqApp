loadData <- function(dataSource, datafolder){
  dataSource$sc1conf = readRDS(file.path(datafolder, dataSource$dataset, "sc1conf.rds"))
  dataSource$sc1def  = readRDS(file.path(datafolder, dataSource$dataset, "sc1def.rds"))
  dataSource$sc1gene = readRDS(file.path(datafolder, dataSource$dataset, "sc1gene.rds"))
  dataSource$sc1meta = readRDS(file.path(datafolder, dataSource$dataset, "sc1meta.rds"))
  return(dataSource)
}
