readData <- function(slot, folder){
  readRDS(file.path(.globals$datafolder, folder, .globals$filenames[[slot]]))
}
loadData <- function(dataSource){
  for(i in c("sc1conf", "sc1def", "sc1gene", "sc1meta")){
    dataSource[[i]] <- readData(i, dataSource$dataset)
  }
  return(dataSource)
}

saveAppConf <- function(appconf){
  pf <- file.path(.globals$datafolder, appconf$id)
  if(!file.exists(pf)) dir.create(pf, recursive = TRUE, showWarnings = FALSE)
  for(i in c("markers", "keywords")){
    appconf[[i]] <- appconf[[i]][!is.na(appconf[[i]])]
    appconf[[i]] <- appconf[[i]][appconf[[i]]!=""]
  }
  saveData(appconf, appconf$id, "appconf")
}

saveData <- function(data, folder, prefix){
  filename <- .globals$filenames[[prefix]]
  if(is.null(filename)) filename <- paste0(prefix, ".rds")
  saveRDS(data,
          file.path(.globals$datafolder, folder, filename))
}

setLocker <- function(folder){
  writeLines(character(0),
             file.path(.globals$datafolder, folder, .globals$filenames$locker))
}
removeLocker <- function(folder){
  unlink(file.path(.globals$datafolder, folder, .globals$filenames$locker))
}
writeMisc <- function(misc, folder, slot){
  if(!is.null(misc)){
    saveData(misc, folder, slot)
  }
}
#' read expression from h5 file
#' @noRd
#' @param h5f Parent folder name of h5 file
#' @param genesID genes IDs retrieved from sc1gene.rds
#' @param meta meta data by loading sc1meta.rds
#' @param config configs by loading sc1conf.rds
#' @param groupName The group name in the metadata colnames
#' @param valueOnly return the values of first gene
#' @param cell barcode/sampleID pos retrieved from sc1meta.rds
#' @return If valueOnly is TRUE, return expression values for first gene.
#' Otherwise, return a data.table with expressions and group information.
#' @importFrom hdf5r H5File
#'
read_exprs <- function(h5f, genesID, meta,
                       config, groupName, valueOnly=FALSE,
                       cell){
  h5file <- H5File$new(file.path(
    .globals$datafolder, h5f, .globals$filenames$sc1gexpr), mode = "r")
  on.exit(h5file$close_all())
  h5data <- h5file[["grp"]][["data"]]
  if(valueOnly){
    if(!missing(cell)){
      expr <- h5data$read(args = list(quote(expr=), cell[1]))
    }else{
      if(!is.na(genesID)){
        expr <- h5data$read(args = list(genesID[1], quote(expr=)))
      }else{
        expr <- 0
      }
    }
    h5file$close_all()
    on.exit()
    return(expr)
  }
  exprs <- data.table()
  for(idx in seq_along(genesID)){
    tmp <- meta[, c("sampleID",
                    config[config$grp == TRUE]$ID),
                with = FALSE]
    if(!missing(groupName)){
      tmp$grpBy <- meta[[config[config$UI == groupName]$ID]]
    }
    tmp$geneName <- names(genesID)[idx]
    tmp$val <- h5data$read(args = list(genesID[idx], quote(expr=)))
    exprs <- rbindlist(list(exprs, tmp))
  }
  h5file$close_all()
  on.exit()
  exprs
}
