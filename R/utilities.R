#' Function to extract legend
#' @noRd
#' @importFrom ggplot2 ggplot_gtable ggplot_build
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

#' read expression from h5 file
#' @noRd
#' @param h5filename Filename of h5 file
#' @param genesID genes IDs retrieved from sc1gene.rds
#' @param meta meta data by loading sc1meta.rds
#' @param config configs by loading sc1conf.rds
#' @param groupName The group name in the metadata colnames
#' @param valueOnly return the values of first gene
#' @return If valueOnly is TRUE, return expression values for first gene.
#' Otherwise, return a data.table with expressions and group information.
#' @importFrom hdf5r H5File
#'
read_exprs <- function(h5filename, genesID, meta,
                       config, groupName, valueOnly=FALSE){
  h5file <- H5File$new(h5filename, mode = "r")
  on.exit(h5file$close_all())
  h5data <- h5file[["grp"]][["data"]]
  if(valueOnly){
    expr <- h5data$read(args = list(genesID[1], quote(expr=)))
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
#' check if a symbol is a gene
#' @noRd
#' @param symbol the character to be checked
#' @param dict the gene symbol dictionary available in the data folder
isGene <- function(symbol, dict){
  g <- sum(grepl(symbol, dict))
  g > 0 && g < 3
}
#' get cell type column name
#' @noRd
#' @param config config data table
#' @param celltypePattern the pattern of cell type column name
getCelltypeCol <- function(config, celltypePattern='celltype'){
  groupName <- config[config$grp]$ID
  ad <- adist(celltypePattern, groupName, ignore.case = TRUE)[1, ]
  groupName[which.min(ad)]
}

#' waffle plot
#' @noRd
#' @param expres expression table returned by `read_exprs`
#' @importFrom utils adist
wafflePlot <- function(expres, plotname){
  list(
    UI = plotOutput(plotname, width = '100%', height = '280px'),
    PLOT = renderPlot(scWafflePlot(expres))
  )
}
#' get search result by gene name
#' @noRd
#' @param gene character(1L), gene name
#' @param datafolder the data folder
#' @param id namespace
#' @param geneIdFilename gene id file name
#' @param metaFilename meta data file name
#' @param configFilename config file name
#' @return Html tags for search results
#' @param exprsFilename gene expression h5 file name
checkGene <- function(gene, datafolder, id,
                      geneIdFilename='sc1gene.rds',
                      metaFilename='sc1meta.rds',
                      configFilename='sc1conf.rds',
                      exprsFilename='sc1gexpr.h5'){
  appconfs <- getAppConf(datafolder = datafolder)
  exprs <- lapply(appconfs, function(.ele){
    geneIds <- readRDS(file.path(datafolder, .ele$id, geneIdFilename))
    genenames <- geneIds[grepl(gene, names(geneIds), ignore.case = TRUE)]
    if(length(genenames)>0){
      config <- readRDS(file.path(datafolder, .ele$id, configFilename))
      groupName <- getCelltypeCol(config)
      ggData <-
        read_exprs(file.path(datafolder, .ele$id, exprsFilename),
                   genenames,
                   readRDS(file.path(datafolder, .ele$id, metaFilename)),
                   config, groupName, valueOnly=FALSE)
      ggData[ggData$val < 0]$val <- 0
      #waffle plot
      plotname = paste0('search-plot', .ele$id)
      wp <- wafflePlot(ggData, NS(id, plotname))
      list(
        UI = tags$li(
          tags$a(href=paste0('?data=', .ele$id, '&gene=',
                             paste(names(genenames), collapse=";")),
                 .ele$title),
          wp$UI
        ),
        PLOT = wp$PLOT,
        NAME = plotname
      )
    }else{
      NULL
    }
  })
  exprs <- exprs[lengths(exprs)>0]
  if(length(exprs)==0){
    return(list(UI=tagList(), PLOT=NULL))
  }else{
    plots <- lapply(exprs, function(.ele){
      .ele$PLOT
    })
    names(plots) <- vapply(exprs, function(.ele) .ele$NAME, character(1L))
    exprs <- lapply(exprs, function(.ele){
      .ele$UI
    })
    return(
      list(
        UI=tags$ul(exprs),
        PLOT=plots
        )
      )
  }
}
