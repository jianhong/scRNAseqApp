#' Create a dataset
#' Create a dataset from an object
createDataSet <- function(datafolder="data", # app data folder
                          appconf, # an appconf object
                          seu, # an suerat object
                          config, # config file for makeShinyApp
                          LOCKER=FALSE,
                          ...){
  pf <- file.path(datafolder, appconf$id)
  dir.create(pf)
  markers <- appconf$markers
  if(length(markers)==0){
    markers <- rownames(seu)[1:2]
  }else{
    if(length(markers)==1){
      markers <- c(markers, markers)
    }
  }
  ## make shiny app
  makeShinyApp(seu,
               scConf = config,
               ...,
               shiny.title = appconf$title,
               shiny.dir = pf,
               default.gene1 = markers[1],
               default.gene2 = markers[2],
               default.multigene = markers)
  saveAppConf(appconf, pf)
  ## save misc data
  for(slot in names(Misc(seu))){
    saveMisc(slot)
  }
  ## "Locker"
  if(LOCKER){
    writeLines("", file.path(pf, "LOCKER"))
  }
  ## "Clean up unused files"
  unlink(file.path(pf, "ui.R"))
  unlink(file.path(pf, "server.R"))
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
#' Add CellChat to the dataset
#' CellChat is not a R/Bioconductor available package
#' DO NOT export
#' @noRd
#' @param expr a normalized (NOT count) data matrix (genes by cells),
#'  Seurat or SingleCellExperiment object
#' @param meta metadata for the expression data
#' @param grp a char name of the variable in meta data, defining cell groups.
#' @param species species used to call CellChatDB. Available choices:
#' Homo sapiens, Mus musculus, Danio rerio
#' @param min.cells the minmum number of cells required in each cell group
#'  for cell-cell communication
#'
addCellChat <- function(expr, meta, grp, species, min.cells = 10){
  cellchat <- CellChat::createCellChat(object = expr,
                                       meta = meta,
                                       group.by=grp)
  cellchat <- CellChat::setIdent(cellchat, ident.use = grp)
  groupSize <- as.numeric(table(cellchat@idents))
  cellchat@DB <- switch(species,
                        'Homo sapiens'= CellChat::CellChatDB.human,
                        'Mus musculus'= CellChat::CellChatDB.mouse,
                        'Danio rerio' = CellChat::CellChatDB.zebrafish)
  cellchat <- CellChat::subsetData(cellchat)
  cellchat <- CellChat::identifyOverExpressedGenes(cellchat)
  cellchat <- CellChat::identifyOverExpressedInteractions(cellchat)
  cellchat <- CellChat::computeCommunProb(cellchat)
  cellchat <- CellChat::filterCommunication(cellchat, min.cells = min.cells)
  cellchat <- CellChat::computeCommunProbPathway(cellchat)
  cellchat <- CellChat::aggregateNet(cellchat)
  cellchat <-
    CellChat::netAnalysis_computeCentrality(cellchat,
                                            slot.name = "netP")
  slots <- c("LR", "net", "DB", "netP", "idents")
  names(slots) <- slots
  lapply(slots, function(.ele){
    slot(cellchat, name = .ele)
  })
}
#' Add monocle3 results to the dataset
#' monocle3 is not a R/Bioconductor available package
#' DO NOT export
#' @noRd
#' @param cds a cell_data_set object
#' @param meta metadata for the expression data
#' @param config config information
#' @param reduction_method The dimensionality reduction method upon which to
#'  base clustering. Options are "UMAP", "tSNE", "PCA" and "LSI".
#'
addMonocle <- function(cds, reduction_method, meta, config){
  stopifnot(is(cds, "cell_data_set"))
  cds <- monocle3::cluster_cells(cds=cds,
                                 reduction_method = reduction_method)

  cds <- monocle3::learn_graph(cds)
  ## return the metadata back to seu
  getMiscData <- function(cds_x){
    p <- monocle3::plot_cells(cds_x,
                              color_cells_by="pseudotime",
                              show_trajectory_graph=TRUE)
    list(
      # full data
      meta_data = p$data[, !colnames(p$data) %in% config$ID,
                         drop=FALSE]
      ,#trajectory graph segment is layer 3
      segments_layer_data = p$layers[[3]]$data
      ,# principal_points is layer 4
      principal_points_data = p$layer[[4]]$data
      ,# leaves_lable is layer 6
      mst_leaf_nodes = p$layers[[6]]$data
      ,# root lable is layer 8
      mst_root_nodes = p$layers[[8]]$data
    )
  }
  # by principal points
  ica_space_df <- t(cds@principal_graph_aux[[reduction_method]]$dp_mst)
  cds_x <- lapply(rownames(ica_space_df),
                  function(root_nodes){
                    monocle3::order_cells(cds,
                                          reduction_method = reduction_method,
                                          root_pr_nodes = root_nodes)})
  # by group points
  grp_ids <- getGrpIDs(config)
  root_cells <- lapply(grp_ids, function(.ele){
    split(rownames(meta), meta[, .ele])
  })
  cds_x1 <- lapply(root_cells, function(.ele){
    lapply(.ele, function(.e){
      monocle3::order_cells(cds,
                            reduction_method = reduction_method,
                            root_cells = .e)
    })
  })
  cds_x <- c(list(root_nodes=cds_x), cds_x1)
  miscData <- lapply(cds_x, function(.ele) lapply(.ele, getMiscData))
}

#' Add tricycle results to the dataset
#' tricycle is not a R/Bioconductor available package
#' DO NOT export
#' @noRd
#' @param exp A numeric matrix of **log-expression** values where rows are
#'  features and columns are cells.
#'  Alternatively, a SummarizedExperiment or SingleCellExperiment containing
#'  such a matrix.
#' @param meta metadata for the expression data
#' @param gname.type The type of gene names as in gname or rownames of exp.
#'  It can be either 'ENSEMBL' or 'SYMBOL'.
#' @param species It can be either 'mouse' or 'human'.
addTricycle <- function(exp, gname.type, species, meta){
  species <- match.arg(species, choices = c("human", "mouse"))
  exp <- tricycle::project_cycle_space(
    exp,
    gname.type=gname.type,
    species=species
  )
  exp <- tricycle::estimate_cycle_position(exp)
  exp <- tricycle::estimate_Schwabe_stage(
    exp,
    gname.type = gname.type,
    species = species
  )
  stopifnot(identical(rownames(meta),
                      rownames(SummarizedExperiment::colData(exp))))
  tricyclePosition <- SummarizedExperiment::colData(exp)$tricyclePosition
  CCStage <- SummarizedExperiment::colData(exp)$CCStage
  return(list(tricyclePosition=tricyclePosition,
              CCStage=CCStage))
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
