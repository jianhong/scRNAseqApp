#' Create a dataset
#' Create a dataset from a Seurat object
#' @param datafolder app data folder
#' @param appconf a list object represent the information about the dataset
#' @param seu a Seurat object
#' @param config config file for makeShinyFiles
#' @param contrast The contrast group
#' @param LOCKER Set locker if the file is required login
#' @param assayName assay in single-cell data object to use for plotting
#'   gene expression, which must match one of the following:
#'   \itemize{
#'     \item{Seurat objects}: "RNA" or "integrated" assay,
#'       default is "RNA"
#'   }
#' @param gexSlot slot in single-cell assay to plot.
#' Default is to use the "data" slot
#' @importFrom SeuratObject Reductions Idents Assays DefaultAssay GetAssayData
#'  `DefaultAssay<-` VariableFeatures Misc `Misc<-` Embeddings `Idents<-`
#' @importFrom Seurat FindAllMarkers FindVariableFeatures ScaleData
#' @return The updated Seurat object.
#' @export
#' @examples
#' library(Seurat)
#' appconf <- createAppConfig(
#'            title="pbmc_small",
#'            destinationFolder = "pbmc_small",
#'            species = "Homo sapiens",
#'            doi="10.1038/nbt.3192",
#'            datatype = "scRNAseq")
#' createDataSet(appconf, pbmc_small, datafolder=tempdir())
createDataSet <- function(appconf,
                          seu,
                          config,
                          contrast,
                          assayName,
                          gexSlot = c("data", "scale.data", "counts"),
                          LOCKER=FALSE,
                          datafolder="data"){
  stopifnot(file.exists(datafolder))
  stopifnot(is(seu, "Seurat"))
  stopifnot(is(appconf, "APPconf"))
  gexSlot <- match.arg(gexSlot)
  if(missing(config)){
    config <- createConfig(seu)
  }
  pf <- file.path(datafolder, appconf$id)
  dir.create(pf, recursive = TRUE)
  cellInfo <- colnames(seu[[]])
  assays <- Assays(seu)
  stopifnot("Please input a seurat object with 'SCT' or 'RNA' assay" =
              any(c("SCT", "RNA") %in% assays))
  if(!DefaultAssay(seu) %in% c("SCT", "RNA")){
    DefaultAssay(seu) <- match.arg(assays, choices = c("SCT", "RNA"),
                                   several.ok = TRUE)[1]
  }
  if(missing(assayName)){
    assayName <- DefaultAssay(seu)
  }
  assayName <- assayName[1]
  stopifnot("The assayName is not in input object" = assayName %in% assays)
  if(length(GetAssayData(seu, "scale.data"))==0){
    seu <- FindVariableFeatures(seu,
                                selection.method = "vst",
                                nfeatures=1000)
    seu <- ScaleData(seu)
  }
  top10 <- head(VariableFeatures(seu), 10)
  ## markers
  markers <- appconf$markers
  if(!missing(contrast)){
    if(contrast[1] %in% cellInfo){
      appconf$groupCol <- contrast[1]
    }else{
      stop("The input contrast is not in seu object")
    }
  }else{
    if(!is.null(appconf$groupCol)){
      if(appconf$groupCol[1] %in% cellInfo){
        contrast <- appconf$groupCol[1]
      }else{
        appconf$groupCol <- appconf$groupCol[-1]
      }
    }else{
      contrast <- NULL
    }
  }
  if(length(markers)==0){
    if(!is.null(Misc(seu, "markers"))){
      ## the markers is available at Misc(seu, "markers") slot
      markers <- Misc(seu, "markers")
    }else{
      if(!is.factor(Idents(seu))){
        if(!is.null(contrast)){
          Idents(seu) <- contrast
        }else{
          grp <- cellInfo[grepl('cluster|cell(.*)type',
                                cellInfo,
                                ignore.case = TRUE)]
          grp_d <- adist('celltype', grp)
          Idents(seu) <- grp[which.min(grp_d)][1]
        }
      }
      markers <- FindAllMarkers(seu, only.pos=TRUE,
                                min.pct=.25, logfc.threshold =.25)
      if(length(markers)){
        Misc(seu, "markers") <- markers
      }
    }
    appconf$markers <- markers
  }
  if(length(markers)==0){
    markers <- top10
  }else{
    if(is.list(markers)&&!is.data.frame(markers)){
      markers <- as.data.frame(markers[[1]])
    }
    markers <- split(markers, markers$cluster)
    markers <- lapply(markers, head, n=min(5, ceiling(50/length(markers))))
    markers <- lapply(markers, function(.ele)(
      if(!is.null(.ele$gene)){
        return(.ele$gene)
      }else{
        return(rownames(.ele))
      }
    ))
    markers <- unique(unlist(markers))
  }
  ## make shiny app
  makeShinyFiles(seu,
                 scConf = config,
                 assayName = assayName,
                 gexSlot = gexSlot,
                 appDir = pf,
                 defaultGene1 = markers[1],
                 defaultGene2 = markers[2],
                 default.multigene = markers)

  .globals$datafolder <- datafolder
  saveAppConf(appconf)
  ## save misc data
  for(slot in names(Misc(seu))){
    writeMisc(Misc(seu, slot), appconf$id, slot)
  }
  ## "Locker"
  if(LOCKER){
    writeLines("", file.path(pf, "LOCKER"))
  }
  return(seu)
}


#' Create a metadata to describe the dataset
#' @description The function will return a APPconf object which contain the reference,
#' keywords for the dataset.
#' @param title The title of the dataset
#' @param destinationFolder The destination folder name of the dataset without
#'  the root folder of the datasets. The data will be saved as
#'  `appdataFolder/destinationFolder`
#' @param species The species of the dataset
#' @param doi,pmid The DOI or PMID of the reference
#' @param bibentry An object of bibentry
#' @param datatype character(1). Type of the data, scRNAseq or scATACseq.
#' @param markers A list of data.frame with gene symbols as rownames or
#'  a character vector.
#' @param keywords The keywords for the dataset.
#' For example the condition, cell type, tissue information
#' The keywords will be used for whole database search
#' @return An object of \link{APPconf} object
#' @importFrom RefManageR GetBibEntryWithDOI GetPubMedByID
#' @export
#' @examples
#' config <- createAppConfig(
#'            title="pbmc_small",
#'            destinationFolder = "pbmc_small",
#'            species = "Homo sapiens",
#'            doi="10.1038/nbt.3192",
#'            datatype = "scRNAseq")
createAppConfig <-
  function(title, destinationFolder,
           species, doi, pmid, bibentry,
           datatype=c("scRNAseq", "scATACseq"),
           markers, keywords){
    ## markers is a list of dataframe, rownames is the gene symbols
    if(!missing(markers)){
      if(is.character(markers)){
        markers <- markers[!is.na(markers)]
        markers <- markers[markers!=""]
        markers <- t(t(markers))
        rownames(markers) <- markers
        markers <- list(markers=as.data.frame(markers))
      }
    }else{
      markers <- list()
    }
    stopifnot(is.list(markers))
    lapply(markers, function(.ele){
      stopifnot("markers must be a list of data.frame" = is.data.frame(.ele))
      stopifnot("markers must be a list of data.frame
                with gene symbols as rownames" =
                  length(rownames(.ele))==nrow(.ele))
    })
    datatype <- match.arg(datatype)
    stopifnot(is.character(title))
    stopifnot(is.character(destinationFolder))
    stopifnot(is.character(species))
    if(!missing(keywords)){
      stopifnot(is.character(keywords))
    } else keywords <- character(0L)
    if(!missing(doi)){
      stopifnot(is.character(doi))
      suppressMessages(bibentry <- GetBibEntryWithDOI(doi))
      if(missing(pmid)) pmid <- idConverter(doi, type="pmid")
    }
    if(!missing(pmid)){
      stopifnot(is.character(pmid))
      suppressMessages(bibentry <- GetPubMedByID(pmid))
      if(missing(doi)) doi <- idConverter(pmid, type="doi")
    }
    bib <- NULL
    if(!missing(bibentry)){
      if(is(bibentry, "bibentry")){
        bib <- format(bibentry, style = 'html')
      }
    }
    return(APPconf(title=title[1],
                   id=destinationFolder[1],
                   species=species[1],
                   ref=list(
                     bib=bib,
                     doi=doi,
                     pmid=pmid,
                     entry=bibentry
                   ),
                   type=datatype,
                   markers = markers,
                   keywords = keywords))
}

#' load data from cellRanger
#' @param outsFolder the outs folder of cellRanger
#' @importFrom Seurat CreateSeuratObject Read10X
#' @importFrom SeuratObject CreateDimReducObject
#' @importFrom utils read.csv
#' @importFrom data.table fread
createSeuFromCellRanger <- function(outsFolder){
  analysisFolder <- file.path(outsFolder, "analysis")
  matrixFolder <- file.path(outsFolder, "filtered_feature_bc_matrix")
  stopifnot("'analysis' folder must exits"=
              file.exists(analysisFolder))
  stopifnot("'filtered_feature_bc_matrix' folder must exits"=
              file.exists(matrixFolder))
  seu <- CreateSeuratObject(Read10X(matrixFolder))
  projections <- dir(analysisFolder, "projection.csv",
                     recursive = TRUE, full.names = TRUE)
  # projections name will be
  # analysisFolder/pca/gene_expression_x_components/projection.csv
  projs <- basename(dirname(dirname(
    sub(analysisFolder, "", projections, fixed=TRUE))))
  for(i in seq_along(projections)){
    projection <- read.csv(projections[i], row.names = 1)
    colnames(projection) <- sub("\\.", "_", colnames(projection))
    seu[[projs[i]]] <- CreateDimReducObject(embeddings=as.matrix(projection),
                                            assay="RNA")
  }
  clusters <- dir(analysisFolder, "clusters.csv",
                  recursive = TRUE, full.names = TRUE)
  clus <- sub("^.*gene_expression_(.*?)\\/clusters.csv", "\\1", clusters)
  for(i in seq_along(clusters)){
    cluster <- read.csv(clusters[i], row.names = 1)
    colnames(cluster) <- clus[i]
    seu[[clus[i]]] <- as.factor(cluster[, 1])
  }
  de_tbls <- dir(analysisFolder, "differential_expression.csv",
                 recursive = TRUE, full.names = TRUE)
  names(de_tbls) <-
    sub("^.*gene_expression_(.*?)\\/differential_expression.csv",
        "\\1", clusters)
  misc <- lapply(de_tbls, read.csv, row.names = 1)
  ## reformat
  misc <- lapply(misc, function(.ele){
    gene <- .ele$Feature.Name
    .ele <- .ele[, -1, drop=FALSE]
    .ele <- lapply(seq.int(ncol(.ele)/3), function(.e){
      .e <- .ele[, (.e-1)*3+seq.int(3)]
      cluster <- sub("Cluster.(.*?).Mean.Counts", "\\1", colnames(.e)[1])
      colnames(.e) <- sub("Cluster.(.*?)\\.", "", colnames(.e))
      cbind(.e, cluster, gene)
    })
    .ele <- do.call(rbind, .ele)
    .ele <- .ele[.ele[, 3]<0.05, , drop=FALSE]
  })
  Misc(seu, "markers") <- misc
  seu
}

#' load data from a count matrix
#' @param matrix count matrix
#' @param meta cell-level meta data
#' @param genes character. gene names, will be the rownames of the matrix
#' @param cluster the cluster coordinates
#' @param ... The parameter passed to read.delim when read cluster file.
#' @importFrom data.table fread
#' @importFrom Seurat CreateSeuratObject
#' @importFrom SeuratObject CreateDimReducObject
cteateSeuFromMatrix <- function(matrix, meta, genes,
                                cluster, ...){
  if(missing(matrix) | missing(meta) ){
    stop('matrix and meta is requred.')
  }
  mat <- fread(matrix)
  meta <- read.delim(meta, header=TRUE)
  if(missing(genes)){
    mat <- mat[!duplicated(mat[[1]]), ]
    genes = mat[[1]]
    mat <- mat[, -1]
  }
  stopifnot(length(genes)==nrow(matrix))
  mat = data.frame(mat, row.names=genes)
  rownames(meta) <- colnames(mat)
  if(identical(colnames(mat), make.names(meta[, 1], unique=TRUE))){
    meta <- meta[, -1]
  }
  getCluster <- function(cluster){
    clusterfile <- read.delim(cluster, ...)
    stopifnot(identical(colnames(mat), make.names(clusterfile$V1, unique=TRUE)))
    clusterfile <- clusterfile[, -1]
    rownames(clusterfile) <- colnames(mat)
    if(all(grepl("^V", colnames(clusterfile)), na.rm = TRUE)){
      colnames(clusterfile)[1:2] <- c("tSNE_1", "tSNE_2")
    }
    clusterfile
  }
  if(is.list(cluster)){
    stopifnot(length(names(cluster))==length(cluster))
    clusters <- lapply(cluster, getCluster)
  }else{
    clusters <- list('tsne'=getCluster(cluster))
    names(clusters) <- lapply(clusters, function(.ele) {
      tolower(gsub("_\\d+$", "", colnames(.ele)[1]))
      })[[1]]
  }

  # create seu
  seu <- CreateSeuratObject(mat, meta.data = meta)
  if(length(clusters)){
    for(i in seq_along(clusters)){
      cluster <- CreateDimReducObject(embeddings=as.matrix(clusters[[i]]))
      seu[[names(clusters)[i]]] <- cluster
    }
  }
  seu
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
#' TODO: need to create a metadata for UI, and real data for plot
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
  cds_x <- c(list(reduction_method=reduction_method, root_nodes=cds_x), cds_x1)
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
#' treatment must be celltype, and contrasts must be injured vs uninjured
