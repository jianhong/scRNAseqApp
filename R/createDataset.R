#' Create a dataset
#' Create a dataset from a Seurat object. The function will try to find
#' the markers in the Misc data named as 'markers'.
#' The misc data should be output of function `FindAllMarkers`.
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
#' @param atacAssayName assay in single-cell data object to use for plotting
#' open chromatin.
#' @param atacSlot slot in single-cell atac assay to plot.
#' Default is to use the "data" slot
#' @importFrom SeuratObject Reductions Idents Assays DefaultAssay GetAssayData
#'  `DefaultAssay<-` VariableFeatures Misc `Misc<-` Embeddings `Idents<-`
#' @importFrom Seurat FindAllMarkers FindVariableFeatures ScaleData
#' @return The updated Seurat object.
#' @export
#' @examples
#' library(Seurat)
#' if(interactive()){
#'     appconf <- createAppConfig(
#'         title="pbmc_small",
#'         destinationFolder = "pbmc_small",
#'         species = "Homo sapiens",
#'         doi="10.1038/nbt.3192",
#'         datatype = "scRNAseq")
#'     createDataSet(appconf, pbmc_small, datafolder=tempdir())
#' }
createDataSet <- function(
        appconf,
        seu,
        config,
        contrast,
        assayName,
        gexSlot = c("data", "scale.data", "counts"),
        atacAssayName,
        atacSlot = c("data", "scale.data", "counts"),
        LOCKER = FALSE,
        datafolder = "data") {
    stopifnot(file.exists(datafolder))
    stopifnot(is(seu, "Seurat"))
    stopifnot(is(appconf, "APPconf"))
    gexSlot <- match.arg(gexSlot)
    if (missing(config)) {
        config <- createConfig(seu)
    }
    pf <- file.path(datafolder, appconf$id)
    dir.create(pf, recursive = TRUE)
    ## markers
    markers <- appconf$markers
    if(length(markers)==0){
        if (!is.null(Misc(seu, "markers"))) {
            ## the markers is available at Misc(seu, "markers") slot
            markers <- Misc(seu, "markers")
        }
    }
    if(length(markers)==0){
        cellInfo <- colnames(seu[[]])
        assays <- Assays(seu)
        stopifnot(
            "Please input a seurat object with 'SCT' or 'RNA' assay" =
                any(c("SCT", "RNA") %in% assays))
        if (!DefaultAssay(seu) %in% c("SCT", "RNA")) {
            DefaultAssay(seu) <- match.arg(
                assays,
                choices = c("SCT", "RNA"),
                several.ok = TRUE)[1]
        }
        if (missing(assayName)) {
            assayName <- DefaultAssay(seu)
        }
        assayName <- assayName[1]
        stopifnot(
            "The assayName is not in input object" = assayName %in% assays)
        if (length(GetAssayData(seu, "scale.data")) == 0) {
            seu <- FindVariableFeatures(
                seu,
                selection.method = "vst",
                nfeatures = 1000)
            seu <- ScaleData(seu)
        }
        top10 <- head(VariableFeatures(seu), 10)
        if (!missing(contrast)) {
            if (contrast[1] %in% cellInfo) {
                appconf$groupCol <- contrast[1]
            } else{
                stop("The input contrast is not in seu object")
            }
        } else{
            if (!is.null(appconf$groupCol)) {
                if (appconf$groupCol[1] %in% cellInfo) {
                    contrast <- appconf$groupCol[1]
                } else{
                    appconf$groupCol <- appconf$groupCol[-1]
                }
            } else{
                contrast <- NULL
            }
        }
        if (length(markers) == 0) {
            if (!is.factor(Idents(seu))) {
                if (!is.null(contrast)) {
                    Idents(seu) <- contrast
                } else{
                    grp <- cellInfo[grepl(
                        'cluster|cell(.*)type',
                        cellInfo,
                        ignore.case = TRUE)]
                    grp_d <- adist('celltype', grp)
                    Idents(seu) <- grp[which.min(grp_d)][1]
                }
            }
            markers <- FindAllMarkers(
                seu,
                only.pos = TRUE,
                min.pct = .25,
                logfc.threshold = .25
            )
            if (length(markers)) {
                Misc(seu, "markers") <- markers
            }
            if(length(markers$cluster)>0){
                appconf$markers <- split(markers, markers$cluster)
            }
        }
        if (length(markers) == 0) {
            markers <- top10
        } else{
            if (is.list(markers) && !is.data.frame(markers)) {
                markers <- as.data.frame(markers[[1]])
            }
            markers <- split(markers, markers$cluster)
            markers <-
                lapply(markers, head, n = min(5, ceiling(50 / length(markers))))
        }
    }
    if(!is.character(markers)){
        if(is.list(markers)){
            if(is.data.frame(markers)){
                markers <- list(markers)
            }
            ## list of data.frame
            markers <- lapply(markers, function(.ele){
                if(is.data.frame(.ele)){
                    cn <- grepl(
                        "^(gene|symbol)",
                        colnames(.ele),
                        ignore.case = TRUE)
                    if(any(cn)){
                        cn <- which(cn)[1]
                        return(as.character(.ele[, cn, drop=TRUE]))
                    }else{
                        return(rownames(.ele))
                    }
                }else{
                    if(is.character(.ele)){
                        return(.ele)
                    }else{
                        return(NULL)
                    }
                }
            })
            markers <- unique(unlist(markers))
        }
    }
    if(length(markers)==0 || !is.character(markers)){
        stop("Can not locate the markers for the inputs.")
    }
    
    ## make shiny app
    makeShinyFiles(
        seu,
        scConf = config,
        assayName = assayName,
        gexSlot = gexSlot,
        atacAssayName = atacAssayName,
        atacSlot = atacSlot,
        appDir = pf,
        defaultGene1 = markers[1],
        defaultGene2 = markers[2],
        default.multigene = markers
    )
    
    .globals$datafolder <- datafolder
    saveAppConf(appconf)
    ## save misc data
    for (slot in names(Misc(seu))) {
        writeMisc(Misc(seu, slot), appconf$id, slot)
    }
    ## "Locker"
    if (LOCKER) {
        writeLines("", file.path(pf, "LOCKER"))
    }
    return(seu)
}


#' Create a metadata to describe the dataset
#' @description The function will return a APPconf object which contain
#' the reference, keywords for the dataset.
#' @param title The title of the dataset
#' @param destinationFolder The destination folder name of the dataset without
#'  the root folder of the datasets. The data will be saved as
#'  `appdataFolder/destinationFolder`
#' @param species The species of the dataset
#' @param doi,pmid The DOI or PMID of the reference
#' @param bibentry An object of bibentry
#' @param datatype character(1). Type of the data, scRNAseq, scATACseq or
#'  scMultiome.
#' @param markers A list of data.frame with gene symbols as rownames or
#'  a character vector.
#' @param keywords The keywords for the dataset.
#' For example the condition, cell type, tissue information
#' The keywords will be used for whole database search
#' @return An object of \link{APPconf} object
#' @importFrom RefManageR GetBibEntryWithDOI GetPubMedByID
#' @export
#' @examples
#' if(interactive()){
#'     config <- createAppConfig(
#'         title="pbmc_small",
#'         destinationFolder = "pbmc_small",
#'         species = "Homo sapiens",
#'         doi="10.1038/nbt.3192",
#'         datatype = "scRNAseq")
#' }

createAppConfig <-
    function(
        title,
        destinationFolder,
        species,
        doi,
        pmid,
        bibentry,
        datatype = c("scRNAseq", "scATACseq", "scMultiome"),
        markers,
        keywords) {
        ## markers is a list of dataframe, rownames is the gene symbols
        if (!missing(markers)) {
            if (is.character(markers)) {
                markers <- markers[!is.na(markers)]
                markers <- markers[markers != ""]
                markers <- t(t(markers))
                rownames(markers) <- markers
                markers <- list(markers = as.data.frame(markers))
            }
        } else{
            markers <- list()
        }
        stopifnot(is.list(markers))
        lapply(markers, function(.ele) {
            stopifnot(
                "markers must be a list of data.frame" =
                    is.data.frame(.ele))
            stopifnot(
                "markers must be a list of data.frame
                with gene symbols as rownames" =
                    length(rownames(.ele)) == nrow(.ele)
            )
        })
        datatype <- match.arg(datatype)
        stopifnot(is.character(title))
        stopifnot(is.character(destinationFolder))
        stopifnot(is.character(species))
        if (!missing(keywords)) {
            stopifnot(is.character(keywords))
        } else
            keywords <- character(0L)
        if (!missing(doi)) {
            stopifnot(is.character(doi))
            bibentry <- GetBibEntryWithDOI(doi)
            if (missing(pmid))
                pmid <- idConverter(doi, type = "pmid")
        }
        if (!missing(pmid)) {
            stopifnot(is.character(pmid))
            bibentry <- GetPubMedByID(pmid)
            if (missing(doi))
                doi <- idConverter(pmid, type = "doi")
        }
        if(missing(doi)){
            doi = ""
        }
        if(missing(pmid)){
            pmid = ""
        }
        bib <- NULL
        if (!missing(bibentry)) {
            if (is(bibentry, "bibentry")) {
                bib <- format(bibentry, style = 'html')
            }
        }else{
            bibentry <- NULL
        }
        return(
            APPconf(
                title = title[1],
                id = destinationFolder[1],
                species = species[1],
                ref = list(
                    bib = bib,
                    doi = doi,
                    pmid = pmid,
                    entry = bibentry
                ),
                type = datatype,
                markers = markers,
                keywords = keywords
            )
        )
    }

#' load data from cellRanger
#' @param outsFolder the outs folder of cellRanger
#' @importFrom Seurat CreateSeuratObject Read10X
#' @importFrom SeuratObject CreateDimReducObject
#' @importFrom utils read.csv
#' @importFrom data.table fread
#' @return An SeuratObject
createSeuFromCellRanger <- function(outsFolder) {
    analysisFolder <- file.path(outsFolder, "analysis")
    matrixFolder <-
        file.path(outsFolder, "filtered_feature_bc_matrix")
    stopifnot(
        "'analysis' folder must exits" =
            file.exists(analysisFolder))
    stopifnot(
        "'filtered_feature_bc_matrix' folder must exits" =
            file.exists(matrixFolder))
    seu <- CreateSeuratObject(Read10X(matrixFolder))
    projections <- dir(
        analysisFolder,
        "projection.csv",
        recursive = TRUE,
        full.names = TRUE)
    # projections name will be
    # analysisFolder/pca/gene_expression_x_components/projection.csv
    projs <- basename(dirname(dirname(
        sub(analysisFolder, "", projections, fixed = TRUE)
    )))
    for (i in seq_along(projections)) {
        projection <- read.csv(projections[i], row.names = 1)
        colnames(projection) <- sub("\\.", "_", colnames(projection))
        seu[[projs[i]]] <-
            CreateDimReducObject(
                embeddings = as.matrix(projection),
                assay = "RNA")
    }
    clusters <- dir(
        analysisFolder,
        "clusters.csv",
        recursive = TRUE,
        full.names = TRUE)
    clus <-
        sub("^.*gene_expression_(.*?)\\/clusters.csv",
            "\\1",
            clusters)
    for (i in seq_along(clusters)) {
        cluster <- read.csv(clusters[i], row.names = 1)
        colnames(cluster) <- clus[i]
        seu[[clus[i]]] <- as.factor(cluster[, 1])
    }
    de_tbls <- dir(
        analysisFolder,
        "differential_expression.csv",
        recursive = TRUE,
        full.names = TRUE
    )
    names(de_tbls) <- sub(
        "^.*gene_expression_(.*?)\\/differential_expression.csv",
        "\\1",
        clusters)
    misc <- lapply(de_tbls, read.csv, row.names = 1)
    ## reformat
    misc <- lapply(misc, function(.ele) {
        gene <- .ele$Feature.Name
        .ele <- .ele[, -1, drop = FALSE]
        .ele <- lapply(seq.int(ncol(.ele) / 3), function(.e) {
            .e <- .ele[, (.e - 1) * 3 + seq.int(3)]
            cluster <- sub(
                "Cluster.(.*?).Mean.Counts",
                "\\1",
                colnames(.e)[1])
            colnames(.e) <- sub("Cluster.(.*?)\\.", "", colnames(.e))
            cbind(.e, cluster, gene)
        })
        .ele <- do.call(rbind, .ele)
        .ele <- .ele[.ele[, 3] < 0.05, , drop = FALSE]
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
#' @return An SeuratObject
createSeuFromMatrix <- function(
        matrix, meta, genes, cluster, ...) {
    if (missing(matrix) | missing(meta)) {
        stop('matrix and meta is requred.')
    }
    mat <- fread(matrix)
    meta <- read.delim(meta, header = TRUE)
    if (missing(genes)) {
        mat <- mat[!duplicated(mat[[1]]),]
        genes <- mat[[1]]
        mat <- mat[, -1]
    }
    stopifnot(length(genes) == nrow(matrix))
    mat <- data.frame(mat, row.names = genes)
    rownames(meta) <- colnames(mat)
    if (identical(colnames(mat), make.names(meta[, 1], unique = TRUE))) {
        meta <- meta[, -1]
    }
    getCluster <- function(cluster) {
        clusterfile <- read.delim(cluster, ...)
        stopifnot(identical(
            colnames(mat),
            make.names(clusterfile$V1, unique = TRUE)
        ))
        clusterfile <- clusterfile[, -1]
        rownames(clusterfile) <- colnames(mat)
        if (all(grepl("^V", colnames(clusterfile)), na.rm = TRUE)) {
            colnames(clusterfile)[c(1, 2)] <- c("tSNE_1", "tSNE_2")
        }
        clusterfile
    }
    if (is.list(cluster)) {
        stopifnot(length(names(cluster)) == length(cluster))
        clusters <- lapply(cluster, getCluster)
    } else{
        clusters <- list('tsne' = getCluster(cluster))
        names(clusters) <- lapply(clusters, function(.ele) {
            tolower(gsub("_\\d+$", "", colnames(.ele)[1]))
        })[[1]]
    }
    
    # create seu
    seu <- CreateSeuratObject(mat, meta.data = meta)
    if (length(clusters)) {
        for (i in seq_along(clusters)) {
            cluster <-
                CreateDimReducObject(embeddings = as.matrix(clusters[[i]]))
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
addSlingshot <- function(dimred, clusterings) {
    stopifnot(length(names(clusterings)) > 0)
    lapply(clusterings, function(clustering) {
        slingshot::getLineages(data = dimred, clusterLabels = clustering)
    })
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
addTricycle <- function(exp, gname.type, species, meta) {
    species <- match.arg(species, choices = c("human", "mouse"))
    exp <- tricycle::project_cycle_space(
        exp,
        gname.type = gname.type,
        species = species)
    exp <- tricycle::estimate_cycle_position(exp)
    exp <- tricycle::estimate_Schwabe_stage(
        exp,
        gname.type = gname.type,
        species = species)
    stopifnot(identical(
        rownames(meta),
        rownames(SummarizedExperiment::colData(exp))
    ))
    tricyclePosition <-
        SummarizedExperiment::colData(exp)$tricyclePosition
    CCStage <- SummarizedExperiment::colData(exp)$CCStage
    return(list(
        tricyclePosition = tricyclePosition,
        CCStage = CCStage))
}
