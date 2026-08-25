#' Create a dataset
#' Create a dataset from a Seurat object. The function will try to find
#' the markers in the Misc data named as 'markers'.
#' The misc data should be output of function `FindAllMarkers`.
#' @param datafolder app data folder
#' @param appconf a APPconf object represent the information about the dataset
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
#' @param gexSlot layer in single-cell assay to plot.
#' Default is to use the "data" layer
#' @param atacAssayName assay in single-cell data object to use for plotting
#' open chromatin.
#' @param atacSlot layer in single-cell atac assay to plot.
#' Default is to use the "data" layer
#' @param default.symbol character(1L) specifying the default rownames to be used. If use default, the gene symbols will be the row names of the assay. If one column name of the meta.feature of the assay is supplied, the function will try to extract the symbols from the meta.feature slot of the assay. 
#' @param theme color theme. default is "Paired" from ColorBrewer palettes.
#' @param binSize number of bps for each bin for ATAC fragment coverage. Used
#' to reduce the file size of bigwig.
#' @param normBy Normalization method for the bigwig files. Default `nCell`.
#' `nCells` will divide the number of insertions in a tile by the number of
#' cells in the group. `none` will apply no normalization.
#' The name of a metadata column can also be passed, in which case insertions
#' will be divided by the sum of that column over the cells in the group, with a
#' scaling factor of 10^4 applied.
#' @param fragmentNameMapList list of named character vector. 
#' The name map list must be the same order as the fragment list in the object.
#' For each element of the list,
#' the names of the vector are the name of the fragment and
#' the vector contains the cell names (column names of the assay). 
#' You can try \link{extractFragmentNameMapList}.
#' @param fov Name of FOV (field of view).
#' @param boundaries The container name of segmentation coordinates.
#' @param molecules The container name of molecules coordinates.
#' @importFrom SeuratObject Reductions Idents Assays DefaultAssay GetAssayData `DefaultAssay<-` VariableFeatures Misc `Misc<-` Embeddings `Idents<-` DefaultFOV DefaultBoundary Images Molecules
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
        datafolder = "data",
        default.symbol = 'rownames',
        theme = "Paired",
        binSize = 100,
        normBy = 'nCells',
        fragmentNameMapList,
        fov = NULL,
        boundaries = NULL,
        molecules = NULL) {
    stopifnot(file.exists(datafolder))
    stopifnot(is(seu, "Seurat"))
    stopifnot(is(appconf, "APPconf"))
    gexSlot <- match.arg(gexSlot)
    if (missing(config)) {
        config <- createConfig(seu, theme=theme)
    }
    pf <- file.path(datafolder, appconf$id)
    if(!file.exists(pf)){
        dir.create(pf, recursive = TRUE)
    }else{
        stop(pf, ' already exists')
    }
    if(appconf$type=='spatial'){
        fov <- fov %||% DefaultFOV(seu)
        ## check boundary 
        fov <- Filter(f = function(x) {
            return(x %in% Images(object = seu) &&
                       inherits(x = seu[[x]], what = "FOV"))
        }, x = fov)
        if (!length(fov)) {
            warning("No compatible spatial coordinates present")
        }
        boundaries <- boundaries %||% unlist(lapply(fov, function(x) {
            return(DefaultBoundary(object = seu[[x]]))
        }), use.names = TRUE)
        null <- mapply(function(.fov, .b){
            stopifnot("The 'boundaries' is not a Segmentation or Centroids object"=
                          inherits(seu[[.fov]][[.b]], c('Segmentation',
                                                        'Centroids')))
        }, fov, boundaries)
        molecules <- molecules %||% unlist(lapply(fov, function(x){
            return(Molecules(object = seu[[x]])[1])
        }))
        null <- mapply(function(.fov, .m){
            stopifnot("The 'molecules' is not a Molecules object"=
                          inherits(seu[[.fov]][[.m]], 'Molecules'))
        }, fov, molecules)
    }
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
        if (length(extAssayData(
            seu,
            assay = assayName,
            slot = "scale.data")) == 0) {
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
            markers <- unique(unlist(markers, use.names = FALSE))
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
        default.multigene = markers,
        default.symbol = default.symbol,
        binSize = binSize,
        normBy = normBy,
        fragmentNameMapList = fragmentNameMapList,
        fov = fov,
        boundaries = boundaries,
        molecules = molecules
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
#' @param datatype character(1). Type of the data, scRNAseq, scATACseq,
#'  scMultiome or spatial.
#' @param markers A list of data.frame with gene symbols as rownames or
#'  a character vector.
#' @param keywords The keywords for the dataset.
#' For example the condition, cell type, tissue information
#' The keywords will be used for whole database search
#' @param abstract The abstract of the reference.
#' @param email The request e-mail address to retrieve the doi or pmid.
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
        datatype = c("scRNAseq", "scATACseq", "scMultiome", "spatial"),
        markers,
        keywords,
        abstract,
        email) {
        is_valid <- function(email) {
            grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,5}$", email)
        }
        if(missing(email)){
            email <- .globals$email
        }
        if(!is_valid(email)){
            stop('"email" is request to get the bibentry. ',
                 'Please provide a valid e-mail address')
        }
        ## markers is a list of dataframe, rownames is the gene symbols
        if (!missing(markers)) {
            if (is.character(markers)) {
                markers <- markers[!is.na(markers)]
                markers <- markers[markers != ""]
                markers <- unique(markers)
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
        pmidNotFromDOI <- TRUE
        if (!missing(doi)) {
            stopifnot(is.character(doi))
            if(missing(bibentry) || !is(bibentry, "bibentry")){
                bibentry <- GetBibEntryWithDOI(doi)
            }
            if (missing(pmid)){
                pmid <- idConverter(doi, type = "pmid", email = email)
                pmidNotFromDOI <- FALSE
            }
        }
        if (pmidNotFromDOI & !missing(pmid)) {
            stopifnot(is.character(pmid))
            if(missing(bibentry) || !is(bibentry, "bibentry")){
                bibentry <- GetPubMedByID(pmid)
            }
            if (missing(doi))
                doi <- idConverter(pmid, type = "doi", email = email)
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
                if(!missing(abstract)){
                    bibentry$abstract <- abstract
                }
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

#' Add background image to the dataset
#' DO NOT export in current stage, until there is enough test data
#' @noRd
#' @param tiff The TIFF file name or a data.frame with column names of 'x', 'y',
#' and 'value'.
#' @param fact positive integer. The sub-sample factor. If it set to 10,
#' the tiff will subsample to 1/10 of the original pixels.
#' @param flip switch x, y coordinates for the tiff or not.
#' @param CCR_90 Counter Clockwise Rotation.
#' @param image_width The width of the image.
#' @param clip clip the tiff file by x, y coordinates.
#' @param reduction The reduction name such as 'coor' which should match with
#' the coordinates used in the target reductions.
#' @param alignmentFUN The alignment function to convert the reduction 
#' coordinates to the image coordinates. The name of the first parameter of the
#' function must be plot_data. If it is NULL, the system will use private
#' function 'transformImage' which is designed to convert the coordinates
#' to Stereo-seq tiff image.
#' @param alignmentArgs The arguments of the alignment function.
#' @param datafolder app data folder
#' @param appconf a APPconf object represent the information about the dataset
addBackgroundImage <- function(
        tiff, fact=10,
        flip = TRUE, CCR_90 = TRUE,
        image_width,
        clip = list(x=NULL, y=NULL),
        reduction,
        alignmentFUN, alignmentArgs,
        datafolder='data', appconf){
    stopifnot(file.exists(datafolder))
    stopifnot(is(appconf, "APPconf"))
    if(!missing(alignmentFUN)){
        stopifnot(is.function(alignmentFUN))
        argNames <- methods::formalArgs(alignmentFUN)
        if(!'plot_data' %in% argNames){
            stop("'plot_data' must be an argument in the alignmentFUN")
        }
    }else{
        alignmentFUN=transformImage
        argNames <- methods::formalArgs(alignmentFUN)
    }
    stopifnot(is.list(alignmentArgs))
    if(!all(names(alignmentArgs) %in% argNames)){
        warning('Not all aligmentArgs are arguments in alignmentFUN.')
    }
    if(is.data.frame(tiff)){
        stopifnot(all(c('x', 'y', 'value') %in% colnames(tiff)))
    }else{
        stopifnot(is.integer(fact))
        raster_data <- terra::rast(tiff)
        r_small <- terra::aggregate(raster_data, fact = fact, fun = mean)
        tiff <- as.data.frame(r_small, xy = TRUE)
        colnames(tiff)[3] <- 'value'
    }
    if(isTRUE(flip)){
        if(isTRUE(CCR_90)){
            tmp <- tiff$x
            tiff$x <- tiff$y
            tiff$y <- tmp
        }else{
            tiff$x <- image_width - tiff$x
        }
    }
    if(length(clip$x)==2){
        tiff <- tiff[tiff$x>=clip$x[1] & tiff$x<=clip$x[2], , drop=FALSE]
    }
    if(length(clip$y)==2){
        tiff <- tiff[tiff$y>=clip$y[1] & tiff$y<=clip$y[2], , drop=FALSE]
    }
    backgroundImage <- list()
    scfile <- file.path(datafolder, appconf$id,
                        .globals$filenames$backgroundImage)
    if(file.exists(scfile)){
        backgroundImage <- readRDS(scfile)
    }
    
    backgroundImage[[reduction[1]]] <- list(raster_df=tiff,
                                            FUN=alignmentFUN,
                                            args=alignmentArgs)
    saveRDS(backgroundImage, scfile)
    return(invisible(backgroundImage))
}

#' Add cell borders to the dataset
#' DO NOT export in current stage, until there is enough test data
#' @noRd
#' @param borders The cell border csv file name or 
#' a data.frame with column names of 'x', 'y', 'idx', 'sampleID'.
#' The 'idx' is the order of the points. The sampleID is the cell barcodes.
#' @param cell_coor If the borders is a csv file name, which saved the relative
#' coordinates to the cell center, 'cell_coor' must be provided.
#' The cell_coor should be a csv file name or a data.frame with column names of
#' 'x', 'y'
#' @param reduction The reduction name such as 'coor' which should match with
#' the coordinates used in the target reductions.
#' @param datafolder app data folder
#' @param appconf a APPconf object represent the information about the dataset
#' @param row.names,... The row.names and other parameter used in read.csv.
#' The rownames of the cell border should be the barcodes of the cells.
addCellBorders <- function(
        borders, cell_coor, reduction, datafolder='data', appconf,
        row.names=1, ...){
    stopifnot(file.exists(datafolder))
    stopifnot(is(appconf, "APPconf"))
    if(is.data.frame(borders)){
        stopifnot(all(c('x', 'y', 'idx', 'sampleID') %in% colnames(borders)))
        stopifnot(length(rownames(borders))==nrow(borders))
    }else{
        if(file.exists(borders)){
            borders <- read.csv(borders, row.names = row.names, ...)
            cell_coor <- read.csv(cell_coor, row.names = row.names, ...)
            stopifnot(nrow(borders)==nrow(cell_coor))
            stopifnot(ncol(cell_coor)==2) ## must be 2 columns with x, y pairs
            data <- cbind(cell_coor, borders)
            borders <- apply(data, 1, function(vals){
                vals <- vals[vals != 32767] # Remove sentinels 2^15-1
                vals <- matrix(vals, nrow=2, byrow=FALSE)
                t(vals[, -1] + vals[, 1])
            }, simplify = FALSE)
            l <- unlist(lapply(borders, nrow))
            borders <- do.call(rbind, borders)
            borders <- as.data.frame(borders)
            borders$idx <- unlist(lapply(l, seq.int))
            borders$cell <- rep(rownames(data), l)
            colnames(borders) <- c('x', 'y', 'idx', 'sampleID')
        }else{
            stop('Only data.frame or csv file name are acceptable for borders.')
        }
    }
    out <- list()
    scfile <- file.path(datafolder, appconf$id,
                        .globals$filenames$cellborder)
    if(file.exists(scfile)){
        out <- readRDS(scfile)
    }
    
    out[[reduction[1]]] <- borders
    saveRDS(out, scfile)
    return(invisible(out))
}

#' Add SPRING links to the dataset
#' DO NOT export in current stage, until there is enough test data
#' @noRd
#' @param links The graph data json file name or 
#' a data.frame with column names of 'source', 'target'.
#' @param reduction The reduction name such as 'SPRING' which should match with
#' the coordinates used in the target reductions.
#' @param datafolder app data folder
#' @param appconf a APPconf object represent the information about the dataset
#' @importFrom jsonlite fromJSON
addCellLinks <- function(
        links, reduction, datafolder='data', appconf){
    stopifnot(file.exists(datafolder))
    stopifnot(is(appconf, "APPconf"))
    if(is.data.frame(links)){
        stopifnot(all(c('source', 'target') %in% colnames(links)))
    }else{
        graph_data <- readLines(links)
        graph_data <- fromJSON(paste(graph_data, collapse=''))
        links <- do.call(rbind, graph_data$links)
        if(!all(links$distance==0)){ # TRUE
            warning('Expect all links distance is 0.')
        }
        links <- links[, c('source', 'target')]
        links <- links + 1 ## index change to from 1
    }
    out <- list()
    scfile <- file.path(datafolder, appconf$id,
                        .globals$filenames$sc1edge)
    if(file.exists(scfile)){
        out <- readRDS(scfile)
    }
    
    out[[reduction[1]]] <- links
    saveRDS(out, scfile)
    return(invisible(out))
}

#' add Gene Score matrix
#' @noRd
#' @param obj input single-cell object for Seurat (v3+)
#' @param scConf config data.table
#' @param assayName assay in single-cell data object to use for plotting
#'   gene scores.
#' @param gscoreSlot layer in single-cell gene score assay to plot.
#' Default is to use the "data" layer
#' @param appDir specify directory to create the shiny app in
#' @param chunkSize number of genes written to h5file at any one time. Lower
#'   this number to reduce memory consumption. Should not be less than 10
addGeneScoreMatrix <- function(obj,
                               assayName='GeneScore',
                               gscoreSlot='data',
                               appDir='Data',
                               chunkSize = 500){
    gscoreAsy <- extAssayData(obj, assay = assayName, slot = gscoreSlot)
    gscore.rownm <- rownames(gscoreAsy)
    gscore.matdim <- dim(gscoreAsy)
    # Make XXXgenes.rds
    sc1gene <- seq(gscore.matdim[1])
    names(sc1gene) <- gscore.rownm
    sc1gene <- sc1gene[order(names(sc1gene))]
    sc1gene <- sc1gene[order(nchar(names(sc1gene)))]
    saveRDS(sc1gene, file = file.path(appDir, .globals$filenames$sc1gsgene))
    
    filename <- file.path(appDir, .globals$filenames$sc1gscore)
    if(h5createFile(filename)){
        if(h5createGroup(filename, .globals$h5fGrpPrefix)){
            if(h5createDataset(
                filename,
                dataset = .globals$h5fGrp,
                dims = gscore.matdim,
                maxdims = gscore.matdim,
                H5type = "H5T_NATIVE_FLOAT", #storage.mode(gscoreAsy[1]),
                chunk = c(1, gscore.matdim[2]),
                filter = 'GZIP',
                level = 6)){
                chk <- chunkSize
                while (chk > (gscore.matdim[1] - 8)) {
                    # Account for cases where nGene < chunkSize
                    chk <-
                        floor(chk / 2)
                }
                for (i in seq.int(floor((gscore.matdim[1] - 8) / chk))) {
                    h5write(as.matrix(gscoreAsy[((i - 1) * chk + 1):(i * chk), ]),
                            file = filename,
                            name = .globals$h5fGrp,
                            index = list(((i - 1) * chk + 1):(i * chk), NULL))
                }
                h5write(as.matrix(gscoreAsy[(i * chk + 1):gscore.matdim[1], ]),
                        file = filename,
                        name = .globals$h5fGrp,
                        index = list((i * chk + 1):gscore.matdim[1], NULL))
            }else{
                stop("can not create dataset:", .globals$h5fGrp)
            }
        }else{
            stop("can not create group:", .globals$h5fGrpPrefix)
        }
    }else{
        stop("can not create file:", filename)
    } 
}
