#' Generate data files required for shiny app
#'
#' Copied from ShinyCell to generate the data files.
#' @noRd
#' @param obj input single-cell object for Seurat (v3+)
#' @param scConf config data.table
#' @param assayName assay in single-cell data object to use for plotting
#'   gene expression, which must match one of the following:
#'   \itemize{
#'     \item{Seurat objects}: "RNA" or "integrated" assay,
#'       default is "RNA"
#'   }
#' @param gexSlot layer in single-cell gex assay to plot.
#' Default is to use the "data" layer
#' @param atacAssayName assay in single-cell data object to use for plotting
#' open chromatin.
#' @param atacSlot layer in single-cell atac assay to plot.
#' Default is to use the "data" layer
#' @param appDir specify directory to create the shiny app in
#' @param defaultGene1 specify primary default gene to show
#' @param defaultGene2 specify secondary default gene to show
#' @param default.multigene character vector specifying default genes to
#'   show in bubbleplot / heatmap
#' @param default.dimred character vector specifying the two default dimension
#'   reductions. Default is to use UMAP if not TSNE embeddings
#' @param default.symbol character(1L) specifying the default rownames to be used. If use default, the gene symbols will be the row names of the assay. If one column name of the meta.feature of the assay is supplied, the function will try to extract the symbols from the meta.feature slot of the assay. 
#' @param chunkSize number of genes written to h5file at any one time. Lower
#'   this number to reduce memory consumption. Should not be less than 10
#' @param binSize number of bps for each bin for ATAC fragment coverage. Used
#' to reduce the file size of bigwig.
#' @param fragmentNameMapList list of named character vector. 
#' The name map list must be the same order as the fragment list in the object.
#' For each element of the list,
#' the names of the vector are the name of the fragment and
#' the vector contains the cell names (column names of the assay). 
#' @return data files required for shiny app
#' @importFrom IRanges tile Views viewMeans ranges nearest
#' @importFrom SeuratObject GetAssayData VariableFeatures Embeddings Reductions
#' @importFrom data.table data.table as.data.table
#' @importFrom rhdf5 h5createFile h5createGroup h5createDataset h5write
#' @importFrom Rsamtools TabixFile seqnamesTabix scanTabix
#' @importFrom GenomeInfoDb keepSeqlevels seqinfo seqnames seqlevelsStyle
#' `seqlevelsStyle<-`
#' @importFrom GenomicRanges GRanges width coverage GRangesList
#' @importFrom rtracklayer export
#' @importFrom utils read.table
#' @importFrom fs path_sanitize
makeShinyFiles <- function(
        obj,
        scConf,
        assayName,
        gexSlot = c("data", "scale.data", "counts"),
        atacAssayName,
        atacSlot = c("data", "scale.data", "counts"),
        appDir = "data",
        defaultGene1 = NA,
        defaultGene2 = NA,
        default.multigene = NA,
        default.dimred = NA,
        default.symbol = 'rownames',
        chunkSize = 500,
        binSize = 1,
        fragmentNameMapList) {
    stopifnot(is.numeric(binSize))
    ### Preprocessing and checks
    # Generate defaults for assayName / slot
    stopifnot(is(obj, "Seurat"))
    # Seurat Object
    if (missing(assayName)) {
        assayName <- "RNA"
    } else{
        assayName <- assayName[1]
    }
    gexSlot <- match.arg(gexSlot)
    atacSlot <- match.arg(atacSlot)
    gexAsy <- extAssayData(obj, assay = assayName, slot = gexSlot)
    gex.matdim <- dim(gexAsy)
    gex.rownm <- rownames(gexAsy)
    gex.colnm <- colnames(gexAsy)
    useRownames <- TRUE
    if(default.symbol!='rownames'){
        featureMeta <- obj[[assayName]][[]]
        if(default.symbol %in% colnames(featureMeta)){
            ## rename the gexAsy
            newRN <- featureMeta[, default.symbol[1]]
            newRN[is.na(newRN)] <- gex.rownm[is.na(newRN)]
            newRN[newRN==''] <- gex.rownm[newRN=='']
            if(any(duplicated(newRN))){
                dup_RN <- newRN %in% newRN[duplicated(newRN)]
                newRN[dup_RN] <- 
                    paste(newRN[dup_RN], gex.rownm[dup_RN], sep='-')
            }
            oldRN <- gex.rownm
            gex.rownm <- newRN
            rm(newRN)
            useRownames <- FALSE
        }
    }
    defGenes <- VariableFeatures(obj)[seq.int(10)]
    if (is.na(defGenes[1])) {
        warning(
            "Variable genes for seurat object not found! Have you ",
            "ran `FindVariableFeatures` or `SCTransform`?"
        )
        defGenes <- gex.rownm[seq.int(10)]
    }
    if(!useRownames){
        if(any(oldRN %in% gex.rownm)){
            defGenes <- gex.rownm[match(defGenes, oldRN)]
            defGenes <- defGenes[!is.na(defGenes)]
            if(length(defGenes)==0){
                defGenes <- gex.rownm[seq.int(10)]
            }
        }
    }
    sc1meta <- data.table(sampleID = rownames(obj[[]]), obj[[]])
    
    geneMap <- gex.rownm
    names(geneMap) <- gex.rownm    # Basically no mapping
    
    defGenes <- geneMap[defGenes]
    
    # Check defaultGene1 / defaultGene2 / default.multigene
    defaultGene1 <- defaultGene1[1]
    defaultGene2 <- defaultGene2[1]
    if(!useRownames){
        if(!defaultGene1 %in% gex.rownm){
            defaultGene1 <- gex.rownm[match(defaultGene1, oldRN)]
        }
        if(!defaultGene2 %in% gex.rownm){
            defaultGene2 <- gex.rownm[match(defaultGene2, oldRN)]
        }
        if(any(!default.multigene %in% gex.rownm)){
            default.multigene <- gex.rownm[match(default.multigene, oldRN)]
        }
    }
    if (is.na(defaultGene1)) {
        defaultGene1 <- defGenes[1]
    }
    if (is.na(defaultGene2)) {
        defaultGene2 <- defGenes[2]
    }
    if (is.na(default.multigene[1])) {
        default.multigene <- defGenes
    }
    if (defaultGene1 %in% geneMap) {
        defaultGene1 <- defaultGene1
    } else {
        warning(
            "defaultGene1 doesn't exist in gene expression, using defaults...")
        defaultGene1 <- defGenes[1]
    }
    if (defaultGene2 %in% geneMap) {
        defaultGene2 <- defaultGene2
    } else {
        warning(
            "defaultGene2 doesn't exist in gene expression, using defaults...")
        defaultGene2 <- defGenes[2]
    }
    if (all(default.multigene %in% geneMap)) {
        default.multigene <- default.multigene
    } else {
        warning(
            "default.multigene doesn't exist in gene expression, ",
            "using defaults...")
        default.multigene <- defGenes
    }
    
    ## check for ATAC
    if (!missing(atacAssayName)) {
        if (atacAssayName %in% Assays(obj)) {
            peaks <- obj[[atacAssayName]]
            ## links, the links between peaks and gene symbol,
            ## used to create the matrix table
            links <- peaks@links #GetAssayData(obj, layer = "links")
            ## annotations, used to plot gene model
            annotations <- peaks@annotation #GetAssayData(obj, layer = "annotation")
            if (length(annotations) < 1) {
                stop("scATAC annotation data are not available.")
            }
            if(any(!c('gene_name', 'tx_id') %in% colnames(mcols(annotations)))){
                stop('gene_name and tx_id must exist as metadata for scATAC ',
                     'annotations. Check it by Annotation(obj[["',
                     atacAssayName, '"]])')
            }
            if (length(links) < 1) {
                warning("scATAC links data are not available.")
            }
            regions <- seqinfo(annotations)
            tryCatch({
                regions <- as(regions, "GRanges")
            }, error=function(.e){
                stop("Cannot get genomic informations from annotations.",
                     "Check it by as(seqinfo(Annotation(obj[['",
                     atacAssayName, "']])), 'GRanges')")
            })
            ## get fragments for each cell and group
            fragments <- peaks@fragments #GetAssayData(obj, layer = "fragments")
            if (missing(fragmentNameMapList)){
                fragmentNameMapList <- sc1meta$sampleID
                names(fragmentNameMapList) <- sc1meta$sampleID
                fragmentNameMapList <- rep(list(fragmentNameMapList),
                                           length(fragments))
            }
            stopifnot(is.list(fragmentNameMapList))
            stopifnot(
                'The length of fragmentNameMapList is not same as the fragment'
                      =length(fragmentNameMapList)==length(fragments))
            null <- lapply(seq_along(fragmentNameMapList), function(k){
                if(sum(sc1meta$sampleID  %in% fragmentNameMapList[[k]])==0){
                    stop('fragmentNameMapList[[',
                         k, ']] does not contain the cell names')
                }
            })
            ## check reads name
            for(k in seq_along(fragments)){
                fragment.path <- fragments[[k]]@path
                if(file.exists(fragment.path)){
                    tbx <- open(TabixFile(fragment.path, yieldSize=100))
                    on.exit({close(tbx)})
                    if(length(reads <- scanTabix(tbx)[[1]])){
                        reads <- read.table(text = reads)
                        colnames(reads) <- 
                            c("seqnames", "start", "end", "name", "score")
                        if(length(intersect(
                            reads$name,
                            names(fragmentNameMapList[[k]])))==0){
                            reads_name <- paste(head(reads$name, n=5),
                                                collapse=', ')
                            cells_name <- paste(head(
                                names(fragmentNameMapList[[k]]),
                                                     n=5),
                                                collapse=', ')
                            stop("The fragment ", k,
                                 " names are not same format as",
                                 " the cell names or the map vector.\n",
                                 "The top 5 fragment names: ",
                                 reads_name,
                                 "\n",
                                 "The top 5 cell names: ",
                                 cells_name,
                                 '\nPlease use the parameter ',
                                 'fragmentNameMapList',
                                 ' to map the correct name.')
                        }
                    }
                    close(tbx)
                    on.exit()
                }else{
                    stop('fragments file are missing for ', fragment.path,
                         '. Please check it by ?Fragment and ?UpdatePath')
                }
            }
        }
    }
    
    # save data
    sc1conf <- scConf
    sc1conf$dimred <- FALSE
    sc1meta <-
        sc1meta[, c("sampleID", as.character(sc1conf$ID)), with = FALSE]
    # Factor metadata again
    for (i in as.character(sc1conf[!is.na(sc1conf$fID)]$ID)) {
        sc1meta[[i]] <- factor(
            sc1meta[[i]],
            levels =
                strsplit(sc1conf[sc1conf$ID == i]$fID, "\\|")[[1]])
        levels(sc1meta[[i]]) <-
            strsplit(sc1conf[sc1conf$ID == i]$fUI, "\\|")[[1]]
        sc1conf[sc1conf$ID == i]$fID <- sc1conf[sc1conf$ID == i]$fUI
    }
    # Extract dimred and append to both XXXmeta.rds and XXXconf.rds...
    for (iDR in Reductions(obj)) {
        drMat <- Embeddings(obj[[iDR]])
        if (ncol(drMat) > 5) {
            drMat <- drMat[, seq.int(5)]
        }  # Take first 5 components only
        drMat <- drMat[sc1meta$sampleID,]          # Ensure ordering
        drMat <- as.data.table(drMat)
        sc1meta <- cbind(sc1meta, drMat)
        
        # Update sc1conf accordingly
        tmp <- data.table(
            ID = colnames(drMat),
            UI = colnames(drMat),
            fID = NA,
            fUI = NA,
            fCL = NA,
            fRow = NA,
            default = 0,
            grp = FALSE,
            dimred = TRUE
        )
        tmp$UI <- gsub("_", "", tmp$UI)
        sc1conf <- rbindlist(list(sc1conf, tmp))
    }
    sc1conf$ID <- as.character(sc1conf$ID)     # Remove levels
    
    # Make XXXgexpr.h5
    if (!dir.exists(appDir)) {
        dir.create(appDir, recursive = TRUE)
    }
    
    filename <- file.path(appDir, .globals$filenames$sc1gexpr)
    if(h5createFile(filename)){
        if(h5createGroup(filename, .globals$h5fGrpPrefix)){
            if(h5createDataset(
                filename,
                dataset = .globals$h5fGrp,
                dims = gex.matdim,
                maxdims = gex.matdim,
                H5type = "H5T_NATIVE_FLOAT", #storage.mode(gexAsy[1]),
                chunk = c(1, gex.matdim[2]),
                filter = 'GZIP',
                level = 6)){
                chk <- chunkSize
                while (chk > (gex.matdim[1] - 8)) {
                    # Account for cases where nGene < chunkSize
                    chk <-
                        floor(chk / 2)
                }
                for (i in seq.int(floor((gex.matdim[1] - 8) / chk))) {
                    h5write(as.matrix(gexAsy[((i - 1) * chk + 1):(i * chk), ]),
                            file = filename,
                            name = .globals$h5fGrp,
                            index = list(((i - 1) * chk + 1):(i * chk), NULL))
                }
                h5write(as.matrix(gexAsy[(i * chk + 1):gex.matdim[1], ]),
                        file = filename,
                        name = .globals$h5fGrp,
                        index = list((i * chk + 1):gex.matdim[1], NULL))
            }else{
                stop("can not create dataset:", .globals$h5fGrp)
            }
        }else{
            stop("can not create group:", .globals$h5fGrpPrefix)
        }
    }else{
        stop("can not create file:", filename)
    }
    
    if (!isTRUE(all.equal(sc1meta$sampleID, gex.colnm))) {
        sc1meta$sampleID <- factor(sc1meta$sampleID, levels = gex.colnm)
        sc1meta <- sc1meta[order(sc1meta$sampleID)]
        sc1meta$sampleID <- as.character(sc1meta$sampleID)
    }
    
    # Make XXXgenes.rds
    sc1gene <- seq(gex.matdim[1])
    names(geneMap) <- NULL
    names(sc1gene) <- geneMap
    sc1gene <- sc1gene[order(names(sc1gene))]
    sc1gene <- sc1gene[order(nchar(names(sc1gene)))]
    
    # Make XXXdef.rds (list of defaults)
    if (all(default.dimred %in% sc1conf[sc1conf$dimred == TRUE]$ID)) {
        default.dimred[1] <- sc1conf[sc1conf$ID == default.dimred[1]]$UI
        default.dimred[2] <- sc1conf[sc1conf$ID == default.dimred[2]]$UI
    } else if (all(default.dimred %in% sc1conf[sc1conf$dimred == TRUE]$UI)) {
        default.dimred <- default.dimred    # Nothing happens
    } else {
        warn <- TRUE
        if (is.na(default.dimred[1])) {
            default.dimred <- "umap"
            warn <- FALSE
        }
        # Try to guess... and give a warning
        guess <- gsub("[0-9]", "", default.dimred[1])
        if (length(
            grep(
                guess, sc1conf[sc1conf$dimred == TRUE]$UI,
                ignore.case = TRUE)) >= 2) {
            default.dimred <-
                sc1conf[sc1conf$dimred == TRUE]$UI[
                    grep(
                        guess, sc1conf[sc1conf$dimred == TRUE]$UI,
                        ignore.case = TRUE)[c(1, 2)]]
        } else {
            nDR <- length(sc1conf[sc1conf$dimred == TRUE]$UI)
            default.dimred <-
                sc1conf[sc1conf$dimred == TRUE]$UI[(nDR - 1):nDR]
        }
        if (warn) {
            warning(
                "default.dimred not found, switching to ",
                default.dimred[1],
                " and ",
                default.dimred[1]
            )
        } # Warn if user-supplied default.dimred is not found
    }
    # Note that we stored the display name here
    sc1def <- list()
    sc1def$meta1 <-
        sc1conf[sc1conf$default == 1]$UI   # Use display name
    sc1def$meta2 <-
        sc1conf[sc1conf$default == 2]$UI   # Use display name
    sc1def$gene1 <- defaultGene1              # Actual == Display name
    sc1def$gene2 <- defaultGene2              # Actual == Display name
    sc1def$genes <-
        default.multigene          # Actual == Display name
    sc1def$dimred <- default.dimred            # Use display name
    tmp <- nrow(sc1conf[sc1conf$default != 0 & sc1conf$grp == TRUE])
    if (tmp == 2) {
        sc1def$grp1 <- sc1def$meta1
        sc1def$grp2 <- sc1def$meta2
    } else if (tmp == 1) {
        sc1def$grp1 <-
            sc1conf[sc1conf$default != 0 & sc1conf$grp == TRUE]$UI
        if (nrow(
            sc1conf[sc1conf$default == 0 & sc1conf$grp == TRUE]) == 0) {
            sc1def$grp2 <- sc1def$grp1
        } else {
            sc1def$grp2 <-
                sc1conf[sc1conf$default == 0 & sc1conf$grp == TRUE]$UI[1]
        }
    } else {
        sc1def$grp1 <-
            sc1conf[sc1conf$default == 0 & sc1conf$grp == TRUE]$UI[1]
        if (nrow(
            sc1conf[sc1conf$default == 0 & sc1conf$grp == TRUE]) < 2) {
            sc1def$grp2 <- sc1def$grp1
        } else {
            sc1def$grp2 <-
                sc1conf[sc1conf$default == 0 & sc1conf$grp == TRUE]$UI[2]
        }
    }
    sc1conf <- sc1conf[, -c("fUI", "default"), with = FALSE]
    
    ### Saving objects
    saveRDS(sc1conf, file = file.path(appDir, .globals$filenames$sc1conf))
    saveRDS(sc1meta, file = file.path(appDir, .globals$filenames$sc1meta))
    saveRDS(sc1gene, file = file.path(appDir, .globals$filenames$sc1gene))
    saveRDS(sc1def,  file = file.path(appDir, .globals$filenames$sc1def))
    
    ### save ATAC objects
    if (!missing(atacAssayName)) {
        if (atacAssayName %in% Assays(obj)) {
            rm(gexAsy)
            DefaultAssay(obj) <- atacAssayName
            peaks <- obj[[atacAssayName]]
            ## links, the links between peaks and gene symbol,
            ## used to create the matrix table
            links <- peaks@links #GetAssayData(obj, layer = "links")
            if (length(links) < 1) {
                warning("scATAC links data are not available.")
            }
            ## annotations, used to plot gene model
            annotations <- peaks@annotation #GetAssayData(obj, layer = "annotation")
            if (length(annotations) < 1) {
                stop("scATAC annotation data are not available.")
            }
            if(any(!c('gene_name', 'tx_id') %in% colnames(mcols(annotations)))){
                stop('gene_name and tx_id must exist as metadata for scATAC ',
                     'annotations. Check it by Annotation(obj[["',
                     atacAssayName, '"]])')
            }
            ## get fragments for each cell and group
            fragments <- peaks@fragments #GetAssayData(obj, layer = "fragments")
            regions <- seqinfo(annotations)
            tryCatch({
                regions <- as(regions, "GRanges")
            }, error=function(.e){
                warning("Cannot get genomic informations")
            })
            grp <- sc1conf[sc1conf$grp, ]$ID
            if(is(regions, "GRanges")){
                message("The following steps will cost memories.")
                res <- list()
                for(k in seq_along(fragments)){
                    fragment.path <- fragments[[k]]@path
                    if(file.exists(fragment.path)){
                        tabix.file <- TabixFile(fragment.path)
                        open(con = tabix.file)
                        on.exit(close(tabix.file))
                        region <- regions
                        seq_x <- as.character(seqnames(x = region))
                        seq_y <- seqnamesTabix(file = tabix.file)
                        seq_x_style <- seqlevelsStyle(seq_x)
                        seq_y_style <- seqlevelsStyle(seq_y)
                        if(length(intersect(seq_x_style, seq_y_style))==0){
                            seqlevelsStyle(region)<-seq_y_style[1]
                        }
                        seqnames.in.both <- intersect(
                            x = seqnames(x = region),
                            y = seqnamesTabix(file = tabix.file))
                        region <- keepSeqlevels(
                            x = region,
                            value = seqnames.in.both,
                            pruning.mode = "coarse")
                        message('Creating coverage for ', fragment.path)
                        coverage <- lapply(seq_along(region), function(i){
                            reads <- scanTabix(
                                file = tabix.file,
                                param = region[i])
                            reads <- read.table(text = reads[[1]])
                            colnames(reads) <- 
                                c("seqnames", "start", "end", "name", "score")
                            reads <- GRanges(reads)
                            seqlevelsStyle(reads)<-seq_x_style[1]
                            if(length(intersect(
                                reads$name,
                                names(fragmentNameMapList[[k]])))==0){
                                reads_name <- paste(head(reads$name, n=5),
                                                    collapse=', ')
                                cells_name <- paste(head(
                                    names(fragmentNameMapList[[k]]),
                                                         n=5),
                                                    collapse=', ')
                                stop("The fragment ", k,
                                     " names are not same format as ",
                                     "the cell names or the vector of map for",
                                     ".\nThe top 5 fragment names: ",
                                     reads_name,
                                     "\n",
                                     "The top 5 cell names: ",
                                     cells_name,
                                     '\nPlease use the parameter ',
                                     'fragmentNameMapList',
                                     ' to map the correct name.')
                             }
                            reads.grp <- lapply(grp, function(.grp){
                                lapply(split(
                                    reads,
                                    sc1meta[[.grp]][
                                        match(fragmentNameMapList[[k]][
                                            reads$name],
                                            sc1meta$sampleID
                                              )]),
                                    function(.e){
                                        coverage(.e, weight = .e$score)
                                    })
                            })
                            rm(reads)
                            names(reads.grp) <- grp
                            reads.grp
                        })
                        seqlevelsStyle(region)<-seq_x_style[1]
                        names(coverage) <- as.character(seqnames(region))
                        ## coverage is 3 level list,
                        ## level 1, chromosome
                        ## level 2, group
                        ## level 3, factors in group
                        if(length(coverage)){
                            res[[k]] <- list()
                            for(i in names(coverage[[1]])){
                                res[[k]][[i]] <- list()
                                for(j in names(coverage[[1]][[i]])){
                                    res[[k]][[i]][[j]] <-
                                        Reduce(c, lapply(coverage,
                                                         function(.cvg){
                                            .cvg[[i]][[j]]
                                        }))
                                }
                            }
                        }
                        
                        close(tabix.file)
                        on.exit()
                    }
                }
                if(length(res)>0){
                    message('Exporting coverage to bigwig files.')
                    ## accumulate the signals
                    if(length(res)>1){
                        for(i in seq_along(res)[-1]){
                            N_grp <- names(res[[1]])
                            names(N_grp) <- N_grp
                            res[[1]] <- lapply(N_grp, function(.grp){
                                N_fac <- names(res[[1]][[.grp]])
                                names(N_fac) <- N_fac
                                lapply(N_fac, function(.fac){
                                    if(sum(lengths(
                                        res[[1]][[.grp]][[.fac]]))==0){
                                        return(res[[i]][[.grp]][[.fac]])
                                    }
                                    if(sum(lengths(
                                        res[[i]][[.grp]][[.fac]]))==0){
                                        return(res[[1]][[.grp]][[.fac]])
                                    }
                                    res[[1]][[.grp]][[.fac]] +
                                        res[[i]][[.grp]][[.fac]]
                                })
                            })
                        }
                    }
                    ## normalization
                    res <- lapply(res[[1]], function(.grp) {
                        mapply(
                            .grp,
                            rep(binSize, length(.grp))[seq_along(.grp)],
                            FUN=function(.fac, bs){
                                if(bs>1){
                                    # summarize by bin
                                    .fac_gr <- GRanges(.fac)
                                    wid <- width(.fac_gr)
                                    k <- wid<bs
                                    if(any(k)){
                                        .fac_0 <- .fac_gr[!k]
                                        .fac_1 <- .fac_gr[k]
                                        ## sumarize signal by bin size
                                        .fac_1.rd <- reduce(.fac_1,
                                                            with.revmap=TRUE)
                                        .fac_1_tile <- tile(.fac_1.rd, width=bs)
                                        .fac_1 <- unlist(.fac_1_tile)
                                        .fac_0 <- c(.fac_0, .fac_1)
                                        .fac_gr <- split(.fac_0,
                                                         seqnames(.fac_0))
                                        .fac <- 
                                            mapply(
                                                .fac, .fac_gr[names(.fac)],
                                                FUN=function(.rle, .gr){
                                                    .ir <- ranges(.gr)
                                                    .gr$score <-
                                                        viewMeans(Views(.rle,
                                                                        .ir),
                                                                  na.rm=TRUE)
                                                    .gr
                                                })
                                        rm(.fac_0, .fac_1,
                                           .fac_1.rd, .fac_1_tile, .fac_gr)
                                        .fac <- unlist(GRangesList(.fac))
                                    }else{
                                        .fac <- .fac_gr
                                    }
                                }else{
                                    .fac <- GRanges(.fac)
                                }
                                .fac <- .fac[.fac$score!=0]
                                ## normalize by FPKM
                                .s <- .fac$score * width(.fac)/1e3
                                .fac$score <- 1e6*.fac$score/sum(.s)
                                .fac
                            }, SIMPLIFY = FALSE)
                    })
                    ## export
                    mapply(res, names(res), FUN=function(.grp, .grpname){
                        .grp <- .grp[lengths(.grp)>0]
                        mapply(.grp, names(.grp), FUN=function(.fac, .facname){
                            .facname <- path_sanitize(.facname)
                            pf <- file.path(
                                appDir, .globals$filenames$bwspath, .grpname)
                            dir.create(pf,
                                recursive = TRUE, showWarnings=FALSE)
                            export(.fac, file.path(
                                pf,
                                paste0(.facname, ".bigwig")),
                                format = "BigWig")
                        })
                    })
                }
            }
            # asy used to create coverage files,
            # Note this is different from fragment signals
            # it just show the counts in each called peaks
            acAsy <- extAssayData(obj, assay = atacAssayName, slot = atacSlot)
            acAsy <- acAsy[, sc1meta$sampleID]
            peaks <- do.call(rbind, strsplit(rownames(acAsy), "-"))
            peaks <- as.data.frame(peaks)
            if(ncol(peaks)==3){
                writeATACdata(acAsy, appDir)
                colnames(peaks) <- c("seqnames", "start", "end")
                mode(peaks[, 2]) <- "numeric"
                mode(peaks[, 3]) <- "numeric"
                saveRDS(peaks, file = file.path(
                    appDir, .globals$filenames$sc1peak
                ))
                if(length(links)<1){
                    message(" Linking peak by nearest TSS.")
                    links <- GRanges(peaks)
                    n <- nearest(links, annotations)
                    links$gene <- annotations$gene_name[n]
                }
            }else{
                warning("Ask rownames of ATAC assays format is chr-start-end!")
            }
            saveRDS(links, file = file.path(
                appDir, .globals$filenames$sc1link
            ))
            saveRDS(annotations, file = file.path(
                appDir, .globals$filenames$sc1anno
            ))
        }
    }
    return(sc1conf)
}
