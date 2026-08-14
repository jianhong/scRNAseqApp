## Per-fragment, per-region processing 
## Each region is scanned independently and saved to disk immediately,
## so only one region's reads are in RAM at a time.
#' @importFrom data.table fread
save_tmp_index_for_one_fragments <- function(
        fragment.path, regions, nameMap, sc1meta, grp, tmp_base, k) {
    if (!file.exists(fragment.path)) return(NULL)
    
    tabix.file <- TabixFile(fragment.path)
    open(con = tabix.file)
    on.exit(close(tabix.file), add = TRUE)
    
    ## Seq-style reconciliation once per fragment
    region <- regions
    seq_x <- as.character(seqnames(x = region))
    seq_y <- seqnamesTabix(file = tabix.file)
    seq_x_style <- seqlevelsStyle(seq_x)
    seq_y_style <- seqlevelsStyle(seq_y)
    if(length(intersect(seq_x_style, seq_y_style))==0){
        seqlevelsStyle(region)<-seq_y_style[1]
    }
    
    region <- keepSeqlevels(
        region,
        value = intersect(seqnames(region), seq_y),
        pruning.mode = "coarse")
    
    seqlevelsStyle(region) <- seq_x_style[1L]
    
    message('Creating coverage for ', fragment.path)
    
    ## Scan one region at a time
    tmp_index <- list()  # group -> factor -> list of tmp paths (one per region)
    
    for (i in seq_along(region)) {
        
        chr_name <- as.character(seqnames(region[i]))
        
        txt <- tryCatch(
            scanTabix(file = tabix.file, param = region[i])[[1L]],
            error = function(e) NULL)
        
        if (is.null(txt) || length(txt) == 0L) next
        
        col.names <- getColNames(head(txt, n5))
        reads <- fread(
            text = txt,
            sep = "\t",
            col.names = col.names,
            showProgress = FALSE)
        rm(txt)
        
        reads <- GRanges(reads)
        
        tryCatch(
            seqlevelsStyle(reads) <- seq_x_style[1L],
            error = function(e) message(
                e, '\nCannot convert seqstyle to ',
                seq_x_style[1L], ' for ', chr_name))
        
        mapped_ids <- nameMap[reads$name]
        
        ## Compute coverage per (group, factor) and save each to disk
        for (g in grp) {
            cell_labels <- sc1meta[[g]][match(mapped_ids, sc1meta$sampleID)]
            cov_by_fac  <- lapply(
                split(reads, cell_labels),
                function(.e) coverage(.e, weight = .e$score))
            
            for (f in names(cov_by_fac)) {
                tmp_path <- file.path(
                    tmp_base,
                    paste0("tmp_k", k, "_r", i, "_", g,
                           "_", path_sanitize(f), ".rds"))
                saveRDS(cov_by_fac[[f]], tmp_path)
                
                ## Build index: tmp_index[[g]][[f]] is a list of per-region paths
                if (is.null(tmp_index[[g]])) tmp_index[[g]] <- list()
                if (is.null(tmp_index[[g]][[f]])) tmp_index[[g]][[f]] <- list()
                tmp_index[[g]][[f]] <- c(tmp_index[[g]][[f]], tmp_path)
            }
            rm(cov_by_fac)
        }
        rm(reads)
    }
    tmp_index
}

exportGroupBW <- function(
        appDir, fragments, fragmentNameMapList, grp,
        regions, sc1meta, normBy, binSize){
    tmp_base <- file.path(appDir, .globals$filenames$bwspath, "tmp")
    dir.create(tmp_base, recursive = TRUE, showWarnings = FALSE)
    
    all_tmp_index <- lapply(seq_along(fragments), function(k){
        fragment.path <- fragments[[k]]@path
        if (!file.exists(fragment.path)) return(NULL)
        
        nameMap <- fragmentNameMapList[[k]]
        save_tmp_index_for_one_fragments(
            fragment.path, regions, nameMap, sc1meta, grp, tmp_base, k)
    })
    
    ## Drop NULLs (missing fragment files)
    all_tmp_index <- Filter(Negate(is.null), all_tmp_index)
    if (length(all_tmp_index) == 0L){
        stop("No coverage computed. Check fragment paths.")
    }
    
    ## ── Accumulate across regions then fragments, entirely on disk ───────────
    ## For each (group, factor): sum across all regions of all fragments,
    ## reading and deleting one tmp file at a time.
    message('Accumulating signals across regions and fragments.')
    
    ## Collect all (group, factor) keys across all fragments
    all_grp_names <- unique(unlist(lapply(all_tmp_index, names)))
    combined_index <- list()
    
    for (g in all_grp_names) {
        all_fac_names <- unique(unlist(lapply(all_tmp_index,
                                              function(idx) names(idx[[g]]))))
        
        for (f in all_fac_names) {
            ## Gather every tmp path for this (g, f) across all fragments and regions
            all_paths <- unlist(lapply(all_tmp_index,
                                       function(idx) idx[[g]][[f]]),
                                use.names = FALSE)
            
            ## Accumulate one file at a time: read -> sum -> write back -> delete
            acc_path <- all_paths[[1L]]
            acc      <- readRDS(acc_path)
            
            for (p in all_paths[-1L]) {
                nxt <- readRDS(p)
                acc <- c(acc, nxt) ## each region is one chromosome, combin is OK
                rm(nxt)
                unlink(p)   # delete tmp file as soon as it's merged
            }
            
            saveRDS(acc, acc_path)
            rm(acc)
            
            if (is.null(combined_index[[g]])) combined_index[[g]] <- list()
            combined_index[[g]][[f]] <- acc_path
        }
    }
    rm(all_tmp_index)
    
    ## Binning + normalisation + export -> one (group, factor) at a time
    message('Bin-averaging, normalising, and exporting ATAC singals.')
    if (binSize > 1L) {
        bins  <- tileGenome(seqlengths(regions), tilewidth = binSize,
                            cut.last.tile.in.chrom = TRUE)
        zeros <- coverage(regions, weight = 0)
    }
    
    for (g in names(combined_index)) {
        if (normBy == 'nCells') {
            norm_denom <- table(sc1meta[[g]])
        } else if (normBy %in% colnames(sc1meta)) {
            cellGroupi <- sc1meta[, c(normBy, g), with = FALSE]
            norm_denom <- vapply(
                split(cellGroupi[[1L]], cellGroupi[[2L]]),
                sum, FUN.VALUE = numeric(1L), na.rm = TRUE)
        }
        
        for (f in names(combined_index[[g]])) {
            
            tmp_path <- combined_index[[g]][[f]]
            .cov     <- readRDS(tmp_path)
            
            if (binSize > 1L) {
                .cov <- binAverage(bins, .cov)
            }
            
            if (normBy == 'nCells') {
                fv <- norm_denom[f]
                if (!is.na(fv) && fv > 0) .cov$score <- .cov$score / fv
            } else if (normBy %in% colnames(sc1meta)) {
                fv <- norm_denom[f]
                if (!is.na(fv) && fv > 0) .cov$score <- .cov$score * 1e4 / fv
            }
            
            .cov <- .cov[.cov$score > 0]
            
            if (length(.cov) > 0L) {
                pf <- file.path(appDir, .globals$filenames$bwspath, g)
                dir.create(pf, recursive = TRUE, showWarnings = FALSE)
                export(.cov,
                       file.path(pf, paste0(path_sanitize(f), ".bigwig")),
                       format = "BigWig")
            }
            
            unlink(tmp_path)   # delete tmp file once bigwig is written
            rm(.cov)
        }
    }
    unlink(tmp_base, recursive = TRUE)
}

getColNames <- function(txt, sep='\t'){
    col5 <- c("seqnames", "start", "end", "name", "score")
    col6 <- c("seqnames", "start", "end", "name", "score", "strand")
    reads <- fread(
        text = txt,
        sep = "\t",
        showProgress = FALSE)
    if(ncol(reads)==5){
        return(col5)
    }else{
        return(col6)
    }
}

binAverage <- function(bins, .cov){
    ## do not use binnedAverage
    ## reason: if the fragment not cover the whole region,
    ##       the right side 0's will be trimmed and the mean values are not
    ##.      accurate.
    ##       and all the bins here are well ordered and timmed.
    ## all tile are trimmed and sorted
    chrs_with_signal <- intersect(seqlevels(bins), names(.cov))
    bins_chr <- keepSeqlevels(
        bins,
        value = chrs_with_signal,
        pruning.mode = "coarse")
    .cov <- .cov[chrs_with_signal]
    gr <- as(.cov, 'GRanges')
    gr <- gr[gr$score>0]
    seqinfo(gr) <- seqinfo(bins_chr)
    bins_chr <- subsetByOverlaps(bins_chr, gr)
    v <- Views(.cov, bins_chr)
    means <- viewMeans(v, na.rm=TRUE)
    bins_chr$score <- unsplit(means, as.factor(seqnames(bins_chr)))
    bins_chr
}