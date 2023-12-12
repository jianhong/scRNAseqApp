readData <- function(slot, folder) {
    fs <- file.path(.globals$datafolder, folder, .globals$filenames[[slot]])
    if(file.exists(fs)){
        return(readRDS(fs))
    }
    return(NULL)
}
loadData <- function(dataSource) {
    for (i in c("sc1conf", "sc1def", "sc1gene", "sc1meta")) {
        dataSource[[i]] <- readData(i, dataSource$dataset)
    }
    return(dataSource)
}

saveAppConf <- function(appconf) {
    pf <- file.path(.globals$datafolder, appconf$id)
    if (!file.exists(pf))
        dir.create(pf, recursive = TRUE, showWarnings = FALSE)
    for (i in c("markers", "keywords")) {
        appconf[[i]] <- appconf[[i]][!is.na(appconf[[i]])]
        appconf[[i]] <- appconf[[i]][appconf[[i]] != ""]
    }
    saveData(appconf, appconf$id, "appconf")
}

saveData <- function(data, folder, prefix) {
    filename <- .globals$filenames[[prefix]]
    if (is.null(filename))
        filename <- paste0(prefix, ".rds")
    saveRDS(data,
            file.path(.globals$datafolder, folder, filename))
}

setLocker <- function(folder) {
    writeLines(
        character(0),
        file.path(.globals$datafolder, folder, .globals$filenames$locker)
    )
    updateLocker(folder, TRUE)
}
removeLocker <- function(folder) {
    unlink(file.path(.globals$datafolder, folder, .globals$filenames$locker))
    updateLocker(folder, FALSE)
}
writeMisc <- function(misc, folder, slot) {
    if (!is.null(misc)) {
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
read_exprs <- function(
        h5f,
        genesID,
        meta,
        config,
        groupName,
        splitName,
        valueOnly = FALSE,
        cell) {
    fs <- file.path(
        .globals$datafolder,
        h5f,
        .globals$filenames$sc1gexpr)
    if(!file.exists(fs)){
        stop("No expression data available. Data may be removed.")
    }
    h5file <- H5File$new(
        fs,
        mode = "r")
    on.exit(h5file$close_all())
    h5data <- h5file[["grp"]][["data"]]
    if (valueOnly) {
        if (!missing(cell)) {
            expr <- h5data$read(args = list(quote(expr = ), cell[1]))
        } else{
            if (!is.na(genesID)) {
                expr <- h5data$read(args = list(genesID[1], quote(expr = )))
            } else{
                expr <- 0
            }
        }
        h5file$close_all()
        on.exit()
        return(expr)
    }
    exprs <- data.table()
    for (idx in seq_along(genesID)) {
        tmp <- meta[, c("sampleID",
                        config[config$grp == TRUE]$ID),
                    with = FALSE]
        if (!missing(groupName)) {
            tmp$grpBy <- meta[[config[config$UI == groupName]$ID]]
        }
        if (!missing(splitName)) {
            if(!is.na(splitName)) {
                if(splitName %in% config$UI){
                    tmp$splitBy <- meta[[config[config$UI == splitName]$ID]]
                }
            }
        }
        tmp$geneName <- names(genesID)[idx]
        tmp$val <- h5data$read(args = list(genesID[idx], quote(expr = )))
        exprs <- rbindlist(list(exprs, tmp))
    }
    h5file$close_all()
    on.exit()
    exprs
}

#' read ATAC counts in peaks
#' The data was write as
#' /cell/cell-name/matrix
#' matrix is sparse matrix, the first column is the index number start from 1
#' of the peak; the second column is the count number.
#' @noRd
#' @importFrom hdf5r H5File h5types H5S
writeATACdata <- function(acAsy, appDir){
    filename <- file.path(appDir, .globals$filenames$sc1atac)
    sc1atac <- H5File$new(filename, mode = "w")
    sc1atac.cell <- sc1atac$create_group("cell")
    lapply(colnames(acAsy), function(j){## time consuming
        x <- acAsy[, j]
        i <- which(x!=0)
        N <- length(i)
        if(N>0){
            x <- matrix(c(i, x[i]), nrow = N)
            type <- if(all(x==round(x)))  h5types$H5T_NATIVE_INT else
                h5types$H5T_NATIVE_DOUBLE
            sc1atac.cell.data <- sc1atac.cell$create_dataset(
                j,
                dtype = type,
                space = H5S$new("simple", dims = dim(x), maxdims = dim(x)),
                dim = dim(x)
            )
            sc1atac.cell.data[, ] <- x 
        }
    })
    sc1atac$close_all()
}
#' @importFrom hdf5r H5File
readATACdata <- function(h5f, cell){
    stopifnot(is.character(cell))
    stopifnot(length(cell)==1)
    
    h5file <- H5File$new(
        file.path(
            .globals$datafolder,
            h5f,
            .globals$filenames$sc1atac),
        mode = "r")
    on.exit(h5file$close_all())
    if(cell %in% names(h5file[["cell"]])){
        h5data <- h5file[["cell"]][[cell]]
        cnts <- h5data$read()
    }else{
        cnts <- matrix(nrow = 0, ncol=2)
    }
    h5file$close_all()
    on.exit()
    cnts
}
#'
readATACdataByCoor <- function(h5f, cells, coord){
    stopifnot(is.character(cells))
    stopifnot(is.list(coord))
    stopifnot(all(c("seqnames", "start", "end") %in% names(coord)))
    peaks <- readData("sc1peak", h5f)
    sel <- which(peaks$seqnames %in% coord$seqnames &
                     peaks$start <= coord$end &
                     peaks$end >= coord$start)
    if(length(sel)==0){
        return(data.frame())
    }
    cnts <- lapply(cells, function(cell){
        cnts <- readATACdata(h5f, cell)
        cnts <- cnts[match(sel, cnts[, 1]), 2, drop=TRUE]
    })
    cnts <- do.call(cbind, cnts)
    colnames(cnts) <- cells
    cnts[is.na(cnts)] <- 0
    cnts <- as.data.frame(cnts)
    cnts <- cbind(peaks[sel, , drop=FALSE], cnts)
}

