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
    updateConfigTblLocker(folder, TRUE)
}
removeLocker <- function(folder) {
    unlink(file.path(.globals$datafolder, folder, .globals$filenames$locker))
    updateConfigTblLocker(folder, FALSE)
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
#' @importFrom rhdf5 h5read
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
    if (valueOnly) {
        if (!missing(cell)) {
            expr <- h5read(fs,
                           .globals$h5fGrp,
                           index=list(NULL, cell[1]))[, 1]
        } else{
            if (!is.na(genesID)) {
                expr <- h5read(fs,
                               .globals$h5fGrp,
                               index=list(genesID[1], NULL))[1, ]
            } else{
                expr <- 0
            }
        }
        return(expr)
    }
    exprs <- data.table()
    vals <- h5read(fs, .globals$h5fGrp, index=list(genesID, NULL))
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
        tmp$val <- vals[idx, , drop=TRUE]
        if(all(tmp$val==0) && length(tmp$val)>0){
            setGeneExprForData(symbol = names(genesID)[idx],
                               dataset = h5f,
                               expr = 0)
        }
        exprs <- rbindlist(list(exprs, tmp))
    }
    exprs
}

#' read ATAC counts in peaks
#' The data was write as
#' /cell/cell-name/matrix
#' matrix is sparse matrix, the first column is the index number start from 1
#' of the peak; the second column is the count number.
#' @noRd
#' @importFrom rhdf5 h5createFile h5createGroup h5write
#' @importMethodsFrom Matrix summary
encodeCoord <- function(coorStr, split='-', min.gapwidth=1e5){
    stopifnot(is.character(coorStr))
    coorStr <- strsplit(coorStr, split = split)
    coorStr <- do.call(rbind, coorStr)
    coor <- GRanges(coorStr[, 1], IRanges(as.numeric(coorStr[, 2]), 
                                          as.numeric(coorStr[, 3])))
    gr <- reduce(coor, min.gapwidth=min.gapwidth, with.revmap=TRUE)
    revmap <- rep(seq_along(gr), lengths(gr$revmap))
    names(revmap) <- unlist(gr$revmap)
    return(revmap)
}
#' @importFrom IRanges IRanges nearest
decodeCoord <- function(index, revmap){
    revmap[as.character(index)]
}
writeATACdata <- function(acAsy, appDir){
    filename <- file.path(appDir, .globals$filenames$sc1atac)
    if(h5createFile(filename)){
        if(is(acAsy, 'sparseMatrix')){
            x <- summary(acAsy, sparse=TRUE)
            # search by coordinates
            revmap <- encodeCoord(rownames(acAsy))
            saveRDS(revmap, file.path(appDir, .globals$filenames$coor_revmap))
            x_coors <- split(x[, c("i", "j", "x")],
                             revmap[as.character(x[, "i"])])
            if(h5createGroup(filename, .globals$h5fATACcoor)){
                null <- mapply(function(.x, .n){
                    ij <- paste0(.globals$h5fATACcoor, '/ij', .n)
                    if(h5createDataset(filename, ij, dims = c(nrow(.x), 2),
                                       storage.mode = "integer",
                                       level=7)){
                        h5write(unname(as.matrix(.x[, c('i', 'j')])), file=filename,
                                name=ij)
                    }
                    v <- paste0(.globals$h5fATACcoor, '/v', .n)
                    if(h5createDataset(filename, v, dims = c(nrow(.x), 1),
                                       storage.mode = "double",
                                       level=7)){
                        h5write(.x[, c('x')], file=filename,
                                name=v)
                    }
                }, x_coors, names(x_coors))
            }
        }else{
            warning('The ATAC matrix must be a sparseMatrix.')
        }
    }
}
#' @importFrom rhdf5 h5read H5Lexists H5Fopen H5Fclose
readATACdata <- function(h5f, index){
    fs <- file.path(
        .globals$datafolder,
        h5f,
        .globals$filenames$sc1atac)
    h5f <- H5Fopen(fs)
    on.exit(H5Fclose(h5f))
    stopifnot(length(index)==1)
    if(H5Lexists(h5f, paste0(.globals$h5fATACcoor, '/ij', index)) &&
       H5Lexists(h5f, paste0(.globals$h5fATACcoor, '/v', index))){
        cnts <- cbind(
            h5read(h5f, paste0(.globals$h5fATACcoor, '/ij', index)),
            h5read(h5f, paste0(.globals$h5fATACcoor, '/v', index)))
        colnames(cnts) <- c('i', 'j', 'x')
    }else{
        cnts <- matrix(nrow = 0, ncol=3,
                       dimnames = list(c(), c('i', 'j', 'x')))
    }
    H5Fclose(h5f)
    on.exit()
    cnts
}

#' @importFrom rhdf5 h5ls
#' @importFrom Matrix sparseMatrix
readATACdataByCoor <- function(h5f, coord, cells, revmap, peaks){
    stopifnot(is.character(cells))
    stopifnot(is.list(coord))
    stopifnot(all(c("seqnames", "start", "end") %in% names(coord)))
    if(missing(peaks)) peaks <- readData("sc1peak", h5f)
    if(missing(revmap)) revmap <- readData('coor_revmap', h5f)
    sel <- which(peaks$seqnames %in% coord$seqnames &
                     peaks$start <= coord$end &
                     peaks$end >= coord$start)
    if(length(sel)==0){
        return(data.frame())
    }
    peaks <- paste(peaks$seqnames, peaks$start, peaks$end, sep='-')
    selIndex <- decodeCoord(sel, revmap)
    cnts <- lapply(selIndex, function(i){
        readATACdata(h5f, index=i)
    })
    cnts <- do.call(rbind, cnts)
    cnts <- cnts[cnts[, 'i'] %in% sel, , drop=FALSE]
    cnts <- sparseMatrix(i=cnts[, 'j'],
                         j=match(cnts[, 'i'], sel),
                         x=cnts[, 'x'],
                         dims=c(length(cells), length(sel)),
                         dimnames = list(
                             cells,
                             peaks[sel]
                         ))
    cnts[is.na(cnts)] <- 0
    cnts <- as.data.frame(cnts)
}

