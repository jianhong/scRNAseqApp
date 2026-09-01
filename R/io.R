readData <- function(slot, folder) {
    fs <- file.path(.globals$datafolder, folder, .globals$filenames[[slot]])
    if(file.exists(fs)){
        return(readRDS(fs))
    }
    return(NULL)
}
loadData <- function(dataSource) {
    for (i in c("sc1conf", "sc1def", "sc1gene", "sc1meta", "sc1gsgene")) {
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

generateToken <- function(){
    tokens <- getTokenList()
    token <- paste(sample(c(letters, LETTERS, seq(0, 9)),
                          size=12, replace = TRUE),
                   collapse = '')
    while (token %in% names(tokens)) {
        token <- paste(sample(c(letters, LETTERS, seq(0, 9)),
                              size=12, replace = TRUE),
                       collapse = '')
    }
    return(token)
}
setToken <- function(folder, token){
    if(is.null(token)){
        token <- ''
    }
    if(nchar(token)<.globals$tokenMinLen){
        token <- generateToken()
    }
    writeLines(
        token,
        file.path(.globals$datafolder, folder, .globals$filenames$token)
    )
    adminMsg(paste0("new token is: ", token,
                   "; use link: /?token=", token),
             "message", duration = 30)
}
removeToken <- function(folder){
    unlink(file.path(.globals$datafolder, folder, .globals$filenames$token))
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
#' @param h5_fn Filename of h5 file
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
        h5_fn = .globals$filenames$sc1gexpr,
        cell) {
    fs <- file.path(
        .globals$datafolder,
        h5f,
        h5_fn)
    if(!file.exists(fs)){
        warning("No expression data available. Data may be removed.")
        return(NULL)
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

#' read molecules
#' @noRd
readMolecule <- function(molecule_fs, fov, molecule){
    if(!file.exists(molecule_fs)){
        ## convert old format to h5 format
        molecule_data <- touchMolecule(molecule_fs)
        if(!missing(fov)){
            if(all(!fov %in% names(molecule_data))) return(NULL)
            molecule_data <- molecule_data[fov]
        }
        molecule_data <- do.call(rbind, molecule_data)
        if(missing(molecule)){
                return(sort(unique(molecule_data$molecule)))
        }else{
            return(molecule_data[
                molecule_data$molecule %in% molecule, , drop=FALSE])
        }
    }else{
        ## read from h5
        if(missing(molecule)){
            return(getMoleculeOnly(molecule_fs, fov))
        }else{
            if(missing(fov)){
                fov <- listMoleculeFOV(molecule_fs)
            }
            res <- lapply(fov, function(rd){
                do.call(rbind, lapply(molecule, function(.ele){
                    getMoleculeXY(h5_file=molecule_fs, fov=rd, molecule=.ele)
                }))
            })
            return(do.call(rbind, res))
        }
    }
    return(NULL)
}

checkMoleculeFile <- function(molecule_fs){
    if(length(molecule_fs)==0) return(FALSE)
    if(file.exists(molecule_fs)){
        return(TRUE)
    }
    rds_file <- file.path(sub('.h5$', '.rds', molecule_fs))
    return(file.exists(rds_file))
}

touchMolecule <- function(molecule_fs){
    rds_file <- file.path(sub('.h5$', '.rds', molecule_fs))
    if(!file.exists(rds_file)) return(NULL)
    molecule_data <- readRDS(rds_file)
    writeMolecule(molecule_data, molecule_fs)
    return(molecule_data)
}

#' @importFrom rhdf5 h5writeAttribute h5readAttributes h5closeAll
writeMolecule <- function(molecule_data, h5_file, overwrite = TRUE){
    showNotification("prepare the molecule data.", type='message')
    if (overwrite && file.exists(h5_file)) file.remove(h5_file)
    h5createFile(h5_file)
    for (fov in names(molecule_data)) {
        df <- molecule_data[[fov]]
        # Sort rows by molecule so same-molecule rows are contiguous
        df <- df[order(df$molecule), ]
        mol_char <- as.character(df$molecule)
        # Build index: for each unique molecule, its start (0-based) and count
        rle_mol <- rle(mol_char)
        starts <- c(0, head(cumsum(rle_mol$lengths), -1))
        index_df <- data.frame(
            molecule = rle_mol$values,
            start    = starts,
            count    = rle_mol$lengths,
            stringsAsFactors = FALSE
        )
        group <- paste0("/", fov)
        h5createGroup(h5_file, group)
        
        # Data, sorted
        h5write(df$x, h5_file, paste0(group, "/x"))
        h5write(df$y, h5_file, paste0(group, "/y"))
        h5write(mol_char, h5_file, paste0(group, "/molecule"))
        h5writeAttribute(range(df$x), h5obj = h5_file,
                         name = "x_range", h5loc = group)
        h5writeAttribute(range(df$y), h5obj = h5_file,
                         name = "y_range", h5loc = group)
        
        # Index for fast lookup
        h5write(index_df$molecule, h5_file, paste0(group, "/index_molecule"))
        h5write(index_df$start,    h5_file, paste0(group, "/index_start"))
        h5write(index_df$count,    h5_file, paste0(group, "/index_count"))
    }
    
    h5closeAll()
    invisible(h5_file)
}

getMoleculeRange <- function(h5_file, fov) {
    if(!file.exists(h5_file)){
        molecule_data <- touchMolecule(h5_file)
        if(length(molecule_data)==0) return(NULL)
    }
    fovs <- listMoleculeFOV(h5_file)
    if(missing(fov)){
        xy_ranges <- lapply(fovs, getMoleculeRange, h5_file=h5_file)
        return(
            list(
                x = range(unlist(lapply(xy_ranges,
                                        function(.ele) .ele$x))),
                y = range(unlist(lapply(xy_ranges,
                                        function(.ele) .ele$y)))
            )
        )
    }
    if(!fov %in% fovs){
        return(NULL)
    }
    attrs <- h5readAttributes(h5_file, paste0("/", fov))
    list(x = attrs$x_range, y = attrs$y_range)
}

getMoleculeXY <- function(h5_file, fov, molecule) {
    if(!file.exists(h5_file)){
        molecule_data <- touchMolecule(h5_file)
        if(length(molecule_data)==0) return(NULL)
        molecule_data <- molecule_data[[fov]]
        return(molecule_data[
            molecule_data$molecule %in% molecule, , drop=FALSE])
    }
    fovs <- listMoleculeFOV(h5_file)
    if(!fov %in% fovs) return(data.frame(x = numeric(0),
                                         y = numeric(0),
                                         molecule = character(0)))
    group <- paste0("/", fov)
    
    idx_mol   <- h5read(h5_file, paste0(group, "/index_molecule"))
    idx_start <- h5read(h5_file, paste0(group, "/index_start"))
    idx_count <- h5read(h5_file, paste0(group, "/index_count"))
    
    pos <- which(idx_mol == molecule)
    if (length(pos) == 0) return(data.frame(x = numeric(0),
                                            y = numeric(0),
                                            molecule = character(0)))
    
    start <- idx_start[pos]  # 0-based
    count <- idx_count[pos]
    rows  <- (start + 1):(start + count)   # convert to 1-based
    
    x <- h5read(h5_file, paste0(group, "/x"), index = list(rows))
    y <- h5read(h5_file, paste0(group, "/y"), index = list(rows))
    
    data.frame(x, y, molecule)
}

getMoleculeOnly <- function(h5_file, fov){
    if(!file.exists(h5_file)){
        molecule_data <- touchMolecule(h5_file)
        if(length(molecule_data)==0) return(NULL)
        if(missing(fov)){
            fov <- names(molecule_data)
        }
        molecule_data <- do.call(rbind, molecule_data[fov])
        return(sort(unique(molecule_data$molecule)))
    }
    if(missing(fov)){
        groups <- h5ls(h5_file, recursive = FALSE)
        fovs <- groups$name[groups$group == "/"]
        molecules <- lapply(fovs, getMoleculeOnly, h5_file=h5_file)
    }else{
        if(length(fov)>1){
            molecules <- lapply(fovs, getMoleculeOnly, h5_file=h5_file)
        }else{
            group <- paste0("/", fov)
            fovs <- listMoleculeFOV(h5_file)
            if(fov %in% fovs){
                molecules <- h5read(h5_file, paste0(group, "/index_molecule"))
            }else{
                return(getMoleculeOnly(h5_file, fovs[1]))
            }
        }
    }
    sort(unique(unlist(molecules)))
}

listMoleculeFOV <- function(h5_file) {
    if(!file.exists(h5_file)){
        molecule_data <- touchMolecule(h5_file)
        if(length(molecule_data)==0) return(NULL)
        return(names(molecule_data))
    }
    groups <- h5ls(h5_file, recursive = FALSE)
    groups$name[groups$group == "/"]
}
