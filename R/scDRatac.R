# Plot ATAC track on dimred
#' @importFrom ggplot2 ggplot aes_string geom_line xlab ylab guides
#' scale_x_continuous xlim vars
#' @importFrom ggridges geom_ridgeline theme_ridges
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRanges strand seqnames gaps
scDRatac <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        gene1,
        coord,
        subsetCellKey,
        subsetCellVal,
        dataset,
        geneIdMap,
        pointSize,
        gradientCol,
        labelsFontsize,
        plotAspectRatio,
        keepXYlables,
        ...) {
    if (gene1[1] == "") {
        return(ggplot())
    }
    if (coord[1] == "") {
        return(ggplot())
    }
    if (is.na(geneIdMap[gene1])) {
        return(ggplot())
    }
    if (is.null(geneIdMap[gene1])) {
        return(ggplot())
    }
    coord <- strsplit(coord, "-|:")[[1]]
    names(coord) <- c("seqnames", "start", "end")
    coord <- as.list(coord)
    coord[-1] <- lapply(coord[-1], as.numeric)
    # Prepare ggData
    pf <- file.path(
        .globals$datafolder, dataset,
        .globals$filenames$bwspath,
        subsetCellKey)
    bws <- dir(pf, ".bigwig$")
    if(length(bws)==0){
        return(ggplot())
    }
    gr <- GRanges(as.data.frame(coord))
    atac_sig <- lapply(subsetCellVal, function(.ele){
        f <- file.path(pf, paste0(.ele, ".bigwig"))
        if(file.exists(f)){
            import(f, format="BigWig", which=gr, as="GRanges")
        }else{
            GRanges()
        }
    })
    names(atac_sig) <- subsetCellVal
    atac_sig <- lapply(atac_sig, function(.ele){
        .ele <- sort(c(.ele, gaps(.ele)))
        .ele <- .ele[strand(.ele)=="*"]
        .ele <- .ele[seqnames(.ele)==coord$seqnames]
        .ele$score[is.na(.ele$score)] <- 0
        start(.ele)[start(.ele)<coord$start] <- coord$start
        end(.ele)[end(.ele)>coord$end] <- coord$end
        .ele
    })
    ggData <- lapply(atac_sig, function(.ele){
        .ele <- as.data.frame(.ele)
        .ele <- .ele[, c("start", "end", "score")]
        colnames(.ele) <- NULL
        .ele <- data.frame(
            x=c(.ele[, 1], .ele[, 2]),
            y=c(.ele[, 3], .ele[, 3]))
        .ele[order(.ele[, 1]), , drop=FALSE]
    })
    
    gp <- rep(names(ggData), vapply(ggData, FUN=nrow, FUN.VALUE = integer(1L)))
    ggData <- do.call(rbind, ggData)
    colnames(ggData) <- c("x", "y")
    ggData <- as.data.frame(ggData)
    # Actual ggplot
    ggData$group <- factor(
        gp,
        levels = rev(sortLevels(as.character(
            unique(gp)
        ))))
    ggOut <- ggplot(
        ggData,
        aes_string(
            x = "x",
            y = "y",
            fill = "group"
        )) +
        geom_line() +
        scale_x_continuous(expand = c(0, 0)) +
        facet_grid(rows = vars(ggData$group)) +
        ylab("Groups") +
        xlab("Acc Level")
    
    return(ggOut)
}
