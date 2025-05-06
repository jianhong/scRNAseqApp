# Plot ATAC track on dimred
#' @importFrom ggplot2 ggplot aes .data geom_line xlab ylab guides
#' scale_x_continuous xlim vars
#' @importFrom gridExtra grid.arrange
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRanges strand seqnames gaps
#' @importFrom patchwork wrap_plots
#' @importFrom IRanges subsetByOverlaps
#' @importFrom shiny Progress
scDRatac <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        gene1,
        coord,
        subsetCellKey,
        subsetCellVal, # not support multiple key and val pairs
        dataset,
        geneIdMap,
        pointSize,
        gradientCol,
        labelsFontsize=24,
        labelsFontFamily="Helvetica",
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
    # Prepare ggData
    pf <- file.path(
        .globals$datafolder, dataset,
        .globals$filenames$bwspath,
        subsetCellKey)
    bws <- dir(pf, ".bigwig$")
    if(length(bws)==0){
        warning('no bws detected. Please check the folder privileges for ', pf)
        return(ggplot())
    }
    gr <- GRanges(coord)
    if(width(gr)[1]>10000000){
        showNotification("The region is greater than 10M! Please decrease the plot region.",
                         type = "error")
        return(ggplot())
    }
    progress <- Progress$new()
    progress$set(message = 'Plot ATAC signals', value =0)
    on.exit(progress$close())
    updateProgress <- function(value = NULL, n = 1, detail=NULL) {
        if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / n
        }
        progress$set(value = value, detail=detail)
    }
    atac_sig <- lapply(subsetCellVal, function(.ele){
        sample <- .ele
        f <- file.path(pf, paste0(.ele, ".bigwig"))
        if(file.exists(f)){
            sig <- import(f, format="BigWig", which=gr, as="GRanges")
        }else{
            sig <- GRanges()
        }
        updateProgress(n=length(subsetCellVal)+5, detail=paste('loading', sample))
        return(sig)
    })
    names(atac_sig) <- subsetCellVal
    atac_sig <- lapply(atac_sig, function(.ele){
        .ele <- sort(c(.ele, gaps(.ele)))
        .ele <- .ele[strand(.ele)=="*"]
        .ele <- subsetByOverlaps(.ele, gr)
        .ele$score[is.na(.ele$score)] <- 0
        start(.ele)[start(.ele)<start(gr)[1]] <- start(gr)[1]
        end(.ele)[end(.ele)>end(gr)[1]] <- end(gr)[1]
        .ele
    })
    updateProgress(n=5)
    ggData <- lapply(atac_sig, function(.ele){
        .ele <- as.data.frame(.ele)
        .ele <- .ele[, c("start", "end", "score")]
        colnames(.ele) <- NULL
        .ele <- data.frame(
            x=c(.ele[, 1], .ele[, 2]),
            y=c(.ele[, 3], .ele[, 3]))
        .ele[order(.ele[, 1]), , drop=FALSE]
    })
    updateProgress(n=4)
    LN <- length(ggData)
    gp <- rep(names(ggData), vapply(ggData, FUN=nrow, FUN.VALUE = integer(1L)))
    ggData <- do.call(rbind, ggData)
    if(length(dim(ggData))==0){
        return(ggplot())
    }
    if(nrow(ggData)<1){
        return(ggplot())
    }
    if(ncol(ggData)!=2){
        return(ggplot())
    }
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
        aes(
            x = .data[["x"]],
            y = .data[["y"]],
            fill = .data[["group"]]
        )) +
        geom_line() +
        scale_x_continuous(expand = c(0, 0)) +
        facet_grid(rows = vars(ggData$group)) +
        ylab("Groups") +
        xlab("Acc Level") +
        scale_y_continuous(breaks = scales::breaks_extended(n = 2)) +
        theme_classic() +
        theme(
            strip.text.y.right = element_text(angle = 0, hjust=0, 
                                              size = labelsFontsize,
                                              family = labelsFontFamily),
            strip.background = element_blank() )
    updateProgress(n=3)
    linkp <- LinkPlot(dataset, gr)
    anno <- AnnotationPlot(dataset, gr, ylimt0 = ifelse(length(linkp)>0, -1, -0.6)) + 
        PeakPlot(dataset, gr) +
        linkp
    updateProgress(n=2)
    return(wrap_plots(
        ggOut, anno, 
        nrow=2, ncol=1,
        heights=c(LN, ifelse(length(linkp)>0, 2.5, 1.5))))
}

#' Plot peaks in a genomic region
#'
#' @noRd
#'
#' @param dataset dataset folder
#' @param region A genomic region to plot
#' @param color Color to use.
#' 
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#' @importFrom GenomicRanges start end
#' @importFrom IRanges subsetByOverlaps
#' @importFrom GenomeInfoDb seqnames
#' @importFrom ggplot2 ggplot aes geom_segment theme_classic element_blank
#' theme xlab ylab scale_color_manual .data
PeakPlot <- function(
        dataset,
        region,
        color = "dimgrey"
) {
    peaks <- readRDS(file.path(
        .globals$datafolder,
        dataset,
        .globals$filenames$sc1peak
    ))
    peaks <- GRanges(peaks)
    # subset to covered range
    peak.intersect <- subsetByOverlaps(x = peaks, ranges = region)
    peak.df <- as.data.frame(x = peak.intersect)
    start.pos <- start(x = region)
    end.pos <- end(x = region)
    chromosome <- seqnames(x = region)
    
    if (nrow(x = peak.df) > 0) {
        peak.df$start[peak.df$start < start.pos] <- start.pos
        peak.df$end[peak.df$end > end.pos] <- end.pos
        peak.plot <- geom_segment(
                data = peak.df,
                aes(
                    x = .data$start,
                    y = .1,
                    xend = .data$end,
                    yend = .1),
                size = 2,
                color = color,
                inherit.aes = FALSE)
    } else {
        # no peaks present in region, make empty panel
        peak.plot <- NULL
    }
    return(peak.plot)
}

#' Plot linked genomic elements
#'
#' Display links between pairs of genomic elements within a given region of the
#' genome.
#' @noRd
#' @param dataset dataset folder
#' @param region A genomic region to plot#' 
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#' @importFrom IRanges subsetByOverlaps
#' @importFrom GenomicRanges start end
#' @importFrom GenomeInfoDb seqnames
#' @importFrom ggplot2 ggplot geom_hline aes theme_classic xlim
#' ylab theme element_blank scale_color_gradient2
#' @importFrom ggforce geom_bezier
LinkPlot <- function(
        dataset,
        region
) {
    chromosome <- seqnames(x = region)
    
    # extract link information
    links <- readRDS(file.path(
        .globals$datafolder,
        dataset,
        .globals$filenames$sc1link))
    
    # if links not set, return NULL
    if (length(x = links) == 0) {
        return(NULL)
    }
    if (!'score' %in% colnames(mcols(links))){
        return(NULL)
    }
    ## re-scale scores to fit the color bar (.1-.9)
    min.color <- min(0, min(links$score))
    oldRange <- range(
        c(min.color, links$score[is.finite(links$score)]), na.rm = TRUE)
    oldRange <- diff(oldRange)
    if(oldRange==0) oldRange <- 1
    links$score <- (links$score - min.color) * .8 / oldRange + .1
    # subset to those in region
    links.keep <- subsetByOverlaps(x = links, ranges = region, type='within')
    
    link.df <- as.data.frame(x = links.keep)
    # plot
    if (nrow(x = link.df) > 0) {
        # convert to format for geom_bezier
        df <- data.frame(
            x = c(link.df$start,
                  (link.df$start + link.df$end) / 2,
                  link.df$end),
            y = c(rep(x = 0, nrow(x = link.df)),
                  rep(x = -1, nrow(x = link.df)),
                  rep(x = 0, nrow(x = link.df))),
            score = rep(link.df$score, 3),
            group = rep(seq_along(links.keep), 3)
        )
        p <- geom_bezier(data = df, aes(
            x = .data$x, y = .data$y, color = .data$score, group=.data$group
        ), inherit.aes = FALSE)
    } else {
        p <- NULL
    }
    return(p)
}

#' Plot gene annotations
#'
#' Display gene annotations in a given region of the genome.
#' @noRd
#' @param dataset dataset folder
#' @param region A genomic region to plot
#' @param mode Display mode. Choose either "gene" or "transcript" to determine
#' whether genes or transcripts are plotted.
#' @param ylimt0 ylim0
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#' @importFrom IRanges subsetByOverlaps
#' @importFrom GenomicRanges start end
#' @importFrom GenomeInfoDb seqnames
#' @importFrom ggplot2 theme_classic ylim xlim ylab xlab
#' geom_segment geom_text aes scale_color_manual .data
#' @importFrom grid arrow
#' @importFrom S4Vectors split
AnnotationPlot <- function(
        dataset,
        region,
        mode = c("gene", "transcript"),
        ylimt0 = -0.6
) {
    mode <- match.arg(mode)
    if(mode == "gene") {
        collapse_transcript <- TRUE
        label <- "gene_name"
    } else if (mode == "transcript") {
        collapse_transcript <- FALSE
        label <- "tx_id"
    }
    annotation <- readRDS(
        file.path(
            .globals$datafolder,
            dataset,
            .globals$filenames$sc1anno))
    if (is.null(x = annotation)) {
        return(NULL)
    }
    # get names of genes that overlap region, then subset to include only those
    # genes. This avoids truncating the gene if it runs outside the region
    annotation.subset <- subsetByOverlaps(x = annotation, ranges = region)
    if (mode == "gene") {
        genes.keep <- unique(x = annotation.subset$gene_name)
        annotation.subset <- annotation[
            match(
                x = annotation$gene_name,
                table = genes.keep,
                nomatch = 0L) > 0L
        ]
    } else {
        tx.keep <- unique(x = annotation.subset$tx_id)
        annotation.subset <- annotation[
            match(x = annotation$tx_id, table = tx.keep, nomatch = 0L) > 0L
        ]
    }
    
    start.pos <- start(x = region)
    end.pos <- end(x = region)
    if (length(x = annotation.subset) == 0) {
        # make empty plot
        p <- ggplot(data = data.frame())
        y_limit <- c(ylimt0, 1)
    } else {
        annotation_df_list <- reformat_annotations(
            annotation = annotation.subset,
            start.pos = start.pos,
            end.pos = end.pos,
            collapse_transcript = collapse_transcript
        )
        colorFun <- function(strand){
            ifelse(strand=="-", 0, 1)
        }
        p <- ggplot() +
            # exons
            geom_segment(
                data = annotation_df_list$exons,
                mapping = aes(
                    x = .data$start,
                    y = .data$dodge,
                    xend = .data$end,
                    yend = .data$dodge,
                    color = colorFun(.data$strand)
                ),
                show.legend = FALSE,
                size = 3
            ) +
            # gene body
            geom_segment(
                data = annotation_df_list$labels,
                mapping = aes(
                    x = .data$start,
                    y = .data$dodge,
                    xend = .data$end,
                    yend = .data$dodge,
                    color = colorFun(.data$strand)
                ),
                show.legend = FALSE,
                size = 1/2
            )
        if (nrow(x = annotation_df_list$plus) > 0) {
            # forward strand arrows
            p <- p + geom_segment(
                data = annotation_df_list$plus,
                mapping = aes(
                    x = .data$start,
                    y = .data$dodge,
                    xend = .data$end,
                    yend = .data$dodge,
                    color = colorFun(.data$strand)
                ),
                arrow = arrow(
                    ends = "last",
                    type = "open",
                    angle = 45,
                    length = unit(x = 0.04, units = "inches")
                ),
                show.legend = FALSE,
                size = 1/2
            )
        }
        if (nrow(x = annotation_df_list$minus) > 0) {
            # reverse strand arrows
            p <- p + geom_segment(
                data = annotation_df_list$minus,
                mapping = aes(
                    x = .data$start,
                    y = .data$dodge,
                    xend = .data$end,
                    yend = .data$dodge,
                    color = colorFun(.data$strand)
                ),
                arrow = arrow(
                    ends = "first",
                    type = "open",
                    angle = 45,
                    length = unit(x = 0.04, units = "inches")
                ),
                show.legend = FALSE,
                size = 1/2
            )
        }
        # label genes
        n_stack <- max(annotation_df_list$labels$dodge)
        annotation_df_list$labels$dodge <- annotation_df_list$labels$dodge + 0.2
        p <- p + geom_text(
            data = annotation_df_list$labels,
            mapping = aes(
                x = .data$position,
                y = .data$dodge,
                label = .data[[label]]),
            size = 2.5
        )
        y_limit <- c(ylimt0, n_stack + 0.4)
    }
    color_breaks <- c(-.05, seq(.1, .9, length.out=9), 1.05)
    colors <- c(
        "darkgreen",
        '#007FFF', '#4CC3FF', '#99EDFF', '#CCFFFF',
        '#FFFFCC', '#FFEE99', '#FFC34C', '#FF7F00',
        "darkblue")
    p <- p +
        theme_classic() +
        ylab("Genes/peaks") +
        xlab(label = paste0(seqnames(region), " position (bp)")) +
        xlim(start.pos, end.pos) +
        ylim(y_limit) +
        theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
        ) +
        scale_color_gradientn(
            colors = colors, breaks = color_breaks,
            name = 'score')
    return(p)
}

# copied from Signac
split_body <- function(df, width = 1000) {
wd <- df$end - df$start
nbreak <- wd / width
if (nbreak > 1) {
    steps <- 0:(nbreak)
    starts <- (width * steps) + df$start
    starts[starts > df$end] <- NULL
} else {
    starts <- df$end
}
breaks <- data.frame(
    seqnames = df$seqnames[[1]],
    start = starts,
    end = starts + 1,
    strand = df$strand[[1]],
    tx_id = df$tx_id[[1]],
    gene_name = df$gene_name[[1]],
    gene_biotype = df$gene_biotype[[1]],
    type = "arrow"
)
return(breaks)
}
#' @importFrom GenomicRanges makeGRangesFromDataFrame reduce
record_overlapping <- function(
        annotation,
        min.gapwidth = 1000,
        collapse_transcript = TRUE
) {
    # convert back to granges
    annotation$strand <- "*"
    gr <- makeGRangesFromDataFrame(
        df = annotation[annotation$type == "body", ], keep.extra.columns = TRUE
    )
    # work out which ranges overlap
    collapsed <- reduce(
        x = gr, with.revmap = TRUE, min.gapwidth = min.gapwidth
    )$revmap
    idx <- seq_along(gr)
    for (i in seq_along(collapsed)) {
        mrg <- collapsed[[i]]
        for (j in seq_along(mrg)) {
            idx[[mrg[[j]]]] <- j
        }
    }
    if (collapse_transcript) {
        names(x = idx) <- gr$gene_name
    } else {
        names(x = idx) <- gr$tx_id
    }
    return(idx)
}

#' @importFrom S4Vectors mcols
reformat_annotations <- function(
        annotation,
        start.pos,
        end.pos,
        collapse_transcript = TRUE
) {
    total.width <- end.pos - start.pos
    tick.freq <- total.width / 50
    for(i in c('tx_id', 'gene_name')){
        if(!i %in% colnames(mcols(annotation))){
            stop(i, ' is missing from annotation.',
                 ' Please reprepare the annotations.')
        }
    }
    if(length(annotation$gene_biotype)!=length(annotation)){
        if(length(annotation)){
            annotation$gene_biotype <- NA
        }
    }
    if('type' %in% colnames(mcols(annotation))){
        annotation <- annotation[annotation$type == "exon"]
    }
    annotation <- unname(annotation)
    exons <- as.data.frame(x = annotation)
    if (collapse_transcript) {
        annotation <- split(
            x = annotation,
            f = annotation$gene_name
        )
    } else {
        annotation <- split(
            x = annotation,
            f = annotation$tx_id
        )
    }
    annotation <- lapply(X = annotation, FUN = as.data.frame)
    
    # add gene total start / end
    gene_bodies <- list()
    for (i in seq_along(annotation)) {
        df <- data.frame(
            seqnames = annotation[[i]]$seqnames[[1]],
            start = min(annotation[[i]]$start),
            end = max(annotation[[i]]$end),
            strand = annotation[[i]]$strand[[1]],
            tx_id = annotation[[i]]$tx_id[[1]],
            gene_name = annotation[[i]]$gene_name[[1]],
            gene_biotype = annotation[[i]]$gene_biotype[[1]],
            type = "body"
        )
        # trim any that extend beyond region
        df$start <- ifelse(
            test = df$start < start.pos,
            yes = start.pos,
            no = df$start
        )
        df$end <- ifelse(
            test = df$end > end.pos,
            yes = end.pos,
            no = df$end
        )
        breaks <- split_body(df = df, width = tick.freq)
        df <- rbind(df, breaks)
        gene_bodies[[i]] <- df
    }
    gene_bodies <- do.call(what = rbind, args = gene_bodies)
    
    # record if genes overlap
    overlap_idx <- record_overlapping(
        annotation = gene_bodies,
        min.gapwidth = 1000,
        collapse_transcript = collapse_transcript
    )
    # overlap_idx <- overlap_idx
    if (collapse_transcript) {
        gene_bodies$dodge <- overlap_idx[gene_bodies$gene_name]
        exons$dodge <- overlap_idx[exons$gene_name]
    } else {
        gene_bodies$dodge <- overlap_idx[gene_bodies$tx_id]
        exons$dodge <- overlap_idx[exons$tx_id]
    }
    
    label_df <- gene_bodies[gene_bodies$type == "body", ]
    label_df$width <- label_df$end - label_df$start
    label_df$position <- label_df$start + (label_df$width / 2)
    
    onplus <- gene_bodies[gene_bodies$strand %in% c("*", "+"), ]
    onminus <- gene_bodies[gene_bodies$strand == "-", ]
    
    return(
        list(
            "labels" = label_df,
            "exons" = exons,
            "plus" = onplus,
            "minus" = onminus
        )
    )
}
