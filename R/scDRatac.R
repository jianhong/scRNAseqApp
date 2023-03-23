# Plot ATAC track on dimred
#' @importFrom ggplot2 ggplot aes .data geom_line xlab ylab guides
#' scale_x_continuous xlim vars
#' @importFrom gridExtra grid.arrange
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRanges strand seqnames gaps
#' @importFrom patchwork wrap_plots
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
    LN <- length(ggData)
    gp <- rep(names(ggData), vapply(ggData, FUN=nrow, FUN.VALUE = integer(1L)))
    ggData <- do.call(rbind, ggData)
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
            strip.text.y.right = element_text(angle = 0, hjust=0),
            strip.background = element_blank() )
    anno <- AnnotationPlot(dataset, gr)
    peaks <- PeakPlot(dataset, gr)
    links <- LinkPlot(dataset, gr)
    return(wrap_plots(
        ggOut, anno, peaks, links,
        nrow=4, ncol=1,
        heights=c(LN, 1, .5, .5)))
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
        peak.plot <- ggplot(
            data = peak.df,
            aes(
                x = .data$start,
                y = 0,
                xend = .data$end,
                yend = 0)) +
            geom_segment(size = 2)
    } else {
        # no peaks present in region, make empty panel
        peak.plot <- ggplot(data = peak.df)
    }
    peak.plot <- peak.plot + theme_classic() +
        ylab(label = "Peaks") +
        theme(axis.ticks.y = element_blank(),
              axis.text.y = element_blank()) +
        xlab(label = paste0(chromosome, " position (bp)")) +
        xlim(c(start.pos, end.pos))
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
    
    # subset to those in region
    links.keep <- subsetByOverlaps(x = links, ranges = region)
    
    link.df <- as.data.frame(x = links.keep)
    
    # remove links outside region
    link.df <- link.df[link.df$start >= start(x = region) & 
                           link.df$end <= end(x = region), ]
    
    # plot
    if (nrow(x = link.df) > 0) {
        # convert to format for geom_bezier
        link.df$group <- seq_len(length.out = nrow(x = link.df))
        df <- data.frame(
            x = c(link.df$start,
                  (link.df$start + link.df$end) / 2,
                  link.df$end),
            y = c(rep(x = 0, nrow(x = link.df)),
                  rep(x = -1, nrow(x = link.df)),
                  rep(x = 0, nrow(x = link.df))),
            group = rep(x = link.df$group, 3),
            score = rep(link.df$score, 3)
        )
        min.color <- min(0, min(df$score))
        p <- ggplot(data = df, aes(
            x = .data$x, y = .data$y, group = .data$group, color = .data$score
        )) + geom_bezier() +
            geom_hline(yintercept = 0, color = 'grey') +
            scale_color_gradient2(
                low = "red", mid = "grey", high = "blue",
                limits = c(min.color, max(df$score)),
                n.breaks = 3)
    } else {
        p <- ggplot(data = link.df)
    }
    p <- p +
        theme_classic() +
        theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) +
        ylab("Links") +
        xlab(label = paste0(chromosome, " position (bp)")) +
        xlim(c(start(x = region), end(x = region)))
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
        mode = c("gene", "transcript")
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
        y_limit <- c(0, 1)
    } else {
        annotation_df_list <- reformat_annotations(
            annotation = annotation.subset,
            start.pos = start.pos,
            end.pos = end.pos,
            collapse_transcript = collapse_transcript
        )
        p <- ggplot() +
            # exons
            geom_segment(
                data = annotation_df_list$exons,
                mapping = aes(
                    x = .data$start,
                    y = .data$dodge,
                    xend = .data$end,
                    yend = .data$dodge,
                    color = .data$strand
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
                    color = .data$strand
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
                    color = .data$strand
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
                    color = .data$strand
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
        y_limit <- c(0.9, n_stack + 0.4)
    }
    p <- p +
        theme_classic() +
        ylab("Genes") +
        xlab(label = paste0(seqnames(region), " position (bp)")) +
        xlim(start.pos, end.pos) +
        ylim(y_limit) +
        theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
        ) +
        scale_color_manual(values = c("darkblue", "darkgreen"))
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

reformat_annotations <- function(
        annotation,
        start.pos,
        end.pos,
        collapse_transcript = TRUE
) {
    total.width <- end.pos - start.pos
    tick.freq <- total.width / 50
    annotation <- unname(annotation[annotation$type == "exon"])
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
