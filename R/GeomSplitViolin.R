# A split violin plot geom
#
#' @importFrom scales zero_range
#' @importFrom ggplot2 GeomPolygon ggproto GeomViolin
#' @importFrom grid grobTree grobName
#
# @author jan-glx on StackOverflow
# @references \url{https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2}
# @seealso \code{\link[ggplot2]{geom_violin}}
#
GeomSplitViolin <- ggproto(
    "GeomSplitViolin",
    GeomViolin,
    # setup_data = function(data, params) {
    #   data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
    #   data <- plyr::ddply(data, "group", transform, xmin = x - width/2, xmax = x + width/2)
    #   e <- globalenv()
    #   name <- paste(sample(x = letters, size = 5), collapse = '')
    #   message("Saving initial data to ", name)
    #   e[[name]] <- data
    #   return(data)
    # },
    draw_group = function(self, data, ..., draw_quantiles = NULL) {
        data$xminv <- data$x - data$violinwidth * (data$x - data$xmin)
        data$xmaxv <- data$x + data$violinwidth * (data$xmax - data$x)
        grp <- data[1, 'group']
        if (grp %% 2 == 1) {
            data$x <- data$xminv
            data.order <- data$y
        } else {
            data$x <- data$xmaxv
            data.order <- -data$y
        }
        newdata <- data[order(data.order), , drop = FALSE]
        newdata <- rbind(
            newdata[1, ],
            newdata,
            newdata[nrow(x = newdata), ],
            newdata[1, ]
        )
        newdata[c(1, nrow(x = newdata) - 1, nrow(x = newdata)), 'x'] <- round(x = newdata[1, 'x'])
        grob <- if (length(x = draw_quantiles) > 0 & !zero_range(x = range(data$y))) {
            stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
            quantiles <- QuantileSegments(data = data, draw.quantiles = draw_quantiles)
            aesthetics <- data[rep.int(x = 1, times = nrow(x = quantiles)), setdiff(x = names(x = data), y = c("x", "y")), drop = FALSE]
            aesthetics$alpha <- rep.int(x = 1, nrow(x = quantiles))
            both <- cbind(quantiles, aesthetics)
            quantile.grob <- GeomPath$draw_panel(both, ...)
            grobTree(GeomPolygon$draw_panel(newdata, ...), name = quantile.grob)
        }
        else {
            GeomPolygon$draw_panel(newdata, ...)
        }
        grob$name <- grobName(grob = grob, prefix = 'geom_split_violin')
        return(grob)
    }
)

# Create a split violin plot geom
#
# @inheritParams ggplot2::geom_violin
#
#' @importFrom ggplot2 layer
#
# @author jan-glx on StackOverflow
# @references \url{https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2}
# @seealso \code{\link[ggplot2]{geom_violin}}
#
geom_split_violin <- function(
        mapping = NULL,
        data = NULL,
        stat = 'ydensity',
        position = 'identity',
        ...,
        draw_quantiles = NULL,
        trim = TRUE,
        scale = 'area',
        na.rm = FALSE,
        show.legend = NA,
        inherit.aes = TRUE
) {
    return(layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSplitViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            trim = trim,
            scale = scale,
            draw_quantiles = draw_quantiles,
            na.rm = na.rm,
            ...
        )
    ))
}
