#' Stream plot (ggstream-style) using data.table + geom_ribbon
#' @noRd
#' @param data A data.frame or data.table
#' @param x Column name for x-axis (numeric, e.g. time)
#' @param y Column name for y-axis values (magnitude per group)
#' @param group Column name for stream groups (filled ribbons)
#' @param type "mirror" (symmetric, classic stream), "ridge" (zero baseline), 
#'                  "proportional" (fills to 1)
#' @param bw Bandwidth for loess smoothing (0–1, default 0.3)
#' @param n_grid Number of x points to interpolate over (default 100)
#' @param alpha Fill transparency (default 0.85)
#' @param palette Named/unnamed color vector (optional)
#' @importFrom data.table setnames setorderv setDT
#' @importFrom ggplot2 geom_ribbon
#' @importFrom stats loess predict setNames
#' 
stream_plot <- function(data, x, y, group, groupY,
                        type = "mirror",
                        bw = 0.3, n_grid = 100,
                        alpha = 0.85, palette = NULL) {
    type <- match.arg(type, c('mirror', 'ridge', 'proportional'))
    dt <- as.data.table(data)
    # smooth each group over a common x grid via loess
    x_grid <- seq(min(dt[[x]], na.rm = TRUE),
                  max(dt[[x]], na.rm = TRUE),
                  length.out = n_grid)
    
    groups <- unique(dt[[group]])
    groupsY <- unique(dt[[groupY]])
    gps <- dt[[group]]
    gpY <- dt[[groupY]]
    smooth_dt <- rbindlist(unlist(lapply(groups, function(g){
        lapply(groupsY, function(gY){
            sub <- subset(dt, gps == g & gpY == gY)
            fit <- loess(as.formula(paste(y, "~", x)), data = sub, span = bw)
            pred <- pmax(predict(fit, newdata = data.frame(setNames(list(x_grid), x))), 0)
            data.table(x_val = x_grid, value = pred, grp = g, grpY = gY)
        })
    }), recursive = FALSE))
    setnames(smooth_dt, "grp", group)
    setnames(smooth_dt, "grpY", groupY)
    
    # order groups by total magnitude (biggest in back)
    group_order <- smooth_dt[, list(total = sum(.SD$value, na.rm = TRUE)), by = group]
    setorderv(group_order, 'total', -1)
    smooth_dt[[group]] <- factor(smooth_dt[[group]], levels = group_order[[group]])
    setorderv(smooth_dt, c(groupY, "x_val", group))
    
    # stack by type
    if (type == "proportional") {
        smooth_dt[, "value" := .SD$value / sum(.SD$value, na.rm = TRUE),
                  by = c("x_val", groupY)]
    }
    setDT(smooth_dt)
    smooth_dt[, "ymax" := cumsum(.SD$value), by = c("x_val", groupY)]
    smooth_dt[, "ymin" := .SD$ymax - .SD$value,  by = c("x_val", groupY)]
    
    if (type == "mirror") {
        # center the stack around zero
        smooth_dt[, "mid" := (max(.SD$ymax) + min(.SD$ymin)) / 2,
                  by = c("x_val", groupY)]
        smooth_dt[, "ymax" := .SD$ymax - .SD$mid]
        smooth_dt[, "ymin" := .SD$ymin - .SD$mid]
    }
    
    # plot
    p <- ggplot(smooth_dt,
                aes(x = .data[["x_val"]],
                    ymin = .data[["ymin"]],
                    ymax = .data[["ymax"]],
                    fill = .data[[group]])) +
        geom_ribbon(alpha = alpha) +
        labs(x = x, y = y, fill = group) +
        theme_ridges() +
        theme(
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank()
        ) +
        facet_grid(rows=as.formula(paste(groupY, "~ .")), scales='free_x')
    
    if (!is.null(palette)) {
        p <- p + scale_fill_manual(values = palette)
    }else{
        p <- p + scale_fill_manual(values = 
                                       scColorRampPalette(length(groups),
                                                               'paired_darken'))
    }
    
    p
}