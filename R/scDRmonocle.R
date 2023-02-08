#' @importFrom ggplot2 geom_text
scDRmonocle <- function(
        data,
        reduction_method = "UMAP",
        alpha = 1,
        cell_size = .35,
        cell_stroke = .1,
        trajectory_graph_segment_size = .75,
        trajectory_graph_color = 'grey28',
        graph_label_size = 2,
        font_size = 'Medium',
        show_trajectory_graph = TRUE,
        label_principal_points = TRUE,
        label_leaves = TRUE,
        label_roots = TRUE) {
    if (is.null(data)) {
        return(NULL)
    }
    data_df <- data$meta_data
    edge_df <- data$segments_layer_data
    princ_point_df <- data$principal_points_data
    leaf_df <- data$mst_leaf_nodes
    root_df <- data$mst_root_nodes
    ggOut <-
        ggplot(data = data_df, aes_string(x = "data_dim_1", y = "data_dim_2")) +
        geom_point(
            color = I("black"),
            size = 1.25 * cell_size,
            stroke = I(cell_stroke),
            na.rm = TRUE,
            alpha = I(alpha)
        ) +
        geom_point(
            aes_string(color = "cell_color"),
            size = I(cell_size),
            stroke = I(cell_stroke),
            na.rm = TRUE,
            alpha = alpha
        ) +
        guides(color = guide_legend(
            title = "cluster",
            override.aes = list(size = 4)))
    if (show_trajectory_graph) {
        ggOut <- ggOut +
            geom_segment(
                aes_string(
                    x = "source_prin_graph_dim_1",
                    y = "source_prin_graph_dim_2",
                    xend = "target_prin_graph_dim_1",
                    yend = "target_prin_graph_dim_2"
                ),
                size = trajectory_graph_segment_size,
                color = I(trajectory_graph_color),
                linetype = "solid",
                na.rm = TRUE,
                data = edge_df
            )
    }
    if (label_principal_points) {
        ggOut <- ggOut +
            geom_point(
                aes_string(x = "prin_graph_dim_1", y = "prin_graph_dim_2"),
                shape = 21,
                stroke = I(trajectory_graph_segment_size),
                color = "white",
                fill = "black",
                size = I(graph_label_size * 1.5),
                na.rm = TRUE,
                princ_point_df
            )
    }
    if (label_leaves) {
        ggOut <- ggOut +
            geom_point(
                aes_string(x = "prin_graph_dim_1", y = "prin_graph_dim_2"),
                shape = 21,
                stroke = I(trajectory_graph_segment_size),
                color = "black",
                fill = "lightgray",
                size = I(graph_label_size * 1.5),
                na.rm = TRUE,
                leaf_df
            ) +
            geom_text(
                aes_string(
                    x = "prin_graph_dim_1",
                    y = "prin_graph_dim_2",
                    label = "leaf_idx"
                ),
                size = I(graph_label_size),
                color = "black",
                na.rm = TRUE,
                leaf_df,
                inherit.aes = FALSE
            )
    }
    if (label_roots) {
        ggOut <- ggOut +
            geom_point(
                aes_string(x = "prin_graph_dim_1", y = "prin_graph_dim_2"),
                shape = 21,
                stroke = I(trajectory_graph_segment_size),
                color = "black",
                fill = "white",
                size = I(graph_label_size * 1.5),
                na.rm = TRUE,
                root_df
            ) +
            geom_text(
                aes_string(
                    x = "prin_graph_dim_1",
                    y = "prin_graph_dim_2",
                    label = "root_idx"
                ),
                size = I(graph_label_size),
                color = "black",
                na.rm = TRUE,
                root_df
            )
    }
    ggOut <- ggOut +
        xlab(paste(reduction_method, "_1")) +
        ylab(paste(reduction_method, "_2")) +
        sctheme(base_size = .globals$sList[font_size])
    return(ggOut)
}
