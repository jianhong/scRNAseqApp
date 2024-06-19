outputFileName <- function(ext, ...) {
    paste0(paste(
        ...,
        format(Sys.time(), "%Y%m%d.%M%S"),
        sep = "_"), ".", ext)
}
#' @noRd
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler
plotsDownloadHandler <- function(device, input, postfix, plot, ...) {
    downloadHandler(
        filename = function() {
            outputFileName(device, ...)
        },
        content = function(file) {
            if (device == "pdf") {
                ggsave(
                    file,
                    device = device,
                    height = input[[paste0("GeneExproup.h", postfix)]],
                    width = input[[paste0("GeneExproup.w", postfix)]],
                    useDingbats = FALSE,
                    plot = plot()
                )
            } else{
                ggsave(
                    file,
                    device = device,
                    height = input[[paste0("GeneExproup.h", postfix)]],
                    width = input[[paste0("GeneExproup.w", postfix)]],
                    plot = plot()
                )
            }
        }
    )
}
#' @importFrom grDevices dev.off pdf
heatmapDownloadHandler <-
    function(device, input, postfix, plot, ...) {
        downloadHandler(
            filename = function() {
                outputFileName(device, ...)
            },
            content = function(file) {
                if (device == "pdf") {
                    pdf(
                        file,
                        height = input[[paste0("GeneExproup.h", postfix)]],
                        width = input[[paste0("GeneExproup.w", postfix)]],
                        useDingbats = FALSE
                    )
                } else{
                    get(device)(file,
                                height = 
                                    input[[paste0("GeneExproup.h",
                                                  postfix)]] * 72,
                                width = 
                                    input[[paste0("GeneExproup.w",
                                                  postfix)]] * 72)
                }
                draw(plot) ## for complexheatmap
                dev.off()
            }
        )
    }
#' @importFrom plotly event_data
#' @importFrom utils write.csv
click_event_data <- function(...) {
    event_data(event = "plotly_click", ...)
}
lasso_select_data <- function(...) {
    event_data(event = "plotly_selected", ...)
}
exprDownloadHandler <- function(geneIdMap, dataset, meta) {
    downloadHandler(
        filename = function() {
            paste0('exprdata-', dataset, '.csv')
        },
        content = function(file) {
            d <- click_event_data()
            expr <- NULL
            if (!is.null(d)) {
                cell <- which(meta$sampleID == d$customdata)
                if (!is.null(cell)) {
                    expr <- read_exprs(
                        h5f = dataset,
                        valueOnly = TRUE,
                        cell = cell)
                    names(expr) <- names(geneIdMap[order(geneIdMap)])
                    expr <- t(t(expr))
                    colnames(expr) <- d$customdata
                }
            }
            write.csv(expr, file)
        }
    )
}

#' @importFrom plotly event_data
plotly3d_click <- function(session) {
    renderPrint({
        d <- click_event_data(session = session)
        if (is.null(d))
            "Click cell appear here"
        else {
            paste("Clicked cell", d$customdata)
        }
    })
}
plotly_selected <- function(session){
    renderPrint({
        d <- lasso_select_data(session = session)
        if (is.null(d))
            "Selected cell appear here"
        else {
            paste("Selected cell", d$customdata)
        }
    })
}