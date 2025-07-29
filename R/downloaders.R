outputFileName <- function(ext, ...) {
    paste0(paste(
        ...,
        format(Sys.time(), "%Y%m%d.%M%S"),
        sep = "_"), ".", tolower(ext))
}
#' @noRd
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler
plotsDownloadHandler <- function(input, postfix, plot, ...) {
    downloadHandler(
        filename = function() {
            outputFileName(input[[paste0("GeneExproup.fmt", postfix)]], ...)
        },
        content = function(file) {
            if (input[[paste0("GeneExproup.fmt", postfix)]] == "PDF") {
                ggsave(
                    file,
                    device = 
                        tolower(input[[paste0("GeneExproup.fmt", postfix)]]),
                    height = input[[paste0("GeneExproup.h", postfix)]],
                    width = input[[paste0("GeneExproup.w", postfix)]],
                    useDingbats = FALSE,
                    plot = plot()
                )
            } else if(input[[paste0("GeneExproup.fmt", postfix)]] == "CSV"){
                plot <- plot()
                write.csv(plot$data, file = file)
            } else{
                ggsave(
                    file,
                    device =
                        tolower(input[[paste0("GeneExproup.fmt", postfix)]]),
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
    function(input, postfix, plot, ...) {
        downloadHandler(
            filename = function() {
                outputFileName(input[[paste0("GeneExproup.fmt", postfix)]], ...)
            },
            content = function(file) {
                if(input[[paste0("GeneExproup.fmt", postfix)]] == "CSV"){
                    plot <- plot()
                    env <- environment(
                        plot@ht_list$expression@matrix_param$layer_fun)
                    if('ggProp' %in% names(env)){
                        dat <- cbind(env$ggProp, env$ggMat)
                        colnames(dat) <- c(
                            paste(colnames(env$ggProp), 'prop',
                                  sep='.'),
                            paste(colnames(env$ggMat), 'rescaled.expr',
                                  sep='.'))
                    }else{
                        dat <- plot@ht_list$expression@matrix
                    }
                    write.csv(dat, file = file)
                }else{
                    if (input[[paste0("GeneExproup.fmt", postfix)]] == "PDF") {
                        pdf(
                            file,
                            height = input[[paste0("GeneExproup.h", postfix)]],
                            width = input[[paste0("GeneExproup.w", postfix)]],
                            useDingbats = FALSE
                        )
                    } else{
                        get(tolower(input[[paste0("GeneExproup.fmt", postfix)]]))(
                            file,
                            height = 
                                input[[paste0("GeneExproup.h",
                                              postfix)]] * 300,
                            width = 
                                input[[paste0("GeneExproup.w",
                                              postfix)]] * 300,
                            res = 300)
                    }
                    on.exit({
                        dev.off()
                    })
                    draw(plot()) ## for complexheatmap
                }
            }
        )
    }
#' @importFrom plotly event_data
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom utils write.csv
click_event_data <- function(session = shiny::getDefaultReactiveDomain(), ...) {
    if('plotly_click' %in% session$userData$plotlyShinyEventIDs){
        event_data(event = "plotly_click", ...)
    }else{
        return(NULL)
    }
}
lasso_select_data <- function(session = shiny::getDefaultReactiveDomain(), ...) {
    if('plotly_click' %in% session$userData$plotlyShinyEventIDs){
        event_data(event = "plotly_selected", ...)
    }else{
        return(NULL)
    }
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