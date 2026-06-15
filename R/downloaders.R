outputFileName <- function(ext, ...) {
    paste0(paste(
        ...,
        format(Sys.time(), "%Y%m%d.%M%S"),
        sep = "_"), ".", tolower(ext))
}
#' @noRd
#' @importFrom ggplot2 ggsave
#' @importFrom shiny downloadHandler
plotsDownloadHandler <- function(input, postfix, plot, dataSource,
                                 ranges, ...) {
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
                    plot = addLimits(darkTheme(plot(), dataSource = dataSource),
                                     ranges=ranges)
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
                    plot = addLimits(darkTheme(plot(), dataSource = dataSource),
                                     ranges=ranges)
                )
            }
        }
    )
}
#' @importFrom grDevices dev.off pdf
heatmapDownloadHandler <-
    function(input, postfix, plot, dataSource, ...) {
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
    d <- event_data(event = "plotly_click", session = session, ...)
    return(d)
}
lasso_select_data <- function(session = shiny::getDefaultReactiveDomain(), ...) {
     event_data(event = "plotly_selected", session=session, ...)
}
get_lasso_selected_ids <- function(session, plot){
    tryCatch({
        d <- lasso_select_data(session = session)
        raw <- plot()$data
        cn <- c('X', 'Y', 'sampleID')
        if(all(cn %in% colnames(raw))){
            raw <- raw[, .SD, .SDcols=cn]
            additional <- plot()$layers
            additional <- lapply(additional, function(i) {
                i <- i$data
                if(all(cn %in% colnames(i))){
                    i[, .SD, .SDcols=cn]
                }else{
                    NULL
                }
            })
            additional <- do.call(rbind, additional)
            raw <- rbind(raw, additional)
        }
        d <- merge(d, raw, by.x=c('x', 'y'), by.y=c('X', 'Y'))
        return(d)
    }, error=function(e){
        adminMsg(e, type='error', duration=3)
        return(NULL)
    })
}
exprDownloadHandler <- function(geneIdMap, dataset, meta, lasso=FALSE, plot, session) {
    downloadHandler(
        filename = function() {
            paste0('exprdata-', dataset, '.csv')
        },
        content = function(file) {
            if(lasso){
                d <- get_lasso_selected_ids(session, plot)
                write.csv(d, file)
            }else{
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
        }
    )
}

#' @importFrom plotly event_data
plotly3d_click <- function(session, source="A") {
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