downloaderUI <- function(id) {
    files <- dir(file.path(
        .globals$app_path,
        .globals$downloadFolder),
                 full.names = TRUE,
                 recursive = TRUE)
    if(length(files)){
        files <- sub(file.path(
            .globals$app_path,
            .globals$downloadFolder), "", files, fixed = TRUE)
        folders <- dirname(files)
        files <- split(files, folders)
        fr <- mapply(files, names(files), FUN=function(fn, fd){
            readme <- file.path(fd, 'readme')
            fluidRow(
                div(strong(sub('^\\/', '', fd)),
                    if(readme %in% fn){
                        includeText(file.path(.globals$app_path,
                                              .globals$downloadFolder,
                                              readme))
                    }),
                div(class='inline-float',
                    lapply(fn, function(.fn){
                        downloadButton(NS(id, make.names(.fn)),
                                       basename(.fn))}))
            )
        }, SIMPLIFY = FALSE)
        tabPanel(
            value = id,
            HTML("Download"),
            h4("Download data"),
            "In this tab, users can view and download data.",
            br(),
            br(),
            fr
        )
    }
}
downloaderServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        files <- dir(file.path(
            .globals$app_path,
            .globals$downloadFolder),
            full.names = TRUE,
            recursive = TRUE)
        if(length(files)){
            files <- sub(file.path(
                .globals$app_path,
                .globals$downloadFolder), "", files, fixed = TRUE)
            folders <- dirname(files)
            files <- split(files, folders)
            observe({
                lapply(seq_along(files), function(i){
                    lapply(seq_along(files[[i]]), function(j){
                        output[[make.names(files[[i]][j])]] <- downloadHandler(
                            filename = basename(files[[i]][j]),
                            content = function(file){
                                file.copy(file.path(.globals$app_path,
                                                    .globals$downloadFolder,
                                                    files[[i]][j]),
                                          file)
                            }
                        )
                    })
                })
            })
        }
    })
}
