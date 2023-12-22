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
            tagList(
                h4(sub('^\\/', '', fd)),
                do.call(fluidRow,
                        lapply(fn, function(.fn){
                            downloadLink(NS(id, make.names(.fn)),
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
            for(i in seq_along(files)){
                for(j in seq_along(files[[i]])){
                    output[[make.names(files[[i]][j])]] <- downloadHandler(
                        filename = basename(files[[i]][j]),
                        content = function(file){
                            file.copy(file.path(.globals$app_path,
                                                .globals$downloadFolder,
                                                files[[i]][j]),
                                      file)
                        }
                    )
                }
            }
        }
    })
}
