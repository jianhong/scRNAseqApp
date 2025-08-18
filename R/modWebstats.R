#' @importFrom DT DTOutput
webstatsUI <- function (id) {
    ns <- NS(id)
    fluidRow(column(
        width = 10,
        offset = 1,
        textAreaInput(
            ns('description'),
            label = 'Homepage description',
            value = paste(readLines(file.path(
                .globals$app_path, "doc.txt")), collapse = "\n"),
            width="100%"
        ),
        actionButton(ns('update'), "update web description"),
        actionButton(ns('restart'), 'force restart the App'),
        fluidRow(column(width = 3, DTOutput(ns(
            "summary"
        ))),
        column(
            width = 9, plotOutput(ns("distPlot"), height = "300px")
        )),
        DTOutput(ns("issues")),
        DTOutput(ns("counter"))
    ))
}
#' @importFrom DT renderDT JS
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom jsonlite parse_json
#' @importFrom data.table as.data.table .SD
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_bar position_dodge geom_text aes labs theme_minimal
webstatsServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ## web description
        observeEvent(input$update, {
            writeLines(
                input$description,
                file.path(.globals$app_path, "doc.txt"))
        })
        
        ## force restart App
        observeEvent(input$restart, {
            writeLines(
                date(),
                file.path(.globals$app_path, "restart.txt")
            )
            adminMsg("reloading the session now!", type = "error")
            Sys.sleep(2)
            session$reload()
        })
        
        ## stats
        cache <- file.path(.globals$app_path, "www", "cache.rds")
        
        freegeoip <- function(ip) {
            if (file.exists(cache)) {
                ip_rds <- readRDS(cache)
            } else{
                ip_rds <- list()
            }
            ip_ <- ip_rds[ip[ip %in% names(ip_rds)]]
            ip <- ip[!ip %in% names(ip_rds)]
            ip_rds_addition <- lapply(ip, function(.ip) {
                url <- paste0("http://ip-api.com/json/", .ip)
                json <- tryCatch({
                    readLines(url, warn = FALSE)
                }, error = function(.e){
                    message(.e)
                    NULL
                })
                if(!is.null(json)){
                    parse_json(json)
                }else{
                    NULL
                }
            })
            names(ip_rds_addition) <- ip
            ip_rds_addition <- ip_rds_addition[!is.null(ip_rds_addition)]
            ip_rds <- c(ip_rds, ip_rds_addition)
            saveRDS(ip_rds, file = cache)
            ret <- c(ip_, ip_rds_addition)
            return(ret)
        }
        data_1 <- reactiveValues()
        observe({
            data_1$df <- reactive(({
                listVisitors()
            }))
        })
        data <- reactive(data_1$df())
        
        ips <- reactive({
            req(data())
            dat <- isolate(data())
            d <- unique(dat$ip)
            d <- d[!is.na(d)]
            d <- d[!is.null(d)]
            ip_info <- freegeoip(d)
            coln <- c(
                "status",
                "country",
                "countryCode",
                "region",
                "regionName",
                "city",
                "zip",
                "lat",
                "lon",
                "timezone",
                "isp",
                "org",
                "as",
                "query"
            )
            ip_info <- lapply(ip_info, function(.ele) {
                unlist(.ele)[coln]
            })
            ip_info <- do.call(rbind, ip_info)
            colnames(ip_info) <- coln
            merge(
                dat,
                ip_info,
                by.x = "ip",
                by.y = "query",
                all.x = TRUE
            )
        })
        summaryTable <- reactive({
            if(!tableExists(.globals$counterTableName)){
                req(ips())
                tab <- isolate(ips()[, c("ip", "date")])
                tab[, "date"] <- as.Date(tab[, "date"])
                tab[, "month"] <-
                    paste(format(tab$date, "%Y"), months(tab$date), sep = "-")
                tab <- as.data.table(tab)
                dat <- tab[, {
                    list(
                        total = length(.SD$ip),
                        uniqueIP = length(unique(.SD$ip)))
                }, by = 'month']
                dat <- reshape2::melt(dat, id.vars = "month")
            }else{
                dat = listVisitors(summary = TRUE)
                colnames(dat) <- c('month', 'total', 'uniqueIP')
                dat <- reshape2::melt(dat, id.vars = "month")
            }
        })
        output$distPlot <- renderPlot({
            req(summaryTable)
            dat <- summaryTable()
            ggplot(
                dat,
                aes(
                    x = .data[["month"]],
                    y = .data[["value"]],
                    fill = .data[["variable"]]
                )) +
                geom_bar(stat = "identity", position = position_dodge()) +
                geom_text(
                    aes(label = .data[["value"]]),
                    vjust = 1.6,
                    color = "black",
                    position = position_dodge(1 / 12)
                ) +
                #scale_fill_brewer(palette="Paired") +
                labs(
                    x = "",
                    y = "conts",
                    fill = "Legend") +
                theme_minimal()
        })
        output$summary <- renderDT({
            req(summaryTable)
            dat <- as.data.table(summaryTable())
            dat <- dat[, {
                list(total = sum(.SD$value))
            }, by = 'variable']
            levels(dat$variable) <-
                c(levels(dat$variable), "database counts")
            dat <-
                rbind(
                    dat,
                    list(
                        "database counts",
                        length(getDataSets(
                            privilege = "all"))))
            dat
        }, options = list(dom = 't'), rownames = FALSE)
        output$counter <- renderDT({
            if(!tableExists(.globals$counterTableName)){
                req(ips())
                ips()
            }else{
                listVisitors()
            }
        },
        extensions = 'Buttons',
        options = list(
            dom = 'Brtip',
            lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
            buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print')
        ))
        updateCommentsTable <- function(){
            output$issues <- renderDT({
                data_to_display <- 
                    listComments(page_size=.globals$totalComments,
                                 full=TRUE, all=TRUE)
                if(nrow(data_to_display)<1){
                    data_to_display
                }else{
                    # Add a column for delete buttons
                    # The inputId of each button will be "delete_button_ROWID"
                    delete_buttons <- paste0(
                        '<button id="delete_button_', data_to_display$id, '" ',
                        'type="button" class="btn btn-danger btn-sm delete-btn" ',
                        'onclick="Shiny.onInputChange(\'', NS(id, "delete_clicked"),
                        '\', this.id)"',
                        '>Delete</button>'
                    )
                    # Add the delete buttons as a new column to the data
                    cbind(data_to_display, Actions = delete_buttons)
                }
            },
            editable = list(target='cell', disable=list(columns = c(1, 10))), # 10 is the delete button
            escape = FALSE, # IMPORTANT: Allow HTML rendering for buttons
            extensions = 'Buttons',
            options = list(
                dom = 'Brtip',
                lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
                buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print')
            ))
        }
        updateCommentsTable()
        observeEvent(input$issues_cell_edit, {
            row_idx  <- input$issues_cell_edit$row
            col_idx <- input$issues_cell_edit$col
            new_value <- input$issues_cell_edit$value
            original_data <- listComments(page_size=.globals$totalComments,
                                          full=TRUE, all=TRUE)
            id_to_update <- original_data[row_idx, "id"]
            col_name_to_update <- colnames(original_data)[col_idx]
            updateComments(id=id_to_update,
                           coln=col_name_to_update,
                           val=new_value)
        })
        # Observe delete button clicks
        observeEvent(input$delete_clicked, {
            showModal(
                modalDialog(
                    title = "Confirm deletion",
                    "Are you sure you want to delete this data?",
                    footer = tagList(
                        actionButton(NS(id, "confirmDelBtn"), "Confirm"), # This is the "Yes" button
                        modalButton("Cancel") # This is the "No" button
                    )
                )
            )
        })
        observeEvent(input$confirmDelBtn, {
            removeModal()
            # Extract the row ID from the button's inputId
            # The inputId is "delete_button_ROWID"
            button_id <- input$delete_clicked
            id_to_delete <- as.numeric(gsub("delete_button_", "", button_id))
            deleteComments(id=id_to_delete)
            # refresh table
            updateCommentsTable()
        })
    })
}
