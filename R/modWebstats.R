#' @importFrom DT DTOutput
webstatsUI <- function (id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 10, offset = 1,
      actionButton(ns('theme'), "accept current theme"),
      fluidRow(
        column(width = 3, DTOutput(ns("summary"))),
        column(width = 9, plotOutput(ns("distPlot"), height = "300px"))
      ),
      DTOutput(ns("counter"))
    )
  )
}
#' @importFrom DT renderDT JS
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom bslib bs_current_theme
#' @importFrom jsonlite parse_json
#' @importFrom data.table as.data.table melt .SD
#' @importFrom ggplot2 geom_bar position_dodge geom_text aes labs theme_minimal
webstatsServer <- function(id, root_session) {
  moduleServer(id, function(input, output, session){
    ## theme
    #bs_themer1(root_session)
    observeEvent(input$theme, {
      .globals$theme <- bs_current_theme(session = root_session)
    })

    ## stats
    cache <- "www/cache.rds"

    freegeoip <- function(ip) {
      if(file.exists(cache)){
        ip_rds <- readRDS(cache)
      }else{
        ip_rds <- list()
      }
      ip_ <- ip_rds[ip[ip %in% names(ip_rds)]]
      ip <- ip[!ip %in% names(ip_rds)]
      ip_rds_addition <- lapply(ip, function(.ip){
        url <- paste0("http://ip-api.com/json/", .ip)
        parse_json(readLines(url, warn=FALSE))
      })
      names(ip_rds_addition) <- ip
      ip_rds <- c(ip_rds, ip_rds_addition)
      saveRDS(ip_rds, file=cache)
      ret <- c(ip_, ip_rds_addition)
      return(ret)
    }
    data_1 <- reactiveValues()
    observe({
      data_record <- reactiveFileReader(intervalMillis = 10000,
                                        session,
                                        filePath = .globals$counterFilename,
                                        readFunc = read.delim)
      data_1$df <- reactive({
        req(data_record())
        data_record()
      })
    })
    data <- reactive(data_1$df())

    ips <- reactive({
      req(data())
      dat <- isolate(data())
      d <- unique(dat$ip)
      d <- d[!is.na(d)]
      d <- d[!is.null(d)]
      ip_info <- freegeoip(d)
      coln <- c("status", "country", "countryCode",
                "region", "regionName", "city",
                "zip", "lat", "lon", "timezone",
                "isp", "org", "as", "query")
      ip_info <- lapply(ip_info, function(.ele){
        unlist(.ele)[coln]
      })
      ip_info <- do.call(rbind, ip_info)
      colnames(ip_info) <- coln
      merge(dat, ip_info, by.x="ip", by.y="query", all.x=TRUE)
    })
    summaryTable <- reactive({
      req(ips())
      tab <- isolate(ips()[, c("ip", "date")])
      tab[, "date"] <- as.Date(tab[, "date"])
      tab[, "month"] <- paste(format(tab$date, "%Y"), months(tab$date), sep="-")
      tab <- as.data.table(tab)
      dat <- tab[, {
        list(
          total = length(.SD$ip),
          uniqueIP=length(unique(.SD$ip))
        )
      }, by = 'month']
      dat <- melt(dat, id="month")
    })
    output$distPlot <- renderPlot({
      req(summaryTable)
      dat <- summaryTable()
      ggplot(dat, aes(x=dat$month, y=dat$value, fill=dat$variable)) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=dat$value), vjust=1.6, color="black",
                  position=position_dodge(1/12)) +
        #scale_fill_brewer(palette="Paired") +
        labs(x="", y="conts", fill = "Legend") +
        theme_minimal()
    })
    output$summary <- renderDT({
      req(summaryTable)
      dat <- as.data.table(summaryTable())
      dat <- dat[, { list(total=sum(.SD$value)) }, by='variable']
      levels(dat$variable) <- c(levels(dat$variable), "database counts")
      dat <- rbind(dat, list("database counts", length(getDataSets())))
      dat
    }, options=list(dom ='t'), rownames=FALSE)
    output$counter <- renderDT({
      req(ips())
      ips()
    },
    extensions = 'Buttons',
    options = list(
      dom = 'Brtip',
      lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
      buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
}
