plotVioBoxUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Violinplot / Boxplot"),
        h4("Cell information / gene expression violin plot / box plot"),
        "In this tab, users can visualise the gene expression ",
        "or continuous cell information ",
        "(e.g. Number of UMIs / module score) across groups of cells ",
        "(e.g. libary / clusters).",
        br(),
        br(),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                xaxisCellInfoUI(id),
                yaxisCellInfoUI(id),
                subsetCellByInfoUI(id, mini = TRUE),
                subsetCellByFilterUI(
                    id,
                    label = "Cell Info / Gene name (Y-axis):",
                    title = "Cell Info / Gene to plot",
                    content = c(
                        "Select cell info / gene to plot on Y-axis",
                        "- Can be continuous cell information ",
                        "(e.g. nUMIs / scores)",
                        "- Can also be gene expression"
                    )
                ),
                radioButtons(
                    NS(id, "plottyp"),
                    "Plot type:",
                    choices = c("violin", "boxplot"),
                    selected = "violin",
                    inline = TRUE
                ),
                checkboxInput(
                    NS(id, "plotpts"), "Show data points",
                    value = FALSE),
                checkboxInput(
                    NS(id, 'addnoise'),
                    "Add noise", value = TRUE
                ),
                checkboxInput(
                    NS(id, "plotord"),
                    "Reorder the contents", value = FALSE
                ),
                conditionalPanel(
                    condition = "input.plotord % 2 == 1",
                    ns=NS(id),
                    uiOutput(outputId = NS(id, "plotXord"))
                ),
                conditionalPanel(
                    condition = "input.CellInfoY != ''",
                    ns = NS(id),
                    checkboxInput(
                        NS(id, "plotsord"),
                        "Reorder the spliter", value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.plotsord % 2 == 1",
                        ns=NS(id),
                        uiOutput(outputId = NS(id, "plotSord"))
                    )
                ),
                boxPlotControlUI(id)
            ),
            column(9, geneExprDotPlotUI(id),
                   br(),
                   actionButton(NS(id, "statstog"), "Toggle to show statistics"),
                   conditionalPanel(
                       condition = "input.statstog % 2 == 1",
                       ns = NS(id),
                       h4("Statistics"),
                       DTOutput(NS(id, "violin.dt"))
                   ))
        )
    )
}
#' @importFrom utils combn
#' @importFrom stats wilcox.test
plotVioBoxServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## input column
        updateSelectInput(
            session,
            "CellInfoX",
            "Cell information (X-axis):",
            choices = getGroupUI(dataSource),
            selected = dataSource()$sc1def$grp1
        )
        
        updateSelectInput(
            session,
            "CellInfoY",
            "Split by:",
            choices = c("N/A", getGroupUI(dataSource)),
            selected = NA
        )
        
        updateSubsetCellUI(id, input, output, session, dataSource, addNA = TRUE)
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotXord",
            NS(id, "cellinfoXorder"))
        updateRankList(
            input, output, dataSource, "CellInfoY", "plotSord",
            NS(id, "cellinfoSorder"))
        
        ## plot region
        ### plots
        plot1 <- reactive({
            scVioBox(
                inpConf = dataSource()$sc1conf,
                inpMeta = dataSource()$sc1meta,
                infoX = input$CellInfoX,
                infoY = input$filterCell,
                subsetCellKey = input$subsetCell,
                subsetCellVal = getSubsetCellVal(input),
                valueFilterKey = input$filterCell,
                valueFilterCutoff = input$filterCellVal,
                valueFilterCutoff2 = input$filterCellVal2,
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                inptyp = input$plottyp,
                inppts = input$plotpts,
                pointSize = input$plotsiz,
                labelsFontsize = input$plotfsz,
                labelsFontFamily = input$plotfml,
                reorder = input$plotord,
                orderX = input$cellinfoXorder,
                splitBy=input$CellInfoY,
                sreorder = input$plotsord,
                orderS = input$cellinfoSorder,
                addnoise = input$addnoise
            )
        })
        updateGeneExprDotPlotUI(
            postfix = 1,
            id,
            input,
            output,
            session,
            plot1,
            .globals$pList2[input$plotpsz],
            dataSource()$dataset,
            input$plottyp,
            input$CellInfoX,
            input$filterCell
        )
        output$violin.dt <- renderDT({
            violin <- plot1()$data
            violin <- split(violin$val, violin$X)
            cbn <- combn(names(violin), 2, simplify = FALSE)
            res <- lapply(cbn, function(.ele) {
                withCallingHandlers({
                    wt <- wilcox.test(violin[[.ele[1]]],
                                      violin[[.ele[2]]])
                }, warning = function(w){
                    showNotification(
                        as.character(w),
                        duration = 5,
                        type = 'warning'
                    )
                }, error = function(e){
                    showNotification(
                        as.character(e),
                        duration = 5,
                        type = 'error'
                    )
                    wt <- list('p.value'=NA)
                })
                
                return(as.data.frame(wt[
                    c('alternative',
                      'statistic',
                      'p.value',
                      'method')
                ]))
            })
            
            res <- cbind(do.call(rbind, cbn), do.call(rbind, res))
            colnames(res)[c(1, 2)] <- c('group 1', 'group 2')
            datatable(
                res,
                rownames = FALSE,
                extensions = "Buttons",
                options = list(
                    pageLength = -1,
                    dom = "tB",
                    buttons = c("copy", "csv", "excel")
                )
            )
        })
    })
}
