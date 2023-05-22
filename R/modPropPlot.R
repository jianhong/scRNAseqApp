plotProportionUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Proportion plot"),
        h4("Proportion / cell numbers across different cell information"),
        "In this tab, users can visualise the composition of single cells ",
        "based on one discrete ",
        "cell information across another discrete cell information. ",
        "Usage examples include the library ",
        "or cellcycle composition across clusters.",
        br(),
        br(),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                xaxisCellInfoUI(id),
                subsetCellByInfoUI(id, mini = TRUE),
                selectInput(
                    NS(id, "CellInfoY"),
                    "Cell information to group / colour by:",
                    choices = NULL
                ) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "Cell information to group / colour cells by",
                        content = c(
                            "Select categorical cell information to group / ",
                            "colour cells by",
                            "- Proportion / cell numbers are shown ",
                            "in different colours"
                        )
                    ),
                radioButtons(
                    NS(id, "plottyp"),
                    "Plot value:",
                    choices = c("Proportion", "CellNumbers"),
                    selected = "Proportion",
                    inline = TRUE
                ),
                checkboxInput(
                    NS(id, "plotflp"),
                    "Flip X/Y", value = FALSE),
                checkboxInput(
                    NS(id, "plotord"),
                    "Reorder the contents", value = FALSE
                ),
                conditionalPanel(
                    condition = "input.plotord % 2 == 1",
                    ns=NS(id),
                    uiOutput(outputId = NS(id, "plotXord")),
                    uiOutput(outputId = NS(id, "plotYord"))
                ),
                boxPlotControlUI(id, withPoints = FALSE)
            ),
            column(
                9,
                geneExprDotPlotUI(id),
                br(),
                actionButton(NS(id, "statstog"), "Toggle to show statistics"),
                conditionalPanel(
                    condition = "input.statstog % 2 == 1",
                    ns = NS(id),
                    h4("Statistics"),
                    dataTableOutput(NS(id, "proportion.dt"))
                )
            )
        )
    )
}
#' @importFrom DT datatable
#' @importFrom sortable rank_list
plotProportionServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## input column
        updateSelectInput(
            session,
            "CellInfoX",
            "Cell information (X-axis):",
            choices = getGroupUI(dataSource),
            selected = dataSource()$sc1def$grp2
        )
        
        updateSubsetCellUI(id, input, output, session, dataSource, addNA = TRUE)
        
        updateSelectInput(
            session,
            "CellInfoY",
            "Cell information to group / colour by:",
            choices = getGroupUI(dataSource),
            selected = dataSource()$sc1def$grp1
        )
        
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotXord",
            NS(id, "cellinfoXorder"))
        updateRankList(
            input, output, dataSource, "CellInfoY", "plotYord",
            NS(id, "cellinfoYorder"))
        
        ## plot region
        ### plots
        plot1 <- reactive({
            scProp(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$CellInfoX,
                input$CellInfoY,
                input$subsetCell,
                getSubsetCellVal(input),
                input$plottyp,
                input$plotflp,
                input$plotfsz,
                reorder = input$plotord,
                orderX = input$cellinfoXorder,
                orderY = input$cellinfoYorder
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
            input$CellInfoY
        )
        
        output$proportion.dt <- renderDataTable({
            datatable(
                plot1()$data,
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
