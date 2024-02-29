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
                actionButton(NS(id, "statstog"), "Toggle to show Chi-squared statistics"),
                conditionalPanel(
                    condition = "input.statstog % 2 == 1",
                    ns = NS(id),
                    DTOutput(NS(id, "proportion.dt"))
                ),
                actionButton(NS(id, "pearsontog"),
                             "Toggle to show correlation plot"),
                conditionalPanel(
                    condition = "input.pearsontog % 2 == 1",
                    ns = NS(id),
                    radioButtons(
                        NS(id, "testby"),
                        "Group by:",
                        choices = c("X", "Y"),
                        selected = "Y",
                        inline = TRUE
                    ),
                    radioButtons(
                        NS(id, "testmethod"),
                        "Method:",
                        choices = c("spearman", "kendall", "pearson"),
                        selected = "spearman",
                        inline = TRUE
                    ),
                    radioButtons(
                        NS(id, "cortyp"),
                        "Method:",
                        choices = c("heatmap", "PCA"),
                        selected = "heatmap",
                        inline = TRUE
                    ),
                    radioButtons(
                        NS(id, "plotcols"), "Colour scheme:",
                        choices = names(.globals$cList),
                        selected = names(.globals$cList)[2]),
                    geneExprDotPlotUI(id, postfix=2)
                )
            )
        )
    )
}
#' @importFrom DT datatable
#' @importFrom sortable rank_list
#' @importFrom data.table dcast.data.table
#' @importFrom stats chisq.test
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
        
        getProportion <- function(plot1){
            proportion <- plot1()$data
            proportion <- dcast.data.table(proportion, as.formula('X~grp'),
                                           value.var = 'nCells')
            proportion[is.na(proportion)] <- 0
            proportion <- as.data.frame(proportion)
            rownames(proportion) <- proportion$X
            proportion$X <- NULL
            proportion
        }
        output$proportion.dt <- renderDT({
            proportion <- getProportion(plot1)
            withCallingHandlers({
                chisq <- chisq.test(proportion)
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
                chisq <- list('observed'=data.frame(nodata=NA),
                              'expected'=data.frame(nodata=NA),
                              'residuals'=data.frame(nodata=NA),
                              'p.value'=NA)
            })
            
            chi_out <- lapply(c('observed', 'expected', 'residuals'),
                              function(.ele){
                                  colnames(chisq[[.ele]]) <-
                                      paste(.ele, colnames(chisq[[.ele]]),
                                            sep='.')
                                  chisq[[.ele]]
                              })
            chi_out <- do.call(cbind, chi_out)
            chi_out <- as.data.frame(chi_out)
            chi_out <- cbind(chisq.test.p.value=
                                 c(chisq$p.value,
                                   rep(NA, nrow(chi_out)-1)),
                             chi_out)
            datatable(
                chi_out,
                rownames = TRUE,
                extensions = "Buttons",
                options = list(
                    pageLength = -1,
                    dom = "tB",
                    buttons = c("copy", "csv", "excel")
                )
            )
        })
        
        plot2 <- reactive({
            scCorProp(
                getProportion(plot1),
                testby = input$testby,
                method = input$testmethod,
                type = input$cortyp,
                labelsFontsize = input$plotfsz,
                inpcols = input$plotcols
            )
        })
        updateGeneExprDotPlotUI(
            postfix = 2,
            id,
            input,
            output,
            session,
            plot2,
            .globals$pList2[input$plotpsz],
            dataSource()$dataset,
            input$plottyp,
            input$CellInfoX,
            input$CellInfoY
        )
    })
}
