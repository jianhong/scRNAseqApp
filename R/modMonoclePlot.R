plotMonocleUI <- function(id) {
    ns <- NS(id)
    tabPanel(
        value = id,
        HTML("Monocle"),
        h4("Monocle plot cell"),
        "In this tab, users can visualise the cells",
        "along with their trajectories.",
        br(),
        br(),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                selectInput(
                    ns("group"),
                    "select the data group",
                    choices = c(),
                    selected = NULL
                ),
                selectInput(
                    ns("root"),
                    "select the root point",
                    choices = c(),
                    selected = NULL
                ),
                checkboxInput(
                    ns("show_trajectory_graph"),
                    "show trajectory graph",
                    value = TRUE
                ),
                checkboxInput(
                    ns("label_principal_points"),
                    "label principal points",
                    value = TRUE
                ),
                checkboxInput(
                    ns("label_leaves"),
                    "label leaves",
                    value = TRUE),
                checkboxInput(
                    ns("label_roots"),
                    "label roots",
                    value = TRUE),
                boxPlotControlUI(id, withPoints = FALSE, withColor = FALSE)
            ),
            column(9, h4(htmlOutput(
                NS(id, "oupTxt")
            )), geneExprDotPlotUI(id))
        )
    )
}
plotMonocleServer <- function(id, dataSource, optCrt) {
    if (checkMisc("monocle", dataSource()$dataset)) {
        monocle <- readData("monocle", dataSource()$dataset)
        reduction_method <- monocle$reduction_method
        if (is.null(reduction_method))
            reduction_method <- "UMAP"
        monocle$reduction_method <- NULL
    } else{
        monocle <- NULL
    }
    moduleServer(id, function(input, output, session) {
        updateSelectInput(
            session,
            "group",
            choices = c(),
            selected = NULL)
        updateSelectInput(
            session,
            "root",
            choices = c(),
            selected = NULL)
        has_monocle <- FALSE
        if (length(monocle)) {
            has_monocle <- TRUE
            monocle <- lapply(monocle, function(.ele) {
                n <- names(.ele)
                if (is.null(n)) {
                    names(.ele) <-  paste0("node", seq_along(.ele))
                }
                .ele
            })
            ## input column
            updateSelectInput(
                session,
                "group",
                choices = names(monocle),
                selected = names(monocle)[1]
            )
            
            observeEvent(input$group, {
                if (!is.null(monocle[[input$group]])) {
                    n <- names(monocle[[input$group]])
                    
                    updateSelectInput(
                        session,
                        "root",
                        choices = n,
                        selected = n[1])
                }
            })
            ## plot region
            ### plots
            plot1 <- reactive({
                scDRmonocle(
                    data = monocle[[input$group]][[input$root]],
                    reduction_method = reduction_method,
                    alpha = 1,
                    cell_size = .globals$fList[input$plotpsz],
                    cell_stroke = .globals$fList[input$plotpsz] / 4,
                    trajectory_graph_segment_size = .75,
                    trajectory_graph_color = 'grey28',
                    graph_label_size = input$plotfsz / 6,
                    font_size = input$plotfsz,
                    labelsFontFamily = input$plotfml,
                    show_trajectory_graph = input$show_trajectory_graph,
                    label_principal_points = input$label_principal_points,
                    label_leaves = input$label_leaves,
                    label_roots = input$label_roots
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
                input$group,
                input$node
            )
        }
        # update the ui
        output$oupTxt <- renderUI({
            if (!has_monocle) {
                HTML("No data available!")
            } else {
                HTML("")
            }
        })
    })
}
