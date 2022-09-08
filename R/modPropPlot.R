plotProportionUI <- function(id){
  tabPanel(
    HTML("Proportion plot"),
    h4("Proportion / cell numbers across different cell information"),
    "In this tab, users can visualise the composition of single cells based on one discrete ",
    "cell information across another discrete cell information. ",
    "Usage examples include the library or cellcycle composition across clusters.",
    br(),br(),
    fluidRow(
      column(
        3, style="border-right: 2px solid black",
        xaxisCellInfoUI(id),
        subsetCellByInfoUI(id, mini=TRUE),
        selectInput(NS(id, "cellInfoY"),
                    "Cell information to group / colour by:",
                    choices = NULL) %>%
          shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                              title = "Cell information to group / colour cells by",
                              content = c("Select categorical cell information to group / colour cells by",
                                          "- Proportion / cell numbers are shown in different colours")),
        radioButtons(NS(id, "plottyp"),
                     "Plot value:",
                     choices = c("Proportion", "CellNumbers"),
                     selected = "Proportion", inline = TRUE),
        checkboxInput(NS(id, "plotflp"),
                      "Flip X/Y", value = FALSE),
        boxPlotControlUI(id, withPoints = FALSE)
      ),
      column(
        9, geneExprDotPlotUI(id), br(),
        actionButton(NS(id, "statstog"), "Toggle to show statistics"),
        conditionalPanel(
          condition = "input.statstog % 2 == 1",
          ns=NS(id),
          h4("Statistics"),
          dataTableOutput(NS(id, "proportion.dt"))
        )
      )
    )
  )
}
plotProportionServer <- function(id, dataSource, optCrt, currentdataset){
  moduleServer(id, function(input, output, session){
    ## input column
    updateSelectInput(session,
                      "CellInfoX",
                      "Cell information (X-axis):",
                      choices = dataSource()$sc1conf[grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp2)
    updateSelectInput(session,
                      "subsetCell",
                      "Cell information to subset by:",
                      choices = c("N/A", dataSource()$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")
    updateSelectInput(session,
                      "cellInfoY",
                      "Cell information to group / colour by:",
                      choices = dataSource()$sc1conf[grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp1)
    ## update the ui
    output$subsetCell.ui <- renderUI({
      if(input$subsetCell!="N/A"){
        sub = strsplit(dataSource()$sc1conf[UI == input$subsetCell]$fID, "\\|")[[1]]
        checkboxGroupInput(NS(id, "subsetCellVal"),
                              "Select which cells to show",
                           inline = TRUE,
                           choices = sub,
                           selected = sub)
      }else{
        sub = NULL
      }
    })

    ## plot region
    ### plots
    plot1 <- reactive({
      scProp(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$CellInfoX,
        input$subsetCell,
        input$subsetCellVal,
        input$cellInfoY,
        input$plottyp,
        input$plotflp,
        input$plotfsz)
    })
    output$GeneExproup1 <- renderPlot({ plot1() })
    output$GeneExproup.ui1 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 1),
                 height = pList2[input$plotpsz])
    })
    output$GeneExproup.pdf1 <-
      plotsDownloadHandler(
        "pdf",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX,
        input$cellInfoY)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX,
        input$cellInfoY)
    output$proportion.dt <- renderDataTable({
      datatable(plot1()$data,
                rownames = FALSE,
                extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB",
                               buttons = c("copy", "csv", "excel")))
    })
  })
}

