#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
geneScoreVsExprUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Gene Score/Expr 2D"),
        tabsubTitleUI(
            id,
            'ScoreExpr2D',
            description = paste(
                "In this tab, users can visualise the gene score vs expression ",
                "on low-dimensional representions."
            )
        ),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(5, subsetCellByInfoUI(id)),
            column(4, graphicsControlUI(id))
        ),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                htmlOutput(NS0(id, "subPlotTitle", 1)),
                geneExprUI(id, 1, title='Score name'),
                geneExprUI(id, 2),
                geneCoExprPlotControlUI(id, 1)
            ),
            column(
                9,
                fluidRow(
                    column(
                        8, style = "border-right: 2px solid black",
                        geneExprDotPlotUI(id, 1)),
                    column(
                        4,
                        geneExprDotPlotUI(id, 2))
                ))
        )
    )
}
#' @importFrom DT renderDT
#' @importFrom magrittr %>%
#' @importFrom plotly plotlyOutput renderPlotly
geneScoreVsExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## subtitle
        output$ScoreExpr2DSubTitle <-
            renderUI({
                h4(paste(
                    "Gene Score",
                    "vs gene",
                    dataSource()$terms['expression'],
                    "of genes on reduced dimensions"
                ))
            })
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, input, dataSource)
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        
        ## plot region
        ### sub region title
        output$subPlotTitle1 <-
            renderUI({
                h4(paste("Gene Score vs.", dataSource()$terms['expression']))
            })
        ### dropdown list
        sharedgene <- intersect(names(dataSource()$sc1gene),
                                names(dataSource()$sc1gsgene))
        if(any(dataSource()$sc1def$gene %in% sharedgene)){
            selected_sharedgene <- dataSource()$sc1def$gene[
                dataSource()$sc1def$gene %in% sharedgene][1]
        }else{
            selected_sharedgene <- sharedgene[1]
        }
        updateSelectizeInput(
            session,
            "GeneName1",
            choices = sort(sharedgene),
            server = TRUE,
            selected = selected_sharedgene,
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        updateSelectizeInput(
            session,
            "GeneName2",
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = selected_sharedgene,
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        ### plots
        plot1 <- reactive({
            scDRcoex(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                gene1=input$GeneName2,
                gene2=input$GeneName1,## gene score
                subsetCellKey=input$subsetCell,
                subsetCellVal=getSubsetCellVal(input),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                geneIdMap2=dataSource()$sc1gsgene,
                geneType='score',
                plotType=input[[paste0('CoExprtype', 1)]],
                pointSize=input$GeneExprsiz,
                GeneExprDotCol=input$CoExprcol1,
                GeneExprDotOrd=input$CoExprord1,
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                hideFilterCell=input$CoExprhid1,
                inpCellBorder=input$CoExprSegmentation1,
                cellborderFilename=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["cellborder"]]),
                cellSegAlpha = input$CoExprSegAlpha1,
                cellSegColor = ifelse(
                    input$CoExprSegBorderColor1,
                    input$CoExprSegColor1,
                    NA),
                inpBgImg=input$CoExprBgImg1,
                backgroundImage=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["backgroundImage"]]),
                inpXlim=if (input[[paste0("manuXYlimTog", 1)]] %% 2 == 0)
                            0
                        else
                            input[[paste0("manuXlim", 1)]],
                useNorm=input[[paste0('useNorm', 1)]],
                streamType=input[[paste0('CoExprStreamtype', 1)]]
            )
        })
        
        updateGeneExprDotPlotUI(
            postfix = 1,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plot1,
            height = .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input$GeneName1,
            input$GeneName2,
            dataSource = dataSource
        )
        
        plot2 <- reactive({
            scDRcoexLeg(input$GeneName1,
                        input$GeneName2,
                        colorPairs = input$CoExprcol1,
                        labelsFontsize = input$GeneExprfsz,
                        labelsFontFamily=input$GeneExprfml,
                        geneType='score')
        })
        updateGeneExprDotPlotUI(
            postfix = 2,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plot2,
            height = 300,
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input$GeneName1,
            input$GeneName2,
            dataSource = dataSource
        )
    })
}
