## BiocManager::install(c("shinyhelper", "DT", "ggplot2", "ggrepel", "Matrix",
##                        "hdf5r", "ggdendro", "gridExtra", "ggridges"))
library(shiny)
library(shinyhelper)
library(data.table)
library(Matrix)
library(DT)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(hdf5r)
library(ggdendro)
library(gridExtra)
library(ggridges)
VERSION = "2.1.0"
if(names(dev.cur())!= "null device") dev.off()
pdf(NULL)

source("R/lang.R")
source("R/data.R")
defaultDataset <- "GSM5023610_glial_app"
if(!defaultDataset %in% datasets){
  defaultDataset <- datasets[1]
}
source("R/userdata.R")

#library(sysfonts)
#font_paths(file.path(getwd(), "inst/extdata/fonts"))
#font_add(family = "Arial", regular = "Arial.ttf")
#library(showtext)
#showtext.auto()

source("R/define.R")
source("R/g_legend.R")
source("R/sctheme.R")

### Common plotting functions
source("R/scDRcell.R")
source("R/scDRnum.R")
source("R/scDRgene.R")
source("R/scDRcoex.R")
source("R/scDRcoexLeg.R")
source("R/scDRcoexNum.R")
source("R/scVioBox.R")
source("R/scProp.R")
source("R/scGeneList.R")
source("R/scBubbHeat.R")

source("R/tab.R")
ui <- function(req){
  fluidPage(
  ### HTML formatting of error messages
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}")),
            tags$style(HTML(".rightAlign{float:right;}")),
            tags$script(src = "login.js"),
            list(tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }")))
            ),

  ### Page title
  titlePanel(htmlOutput("dataTitle"), windowTitle = "scRNAseq regeneration database"),
  navbarPage(
    NULL,
    id = "topnav",
    footer = div(p(em("scRNAseq/scATACseq Database (Version:", VERSION, ")"),
               HTML("&copy;"), "2020 -",
               format(Sys.Date(), "%Y"),
               "jianhong@duke"), class="rightAlign"),
    ### Tab: cellInfo vs geneExpr on dimRed
    tabcellInfoGeneExpr(),
    ### Tab: cellInfo vs cellInfo on dimRed
    tabcellInfoCellInfo(),
    ### Tab: geneExpr vs geneExpr on dimRed
    tabgeneExprGeneExpr(),
    ### Tab: Gene coexpression plot
    tabscCoExpr(),
    ### Tab: subset gene expr
    tabsubsetGeneExpr(),
    ### Tab: violinplot / boxplot
    tabscVioBoxPlot(),
    ### Tab: Proportion plot
    tabscProportion(),
    ### Tab: Multiple gene expr
    tabscBubbleHeatmap(),
    ### Tab: change dataset
    tabChangeDataset(req),
    ### Tab: Login form
    tabLogin(),
    br(),br(),br(),br(),br()
  ))
}

### Start server code
server <- function(input, output, session) {
  ### For all tags and Server-side selectize
  observe_helpers()
  optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
  dataSource <- reactiveValues(dataset=defaultDataset,
                               geoAcc=NULL,
                               sc1conf=NULL,
                               sc1def=NULL,
                               sc1gene=NULL,
                               sc1meta=NULL,
                               Logged=FALSE,
                               terms=terms[["scRNAseq"]],
                               Username="",
                               Password="",
                               token="")
  observeEvent(input$Login, {
        dataSource$Username <- isolate(input$userName)
        dataSource$Password <- isolate(input$passwd)
        if(!checkLockedDataset(dataSource$dataset)){
          output$loginmsg <- renderText("No need to login yet.")
          updateTabsetPanel(session, "topnav", selected = "ChangeDataset")
        }
        if (checkUserNameAndPassword(dataSource$Username,
                                     dataSource$Password,
                                     dataSource$dataset) ||
            checkToken(token, dataSource$token, dataSource$dataset)) {
          dataSource$Logged <- TRUE
          output$loginmsg <- renderText("Logged!")
          updateTabsetPanel(session, "topnav", selected = "ChangeDataset")
        }else{
          dataSource$Logged <- FALSE
          dataSource$Username <- ""
          dataSource$Password <- ""
          dataSource$token <- ""
          output$loginmsg <- renderText("Wrong Username, Password or Dataset!")
        }
  })

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['data']])){
      updateSelectInput(session, "availableDatasets", selected=query[['data']])
    }
    if(!is.null(query[['token']])){
      if(query[["token"]] %in% names(token)){
        dataSource$token <- query[["token"]]
        if(dataSource$token %in% names(token)){
          updateSelectInput(session,
                            "availableDatasets",
                            selected=token[[query[['token']]]])
        }
      }
    }
  })
  observeEvent(input$availableDatasets,{
    dataSource$dataset <- input$availableDatasets
    dataSource$geoAcc <- sub("^(.*?)_.*$", "\\1", input$availableDatasets)
    if(checkLockedDataset(dataSource$dataset)){
        if(dataSource$Username!="" && dataSource$Password!=""){
          if(checkUserNameAndPassword(dataSource$Username, dataSource$Password, dataSource$dataset)){
            dataSource$Logged <- TRUE
            output$loginmsg <- renderText("Logged!")
          }else{
            updateTabsetPanel(session, "topnav", selected = "Login")
          }
        }else{
          if(dataSource$token!=""){
            if(checkToken(token, dataSource$token, dataSource$dataset)){
              dataSource$Logged <- TRUE
              output$loginmsg <- renderText("Logged!")
            }
          }else{
            updateTabsetPanel(session, "topnav", selected = "Login")
          }
        }
    }
    refreshData(input, output, session)
  })

  update_visitor <- function(){
    req(input$remote_addr)
    counter <- read.delim("www/counter.tsv", header = TRUE)
    ips <- counter$ip
    counter <- as.Date(counter$date)
    visitors <- paste(format(counter, "%d/%m/%y %H"), ips)
    current <- Sys.time()
    ip <- isolate(input$remote_addr)
    agent <- isolate(input$remote_agent)
    if(!paste(format(current, "%d/%m/%y %H"), ip) %in% visitors){
      write(paste(as.character(current), ip, agent, sep="\t"),
            "www/counter.tsv", append = TRUE)
    }
  }
  observeEvent(input$remote_addr, update_visitor())
  output$total_visitor <- renderPlot({
    counter <- read.delim("www/counter.tsv", header = TRUE)
    counter <- as.Date(counter$date)
    counter <- table(format(counter, "%m/%y"))
    counter <- as.data.frame(counter)
    ggplot(counter, aes(x=Var1, y=Freq)) +
      geom_bar(stat = "identity", fill="darkorchid4") +
      theme_minimal() + xlab("") + ylab("visitor counts")
  })

  refreshData <- function(input, output, session){
    hasRef <- dataSource$geoAcc %in% names(refs_pmids)
    if(dataSource$dataset %in% names(data_types)){
      dataSource$terms <- terms[[data_types[[dataSource$dataset]]]]
    }
    dataSource <- loadData(dataSource)
    output$dataTitle <- renderUI({HTML(names(datasets)[datasets==input$availableDatasets])})
    output$ref_author <- renderText(ifelse(hasRef, refs_authors[dataSource$geoAcc], ""))
    output$ref_title <- renderText(ifelse(hasRef, refs_titles[dataSource$geoAcc], ""))
    output$ref_journal <- renderText(ifelse(hasRef, refs_journals[dataSource$geoAcc], ""))
    output$ref_year <- renderText(ifelse(hasRef, refs_years[dataSource$geoAcc], ""))
    output$ref_pmid <- renderUI({
      if(hasRef){
        a(refs_pmids[dataSource$geoAcc],
          href = paste0("https://www.ncbi.nlm.nih.gov/pubmed/",
                        refs_pmids[dataSource$geoAcc]))
      }else{
        br()
      }
    })
    output$tabCellInfoGeneExpr <- renderUI({HTML(paste("CellInfo vs", dataSource$terms["GeneExpr"]))})
    output$tabCellInfoGeneExprSubTitle <- renderUI({h4(paste("Cell information vs gene", dataSource$terms["expression"], "on reduced dimensions"))})
    output$tabCellInfoGeneExprSubTitGene <- renderUI({h4(paste("Gene", dataSource$terms['expression']))})
    output$tabGeneExprGeneExpr <- renderUI({HTML(paste(dataSource$terms["GeneExpr"], "vs", dataSource$terms["GeneExpr"]))})
    output$tabGeneExprGeneExprSubTitle <- renderUI({
      h4(paste("Gene", dataSource$terms['expression'], "vs gene", dataSource$terms['expression'], "on dimension reduction"))})
    output$tabGeneExprGeneExprSub1 <- renderUI({h4(paste("Gene", dataSource$terms['expression'], "1"))})
    output$tabGeneExprGeneExprSub2 <- renderUI({h4(paste("Gene", dataSource$terms['expression'], "2"))})
    output$tabCoExpr <- renderUI({HTML(paste("Gene", dataSource$terms['coexpression']))})
    output$tabCoExprSubTitle <- renderUI({h4(paste(sub(substr(dataSource$terms['coexpression'], 1, 1),
                                                       toupper(substr(dataSource$terms['coexpression'], 1, 1)),
                                                       dataSource$terms['coexpression']), "of two genes on reduced dimensions"))})
    output$tabCoExprSubTit1 <- renderUI({h4(paste("Gene", dataSource$terms['expression']))})

    updateSelectInput(session, "cellInfoGeneExprdrX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session, "cellInfoGeneExprdrY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session, "cellInfoGeneExprsub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session, "cellInfoGeneExprinp1", "Cell information:",
                      choices = dataSource$sc1conf$UI,
                      selected = dataSource$sc1def$meta1)
    updateSelectizeInput(session, "cellInfoGeneExprinp2",
                         choices = sort(names(dataSource$sc1gene)),
                         server = TRUE,
                         selected = dataSource$sc1def$gene1,
                         options = list(maxOptions = 6, create = TRUE,
                                        persist = TRUE, render = I(optCrt)))

    updateSelectInput(session, "cellInfoCellInfodrX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session, "cellInfoCellInfodrY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session, "cellInfoCellInfosub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session, "cellInfoCellInfoinp1", "Cell information:",
                      choices = dataSource$sc1conf$UI,
                      selected = dataSource$sc1def$meta1)
    updateSelectInput(session, "cellInfoCellInfoinp2", "Cell information:",
                      choices = dataSource$sc1conf$UI,
                      selected = dataSource$sc1def$meta2)
    updateSelectInput(session, "geneExprGeneExprdrX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session,"geneExprGeneExprdrY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session,"geneExprGeneExprsub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)

    updateSelectizeInput(session, "geneExprGeneExprinp1", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene1, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "geneExprGeneExprinp2", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene2, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))

    updateSelectInput(session,"scCoExprdrX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session,"scCoExprdrY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session,"scCoExprsub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)

    updateSelectizeInput(session, "scCoExprinp1", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene1, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "scCoExprinp2", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene2, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))

    updateSelectInput(session,"scVioBoxPlotinp1", "Cell information (X-axis):",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session,"scVioBoxPlotinp1a", "Cell information to subset by:",
                      choices = c("N/A", dataSource$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")
    updateSelectizeInput(session, "scVioBoxPlotinp2", server = TRUE,
                         choices = c(dataSource$sc1conf[is.na(fID)]$UI,sort(names(dataSource$sc1gene))),
                         selected = dataSource$sc1conf[is.na(fID)]$UI[1], options = list(
                           maxOptions = length(dataSource$sc1conf[is.na(fID)]$UI) + 3,
                           create = TRUE, persist = TRUE, render = I(optCrt)))

    updateSelectInput(session,"scProportioninp1", "Cell information to plot (X-axis):",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp2)
    updateSelectInput(session,"scProportioninp1a", "Cell information to subset by:",
                      choices = c("N/A", dataSource$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")
    updateSelectInput(session,"scProportioninp2", "Cell information to group / colour by:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)

    updateSelectInput(session,"scBubbleHeatmapgrp", "Group by:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1conf[grp == TRUE]$UI[1])
    updateSelectInput(session,"scBubbleHeatmapgrp1a", "Cell information to subset by:",
                      choices = c("N/A", dataSource$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")

    updateTextAreaInput(session, "scBubbleHeatmapinp",
                        value = paste0(dataSource$sc1def$genes, collapse = ", "))

    ### Plots for tab cell info vs gene expression
    output$cellInfoGeneExprsub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$cellInfoGeneExprsub1]$fID, "\\|")
      if(length(sub)){
        sub <- sub[[1]]
      }
      checkboxGroupInput("cellInfoGeneExprsub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$cellInfoGeneExproup1 <- renderPlot({
      scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprdrX, input$cellInfoGeneExprdrY, input$cellInfoGeneExprinp1,
               input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
               input$cellInfoGeneExprsiz, input$cellInfoGeneExprcol1, input$cellInfoGeneExprord1,
               input$cellInfoGeneExprfsz, input$cellInfoGeneExprasp, input$cellInfoGeneExprtxt, input$cellInfoGeneExprlab1)
    })
    output$cellInfoGeneExproup1.ui <- renderUI({
      plotOutput("cellInfoGeneExproup1", height = pList[input$cellInfoGeneExprpsz])
    })
    output$cellInfoGeneExproup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoGeneExprdrX,"_",input$cellInfoGeneExprdrY,"_",
                                     input$cellInfoGeneExprinp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$cellInfoGeneExproup1.h, width = input$cellInfoGeneExproup1.w, useDingbats = FALSE,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprdrX, input$cellInfoGeneExprdrY, input$cellInfoGeneExprinp1,
                        input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
                        input$cellInfoGeneExprsiz, input$cellInfoGeneExprcol1, input$cellInfoGeneExprord1,
                        input$cellInfoGeneExprfsz, input$cellInfoGeneExprasp, input$cellInfoGeneExprtxt, input$cellInfoGeneExprlab1) )
      })
    output$cellInfoGeneExproup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoGeneExprdrX,"_",input$cellInfoGeneExprdrY,"_",
                                     input$cellInfoGeneExprinp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$cellInfoGeneExproup1.h, width = input$cellInfoGeneExproup1.w,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprdrX, input$cellInfoGeneExprdrY, input$cellInfoGeneExprinp1,
                        input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
                        input$cellInfoGeneExprsiz, input$cellInfoGeneExprcol1, input$cellInfoGeneExprord1,
                        input$cellInfoGeneExprfsz, input$cellInfoGeneExprasp, input$cellInfoGeneExprtxt, input$cellInfoGeneExprlab1) )
      })
    output$cellInfoGeneExpr.dt <- renderDataTable({
      ggData = scDRnum(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprinp1, input$cellInfoGeneExprinp2,
                       input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
                       dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$cellInfoGeneExprsplt)
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("pctExpress"), digits = 2)
    })

    output$cellInfoGeneExproup2 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprdrX, input$cellInfoGeneExprdrY, input$cellInfoGeneExprinp2,
               input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$cellInfoGeneExprsiz, input$cellInfoGeneExprcol2, input$cellInfoGeneExprord2,
               input$cellInfoGeneExprfsz, input$cellInfoGeneExprasp, input$cellInfoGeneExprtxt,
               input$cellInfoGeneExprtype2, if(input$cellInfoGeneExprxlim0 %% 2==0) 0 else input$cellInfoGeneExprxlim1,
               if(input$cellInfoGeneExprrg0 %% 2==0) 0 else input$cellInfoGeneExprrg1)
    })
    output$cellInfoGeneExproup2.ui <- renderUI({
      plotOutput("cellInfoGeneExproup2", height = pList[input$cellInfoGeneExprpsz])
    })
    output$cellInfoGeneExproup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoGeneExprdrX,"_",input$cellInfoGeneExprdrY,"_",
                                     input$cellInfoGeneExprinp2,".pdf") },
      content = function(file) {
        ggsave(
          file, device = "pdf", height = input$cellInfoGeneExproup2.h, width = input$cellInfoGeneExproup2.w, useDingbats = FALSE,
          plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprdrX, input$cellInfoGeneExprdrY, input$cellInfoGeneExprinp2,
                          input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
                          dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                          input$cellInfoGeneExprsiz, input$cellInfoGeneExprcol2, input$cellInfoGeneExprord2,
                          input$cellInfoGeneExprfsz, input$cellInfoGeneExprasp, input$cellInfoGeneExprtxt,
                          input$cellInfoGeneExprtype2, if(input$cellInfoGeneExprxlim0 %% 2==0) 0 else input$cellInfoGeneExprxlim1,
                          if(input$cellInfoGeneExprrg0 %% 2==0) 0 else input$cellInfoGeneExprrg1)
        )
      })
    output$cellInfoGeneExproup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoGeneExprdrX,"_",input$cellInfoGeneExprdrY,"_",
                                     input$cellInfoGeneExprinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$cellInfoGeneExproup2.h, width = input$cellInfoGeneExproup2.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoGeneExprdrX, input$cellInfoGeneExprdrY, input$cellInfoGeneExprinp2,
                        input$cellInfoGeneExprsub1, input$cellInfoGeneExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$cellInfoGeneExprsiz, input$cellInfoGeneExprcol2, input$cellInfoGeneExprord2,
                        input$cellInfoGeneExprfsz, input$cellInfoGeneExprasp, input$cellInfoGeneExprtxt,
                        input$cellInfoGeneExprtype2, if(input$cellInfoGeneExprxlim0 %% 2==0) 0 else input$cellInfoGeneExprxlim1,
                        if(input$cellInfoGeneExprrg0 %% 2==0) 0 else input$cellInfoGeneExprrg1) )
      })


    ### Plots for tab cell info vs cell info
    output$cellInfoCellInfosub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$cellInfoCellInfosub1]$fID, "\\|")[[1]]
      checkboxGroupInput("cellInfoCellInfosub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$cellInfoCellInfooup1 <- renderPlot({
      scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoCellInfodrX, input$cellInfoCellInfodrY, input$cellInfoCellInfoinp1,
               input$cellInfoCellInfosub1, input$cellInfoCellInfosub2,
               input$cellInfoCellInfosiz, input$cellInfoCellInfocol1, input$cellInfoCellInfoord1,
               input$cellInfoCellInfofsz, input$cellInfoCellInfoasp, input$cellInfoCellInfotxt, input$cellInfoCellInfolab1)
    })
    output$cellInfoCellInfooup1.ui <- renderUI({
      plotOutput("cellInfoCellInfooup1", height = pList[input$cellInfoCellInfopsz])
    })
    output$cellInfoCellInfooup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoCellInfodrX,"_",input$cellInfoCellInfodrY,"_",
                                     input$cellInfoCellInfoinp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$cellInfoCellInfooup1.h, width = input$cellInfoCellInfooup1.w, useDingbats = FALSE,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoCellInfodrX, input$cellInfoCellInfodrY, input$cellInfoCellInfoinp1,
                        input$cellInfoCellInfosub1, input$cellInfoCellInfosub2,
                        input$cellInfoCellInfosiz, input$cellInfoCellInfocol1, input$cellInfoCellInfoord1,
                        input$cellInfoCellInfofsz, input$cellInfoCellInfoasp, input$cellInfoCellInfotxt, input$cellInfoCellInfolab1) )
      })
    output$cellInfoCellInfooup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoCellInfodrX,"_",input$cellInfoCellInfodrY,"_",
                                     input$cellInfoCellInfoinp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$cellInfoCellInfooup1.h, width = input$cellInfoCellInfooup1.w,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoCellInfodrX, input$cellInfoCellInfodrY, input$cellInfoCellInfoinp1,
                        input$cellInfoCellInfosub1, input$cellInfoCellInfosub2,
                        input$cellInfoCellInfosiz, input$cellInfoCellInfocol1, input$cellInfoCellInfoord1,
                        input$cellInfoCellInfofsz, input$cellInfoCellInfoasp, input$cellInfoCellInfotxt, input$cellInfoCellInfolab1) )
      })

    output$cellInfoCellInfooup2 <- renderPlot({
      scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoCellInfodrX, input$cellInfoCellInfodrY, input$cellInfoCellInfoinp2,
               input$cellInfoCellInfosub1, input$cellInfoCellInfosub2,
               input$cellInfoCellInfosiz, input$cellInfoCellInfocol2, input$cellInfoCellInfoord2,
               input$cellInfoCellInfofsz, input$cellInfoCellInfoasp, input$cellInfoCellInfotxt, input$cellInfoCellInfolab2)
    })
    output$cellInfoCellInfooup2.ui <- renderUI({
      plotOutput("cellInfoCellInfooup2", height = pList[input$cellInfoCellInfopsz])
    })
    output$cellInfoCellInfooup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoCellInfodrX,"_",input$cellInfoCellInfodrY,"_",
                                     input$cellInfoCellInfoinp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$cellInfoCellInfooup2.h, width = input$cellInfoCellInfooup2.w, useDingbats = FALSE,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoCellInfodrX, input$cellInfoCellInfodrY, input$cellInfoCellInfoinp2,
                        input$cellInfoCellInfosub1, input$cellInfoCellInfosub2,
                        input$cellInfoCellInfosiz, input$cellInfoCellInfocol2, input$cellInfoCellInfoord2,
                        input$cellInfoCellInfofsz, input$cellInfoCellInfoasp, input$cellInfoCellInfotxt, input$cellInfoCellInfolab2) )
      })
    output$cellInfoCellInfooup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$cellInfoCellInfodrX,"_",input$cellInfoCellInfodrY,"_",
                                     input$cellInfoCellInfoinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$cellInfoCellInfooup2.h, width = input$cellInfoCellInfooup2.w,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$cellInfoCellInfodrX, input$cellInfoCellInfodrY, input$cellInfoCellInfoinp2,
                        input$cellInfoCellInfosub1, input$cellInfoCellInfosub2,
                        input$cellInfoCellInfosiz, input$cellInfoCellInfocol2, input$cellInfoCellInfoord2,
                        input$cellInfoCellInfofsz, input$cellInfoCellInfoasp, input$cellInfoCellInfotxt, input$cellInfoCellInfolab2) )
      })


    ### Plots for tab gene expression vs gene expression
    output$geneExprGeneExprsub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$geneExprGeneExprsub1]$fID, "\\|")[[1]]
      checkboxGroupInput("geneExprGeneExprsub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$geneExprGeneExproup1 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$geneExprGeneExprdrX, input$geneExprGeneExprdrY, input$geneExprGeneExprinp1,
               input$geneExprGeneExprsub1, input$geneExprGeneExprsub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$geneExprGeneExprsiz, input$geneExprGeneExprcol1, input$geneExprGeneExprord1,
               input$geneExprGeneExprfsz, input$geneExprGeneExprasp, input$geneExprGeneExprtxt,
               inpColRange=if(input$geneExprGeneExprrg0 %% 2==0) 0 else input$geneExprGeneExprrg1)
    })
    output$geneExprGeneExproup1.ui <- renderUI({
      plotOutput("geneExprGeneExproup1", height = pList[input$geneExprGeneExprpsz])
    })
    output$geneExprGeneExproup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$geneExprGeneExprdrX,"_",input$geneExprGeneExprdrY,"_",
                                     input$geneExprGeneExprinp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$geneExprGeneExproup1.h, width = input$geneExprGeneExproup1.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$geneExprGeneExprdrX, input$geneExprGeneExprdrY, input$geneExprGeneExprinp1,
                        input$geneExprGeneExprsub1, input$geneExprGeneExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$geneExprGeneExprsiz, input$geneExprGeneExprcol1, input$geneExprGeneExprord1,
                        input$geneExprGeneExprfsz, input$geneExprGeneExprasp, input$geneExprGeneExprtxt,
                        inpColRange=if(input$geneExprGeneExprrg0 %% 2==0) 0 else input$geneExprGeneExprrg1) )
      })
    output$geneExprGeneExproup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$geneExprGeneExprdrX,"_",input$geneExprGeneExprdrY,"_",
                                     input$geneExprGeneExprinp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$geneExprGeneExproup1.h, width = input$geneExprGeneExproup1.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$geneExprGeneExprdrX, input$geneExprGeneExprdrY, input$geneExprGeneExprinp1,
                        input$geneExprGeneExprsub1, input$geneExprGeneExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$geneExprGeneExprsiz, input$geneExprGeneExprcol1, input$geneExprGeneExprord1,
                        input$geneExprGeneExprfsz, input$geneExprGeneExprasp, input$geneExprGeneExprtxt,
                        inpColRange=if(input$geneExprGeneExprrg0 %% 2==0) 0 else input$geneExprGeneExprrg1) )
      })

    output$geneExprGeneExproup2 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$geneExprGeneExprdrX, input$geneExprGeneExprdrY, input$geneExprGeneExprinp2,
               input$geneExprGeneExprsub1, input$geneExprGeneExprsub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$geneExprGeneExprsiz, input$geneExprGeneExprcol2, input$geneExprGeneExprord2,
               input$geneExprGeneExprfsz, input$geneExprGeneExprasp, input$geneExprGeneExprtxt,
               inpColRange=if(input$geneExprGeneExprrg2 %% 2==0) 0 else input$geneExprGeneExprrg3)
    })
    output$geneExprGeneExproup2.ui <- renderUI({
      plotOutput("geneExprGeneExproup2", height = pList[input$geneExprGeneExprpsz])
    })
    output$geneExprGeneExproup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$geneExprGeneExprdrX,"_",input$geneExprGeneExprdrY,"_",
                                     input$geneExprGeneExprinp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$geneExprGeneExproup2.h, width = input$geneExprGeneExproup2.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$geneExprGeneExprdrX, input$geneExprGeneExprdrY, input$geneExprGeneExprinp2,
                        input$geneExprGeneExprsub1, input$geneExprGeneExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$geneExprGeneExprsiz, input$geneExprGeneExprcol2, input$geneExprGeneExprord2,
                        input$geneExprGeneExprfsz, input$geneExprGeneExprasp, input$geneExprGeneExprtxt,
                        inpColRange=if(input$geneExprGeneExprrg2 %% 2==0) 0 else input$geneExprGeneExprrg3) )
      })
    output$geneExprGeneExproup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$geneExprGeneExprdrX,"_",input$geneExprGeneExprdrY,"_",
                                     input$geneExprGeneExprinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$geneExprGeneExproup2.h, width = input$geneExprGeneExproup2.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$geneExprGeneExprdrX, input$geneExprGeneExprdrY, input$geneExprGeneExprinp2,
                        input$geneExprGeneExprsub1, input$geneExprGeneExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$geneExprGeneExprsiz, input$geneExprGeneExprcol2, input$geneExprGeneExprord2,
                        input$geneExprGeneExprfsz, input$geneExprGeneExprasp, input$geneExprGeneExprtxt,
                        inpColRange=if(input$geneExprGeneExprrg2 %% 2==0) 0 else input$geneExprGeneExprrg3) )
      })


    ### Plots for tab co-expression
    output$scCoExprsub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$scCoExprsub1]$fID, "\\|")[[1]]
      checkboxGroupInput("scCoExprsub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$scCoExproup1 <- renderPlot({
      scDRcoex(dataSource$sc1conf, dataSource$sc1meta, input$scCoExprdrX, input$scCoExprdrY,
               input$scCoExprinp1, input$scCoExprinp2, input$scCoExprsub1, input$scCoExprsub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$scCoExprsiz, input$scCoExprcol1, input$scCoExprord1,
               input$scCoExprfsz, input$scCoExprasp, input$scCoExprtxt)
    })
    output$scCoExproup1.ui <- renderUI({
      plotOutput("scCoExproup1", height = pList2[input$scCoExprpsz])
    })
    output$scCoExproup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$scCoExprdrX,"_",input$scCoExprdrY,"_",
                                     input$scCoExprinp1,"_",input$scCoExprinp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$scCoExproup1.h, width = input$scCoExproup1.w, useDingbats = FALSE,
        plot = scDRcoex(dataSource$sc1conf, dataSource$sc1meta, input$scCoExprdrX, input$scCoExprdrY,
                        input$scCoExprinp1, input$scCoExprinp2, input$scCoExprsub1, input$scCoExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$scCoExprsiz, input$scCoExprcol1, input$scCoExprord1,
                        input$scCoExprfsz, input$scCoExprasp, input$scCoExprtxt) )
      })
    output$scCoExproup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$scCoExprdrX,"_",input$scCoExprdrY,"_",
                                     input$scCoExprinp1,"_",input$scCoExprinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$scCoExproup1.h, width = input$scCoExproup1.w,
        plot = scDRcoex(dataSource$sc1conf, dataSource$sc1meta, input$scCoExprdrX, input$scCoExprdrY,
                        input$scCoExprinp1, input$scCoExprinp2, input$scCoExprsub1, input$scCoExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$scCoExprsiz, input$scCoExprcol1, input$scCoExprord1,
                        input$scCoExprfsz, input$scCoExprasp, input$scCoExprtxt) )
      })
    output$scCoExproup2 <- renderPlot({
      scDRcoexLeg(input$scCoExprinp1, input$scCoExprinp2, input$scCoExprcol1, input$scCoExprfsz)
    })
    output$scCoExproup2.ui <- renderUI({
      plotOutput("scCoExproup2", height = "300px")
    })
    output$scCoExproup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$scCoExprdrX,"_",input$scCoExprdrY,"_",
                                     input$scCoExprinp1,"_",input$scCoExprinp2,"_leg.pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = 3, width = 4, useDingbats = FALSE,
        plot = scDRcoexLeg(input$scCoExprinp1, input$scCoExprinp2, input$scCoExprcol1, input$scCoExprfsz) )
      })
    output$scCoExproup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$scCoExprdrX,"_",input$scCoExprdrY,"_",
                                     input$scCoExprinp1,"_",input$scCoExprinp2,"_leg.png") },
      content = function(file) { ggsave(
        file, device = "png", height = 3, width = 4,
        plot = scDRcoexLeg(input$scCoExprinp1, input$scCoExprinp2, input$scCoExprcol1, input$scCoExprfsz) )
      })
    output$scCoExpr.dt <- renderDataTable({
      ggData = scDRcoexNum(dataSource$sc1conf, dataSource$sc1meta, input$scCoExprinp1, input$scCoExprinp2,
                           input$scCoExprsub1, input$scCoExprsub2, dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene)
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("percent"), digits = 2)
    })

    ### Plots for tab subset
    output$tabSubsetGeneExpr <- renderUI({HTML(paste("Subset", dataSource$terms["GeneExpr"]))})
    output$tabSubsetGeneExprSubTitle <- renderUI({
      h4(paste("Subset gene", dataSource$terms['expression'], "on dimension reduction"))})
    output$tabSubsetGeneExprSub1 <- renderUI({h4(paste("Gene", dataSource$terms['expression']))})
    output$tabSubsetGeneExprSub2 <- renderUI({h4(paste("Gene", dataSource$terms['expression']))})
    updateSelectInput(session, "subsetGeneExprdrX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session,"subsetGeneExprdrY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectizeInput(session, "subsetGeneExprinp1", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene1, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectInput(session,"subsetGeneExprinp2", "Cell information to show:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session,"subsetGeneExprsub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectizeInput(session, "subsetGeneExprsub2", server = TRUE,
                         choices = c(dataSource$sc1conf[is.na(fID)]$UI,sort(names(dataSource$sc1gene))),
                         selected = dataSource$sc1conf[is.na(fID)]$UI[1], options = list(
                           maxOptions = length(dataSource$sc1conf[is.na(fID)]$UI) + 3,
                           create = TRUE, persist = TRUE, render = I(optCrt)))
    output$subsetGeneExprgrp1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$subsetGeneExprinp2]$fID, "\\|")[[1]]
      checkboxGroupInput("scSubExprsub1", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub[1])
    })
    output$subsetGeneExprgrp2.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$subsetGeneExprinp2]$fID, "\\|")[[1]]
      checkboxGroupInput("scSubExprsub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = ifelse(length(sub)>1, sub[2], sub[1]))
    })
    output$subsetGeneExprsub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$subsetGeneExprsub1]$fID, "\\|")[[1]]
      checkboxGroupInput("scSubExprinp1b", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$subsetGeneExprsub2.ui <- renderUI({
      if(!input$subsetGeneExprsub2 %in% dataSource$sc1conf$UI){
        h5file <- H5File$new(file.path(datafolder, dataSource$dataset, "sc1gexpr.h5"), mode = "r")
        h5data <- h5file[["grp"]][["data"]]
        val = h5data$read(args = list(dataSource$sc1gene[input$subsetGeneExprsub2], quote(expr=)))
        val <- max(val, na.rm = TRUE)
        h5file$close_all()
      }else{
        val = dataSource$sc1meta[[dataSource$sc1conf[UI == input$subsetGeneExprsub2]$ID]]
        val <- max(val, na.rm = TRUE)
      }
      if(val<=1) maxv <- round(val, digits = 3)
      if(val>1 && val<=10) maxv <- round(val, digits = 1)
      if(val>10) maxv <- round(val, digits = 0)
      sliderInput("scSubExprinp1c", "Filter the cells by value",
                  min = 0, max = maxv, value = 0)
    })
    output$subsetGeneExproup1 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$subsetGeneExprdrX, input$subsetGeneExprdrY, input$subsetGeneExprinp1,
               input$subsetGeneExprinp2, input$scSubExprsub1,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$subsetGeneExprsiz, input$subsetGeneExprcol1, input$subsetGeneExprord1,
               input$subsetGeneExprfsz, input$subsetGeneExprasp, input$subsetGeneExprtxt,
               inpColRange=if(input$subsetGeneExprrg0 %% 2==0) 0 else input$subsetGeneExprrg1,
               inpsub3=input$subsetGeneExprsub1,
               inpsub3filter=input$scSubExprinp1b,
               inpsub4=input$subsetGeneExprsub2,
               inpsub4filter=input$scSubExprinp1c)
    })
    output$subsetGeneExproup1.ui <- renderUI({
      plotOutput("subsetGeneExproup1", height = pList[input$subsetGeneExprpsz])
    })
    output$subsetGeneExproup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$subsetGeneExprdrX,"_",input$subsetGeneExprdrY,"_",
                                     input$subsetGeneExprinp1, "_", input$subsetGeneExprinp2, ".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$subsetGeneExproup1.h, width = input$subsetGeneExproup1.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$subsetGeneExprdrX, input$subsetGeneExprdrY, input$subsetGeneExprinp1,
                        input$subsetGeneExprinp2, input$scSubExprsub1,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$subsetGeneExprsiz, input$subsetGeneExprcol1, input$subsetGeneExprord1,
                        input$subsetGeneExprfsz, input$subsetGeneExprasp, input$subsetGeneExprtxt,
                        inpColRange=if(input$subsetGeneExprrg0 %% 2==0) 0 else input$subsetGeneExprrg1,
                        inpsub3=input$subsetGeneExprsub1,
                        inpsub3filter=input$scSubExprinp1b,
                        inpsub4=input$subsetGeneExprsub2,
                        inpsub4filter=input$scSubExprinp1c) )
      })
    output$subsetGeneExproup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$subsetGeneExprdrX,"_",input$subsetGeneExprdrY,"_",
                                     input$subsetGeneExprinp1, "_", input$subsetGeneExprinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$subsetGeneExproup1.h, width = input$subsetGeneExproup1.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$subsetGeneExprdrX, input$subsetGeneExprdrY, input$subsetGeneExprinp1,
                        input$subsetGeneExprinp2, input$scSubExprsub1,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$subsetGeneExprsiz, input$subsetGeneExprcol1, input$subsetGeneExprord1,
                        input$subsetGeneExprfsz, input$subsetGeneExprasp, input$subsetGeneExprtxt,
                        inpColRange=if(input$subsetGeneExprrg0 %% 2==0) 0 else input$subsetGeneExprrg1,
                        inpsub3=input$subsetGeneExprsub1,
                        inpsub3filter=input$scSubExprinp1b,
                        inpsub4=input$subsetGeneExprsub2,
                        inpsub4filter=input$scSubExprinp1c) )
      })
    output$subsetGeneExproup2 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$subsetGeneExprdrX, input$subsetGeneExprdrY, input$subsetGeneExprinp1,
               input$subsetGeneExprinp2, input$scSubExprsub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$subsetGeneExprsiz, input$subsetGeneExprcol2, input$subsetGeneExprord2,
               input$subsetGeneExprfsz, input$subsetGeneExprasp, input$subsetGeneExprtxt,
               inpColRange=if(input$subsetGeneExprrg2 %% 2==0) 0 else input$subsetGeneExprrg3,
               inpsub3=input$subsetGeneExprsub1,
               inpsub3filter=input$scSubExprinp1b,
               inpsub4=input$subsetGeneExprsub2,
               inpsub4filter=input$scSubExprinp1c)
    })
    output$subsetGeneExproup2.ui <- renderUI({
      plotOutput("subsetGeneExproup2", height = pList[input$subsetGeneExprpsz])
    })
    output$subsetGeneExproup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$subsetGeneExprdrX,"_",input$subsetGeneExprdrY,"_",
                                     input$subsetGeneExprinp1, "_", input$subsetGeneExprinp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$subsetGeneExproup1.h, width = input$subsetGeneExproup1.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$subsetGeneExprdrX, input$subsetGeneExprdrY, input$subsetGeneExprinp1,
                        input$subsetGeneExprinp2, input$scSubExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$subsetGeneExprsiz, input$subsetGeneExprcol2, input$subsetGeneExprord2,
                        input$subsetGeneExprfsz, input$subsetGeneExprasp, input$subsetGeneExprtxt,
                        inpColRange=if(input$subsetGeneExprrg2 %% 2==0) 0 else input$subsetGeneExprrg3,
                        inpsub3=input$subsetGeneExprsub1,
                        inpsub3filter=input$scSubExprinp1b,
                        inpsub4=input$subsetGeneExprsub2,
                        inpsub4filter=input$scSubExprinp1c) )
      })
    output$subsetGeneExproup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$subsetGeneExprdrX,"_",input$subsetGeneExprdrY,"_",
                                     input$subsetGeneExprinp1, "_", input$subsetGeneExprinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$subsetGeneExproup1.h, width = input$subsetGeneExproup1.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$subsetGeneExprdrX, input$subsetGeneExprdrY, input$subsetGeneExprinp1,
                        input$subsetGeneExprinp2, input$scSubExprsub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$subsetGeneExprsiz, input$subsetGeneExprcol2, input$subsetGeneExprord2,
                        input$subsetGeneExprfsz, input$subsetGeneExprasp, input$subsetGeneExprtxt,
                        inpColRange=if(input$subsetGeneExprrg2 %% 2==0) 0 else input$subsetGeneExprrg3,
                        inpsub3=input$subsetGeneExprsub1,
                        inpsub3filter=input$scSubExprinp1b,
                        inpsub4=input$subsetGeneExprsub2,
                        inpsub4filter=input$scSubExprinp1c) )
      })

    ### Plots for tab violion
    output$scVioBoxPlotinp1b.ui <- renderUI({
      if(input$scVioBoxPlotinp1a!="N/A"){
        sub = strsplit(dataSource$sc1conf[UI == input$scVioBoxPlotinp1a]$fID, "\\|")[[1]]
        checkboxGroupInput("scVioBoxPlotinp1b", "Select which cells to show", inline = TRUE,
                           choices = sub)
      }else{
        sub = NULL
      }
    })
    output$scVioBoxPlotinp1c.ui <- renderUI({
      if(!input$scVioBoxPlotinp2 %in% dataSource$sc1conf$UI){
        h5file <- H5File$new(file.path(datafolder, dataSource$dataset, "sc1gexpr.h5"), mode = "r")
        h5data <- h5file[["grp"]][["data"]]
        val = h5data$read(args = list(dataSource$sc1gene[input$scVioBoxPlotinp2], quote(expr=)))
        val <- max(val, na.rm = TRUE)
        h5file$close_all()
      }else{
          val = dataSource$sc1meta[[dataSource$sc1conf[UI == input$scVioBoxPlotinp2]$ID]]
          val <- max(val, na.rm = TRUE)
      }
      if(val<=1) maxv <- round(val, digits = 3)
      if(val>1 && val<=10) maxv <- round(val, digits = 1)
      if(val>10) maxv <- round(val, digits = 0)
      sliderInput("scVioBoxPlotinp1c", "Filter the cells by value",
                  min = 0, max = maxv, value = 0)
    })
    output$scVioBoxPlotoup <- renderPlot({
      scVioBox(dataSource$sc1conf, dataSource$sc1meta, input$scVioBoxPlotinp1, input$scVioBoxPlotinp1a, input$scVioBoxPlotinp1b, input$scVioBoxPlotinp1c, input$scVioBoxPlotinp2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$scVioBoxPlottyp, input$scVioBoxPlotpts,
               input$scVioBoxPlotsiz, input$scVioBoxPlotfsz)
    })
    output$scVioBoxPlotoup.ui <- renderUI({
      plotOutput("scVioBoxPlotoup", height = pList2[input$scVioBoxPlotpsz])
    })
    output$scVioBoxPlotoup.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$scVioBoxPlottyp,"_",input$scVioBoxPlotinp1,"_",
                                     input$scVioBoxPlotinp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$scVioBoxPlotoup.h, width = input$scVioBoxPlotoup.w, useDingbats = FALSE,
        plot = scVioBox(dataSource$sc1conf, dataSource$sc1meta, input$scVioBoxPlotinp1, input$scVioBoxPlotinp1a, input$scVioBoxPlotinp1b, input$scVioBoxPlotinp1c, input$scVioBoxPlotinp2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$scVioBoxPlottyp, input$scVioBoxPlotpts,
                        input$scVioBoxPlotsiz, input$scVioBoxPlotfsz) )
      })
    output$scVioBoxPlotoup.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$scVioBoxPlottyp,"_",input$scVioBoxPlotinp1,"_",
                                     input$scVioBoxPlotinp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$scVioBoxPlotoup.h, width = input$scVioBoxPlotoup.w,
        plot = scVioBox(dataSource$sc1conf, dataSource$sc1meta, input$scVioBoxPlotinp1, input$scVioBoxPlotinp1a, input$scVioBoxPlotinp1b, input$scVioBoxPlotinp1c, input$scVioBoxPlotinp2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$scVioBoxPlottyp, input$scVioBoxPlotpts,
                        input$scVioBoxPlotsiz, input$scVioBoxPlotfsz) )
      })


    ### Plots for tab proportion
    output$scProportionoup <- renderPlot({
      scProp(dataSource$sc1conf, dataSource$sc1meta, input$scProportioninp1, input$scProportioninp1a, input$scProportioninp1b, input$scProportioninp2,
             input$scProportiontyp, input$scProportionflp, input$scProportionfsz)
    })
    output$scProportioninp1b.ui <- renderUI({
      if(input$scProportioninp1a!="N/A"){
        sub = strsplit(dataSource$sc1conf[UI == input$scProportioninp1a]$fID, "\\|")[[1]]
        checkboxGroupInput("scProportioninp1b", "Select which cells to show", inline = TRUE,
                           choices = sub)
      }else{
        sub = NULL
      }
    })
    output$scProportionoup.ui <- renderUI({
      plotOutput("scProportionoup", height = pList2[input$scProportionpsz])
    })
    output$scProportionoup.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$scProportiontyp,"_",input$scProportioninp1,"_",
                                     input$scProportioninp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$scProportionoup.h, width = input$scProportionoup.w, useDingbats = FALSE,
        plot = scProp(dataSource$sc1conf, dataSource$sc1meta, input$scProportioninp1, input$scProportioninp1a, input$scProportioninp1b, input$scProportioninp2,
                      input$scProportiontyp, input$scProportionflp, input$scProportionfsz) )
      })
    output$scProportionoup.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$scProportiontyp,"_",input$scProportioninp1,"_",
                                     input$scProportioninp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$scProportionoup.h, width = input$scProportionoup.w,
        plot = scProp(dataSource$sc1conf, dataSource$sc1meta, input$scProportioninp1, input$scProportioninp1a, input$scProportioninp1b, input$scProportioninp2,
                      input$scProportiontyp, input$scProportionflp, input$scProportionfsz) )
      })
    output$scProportion.dt <- renderDataTable({
      ggData = scProp(dataSource$sc1conf, dataSource$sc1meta, input$scProportioninp1, input$scProportioninp1a, input$scProportioninp1b, input$scProportioninp2,
                      input$scProportiontyp, input$scProportionflp, input$scProportionfsz)
      datatable(ggData$data, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel")))
    })


    ### Plots for tab bubble heatmap
    output$scBubbleHeatmapgrp1b.ui <- renderUI({
      if(input$scBubbleHeatmapgrp1a!="N/A"){
        sub = strsplit(dataSource$sc1conf[UI == input$scBubbleHeatmapgrp1a]$fID, "\\|")[[1]]
        checkboxGroupInput("scBubbleHeatmapgrp1b", "Select which cells to show", inline = TRUE,
                           choices = sub)
      }else{
        sub = NULL
      }
    })
    output$scBubbleHeatmapoupTxt <- renderUI({
      geneList = scGeneList(input$scBubbleHeatmapinp, dataSource$sc1gene)
      if(nrow(geneList) > 50){
        HTML("More than 50 input genes! Please reduce the gene list!")
      } else {
        oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted")
        if(nrow(geneList[present == FALSE]) > 0){
          oup = paste0(oup, "<br/>",
                       nrow(geneList[present == FALSE]), " genes not found (",
                       paste0(geneList[present == FALSE]$gene, collapse = ", "), ")")
        }
        HTML(oup)
      }
    })
    output$scBubbleHeatmapoup <- renderPlot({
      scBubbHeat(dataSource$sc1conf, dataSource$sc1meta, input$scBubbleHeatmapinp, input$scBubbleHeatmapgrp,
                 input$scBubbleHeatmapgrp1a, input$scBubbleHeatmapgrp1b, input$scBubbleHeatmapgrp1c, input$scBubbleHeatmapplt,
                 dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                 input$scBubbleHeatmapscl, input$scBubbleHeatmaprow, input$scBubbleHeatmapcol,
                 input$scBubbleHeatmapcols, input$scBubbleHeatmapfsz, legendTitle=dataSource$terms['expression'])
    })
    output$scBubbleHeatmapoup.ui <- renderUI({
      plotOutput("scBubbleHeatmapoup", height = pList3[input$scBubbleHeatmappsz])
    })
    output$scBubbleHeatmapoup.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$scBubbleHeatmapplt,"_",input$scBubbleHeatmapgrp,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$scBubbleHeatmapoup.h, width = input$scBubbleHeatmapoup.w,
        plot = scBubbHeat(dataSource$sc1conf, dataSource$sc1meta, input$scBubbleHeatmapinp, input$scBubbleHeatmapgrp,
                          input$scBubbleHeatmapgrp1a, input$scBubbleHeatmapgrp1b, input$scBubbleHeatmapgrp1c, input$scBubbleHeatmapplt,
                          dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                          input$scBubbleHeatmapscl, input$scBubbleHeatmaprow, input$scBubbleHeatmapcol,
                          input$scBubbleHeatmapcols, input$scBubbleHeatmapfsz, save = TRUE,
                          legendTitle=dataSource$terms['expression']) )
      })
    output$scBubbleHeatmapoup.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$scBubbleHeatmapplt,"_",input$scBubbleHeatmapgrp,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$scBubbleHeatmapoup.h, width = input$scBubbleHeatmapoup.w,
        plot = scBubbHeat(dataSource$sc1conf, dataSource$sc1meta, input$scBubbleHeatmapinp, input$scBubbleHeatmapgrp,
                          input$scBubbleHeatmapgrp1a, input$scBubbleHeatmapgrp1b, input$scBubbleHeatmapgrp1c, input$scBubbleHeatmapplt,
                          dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                          input$scBubbleHeatmapscl, input$scBubbleHeatmaprow, input$scBubbleHeatmapcol,
                          input$scBubbleHeatmapcols, input$scBubbleHeatmapfsz, save = TRUE,
                          legendTitle=dataSource$terms['expression']) )
      })
  }
}

shinyApp(ui=ui, server = server)



