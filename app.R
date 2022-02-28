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
VERSION = "2.0.6"
if(names(dev.cur())!= "null device") dev.off()
pdf(NULL)

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
    footer = div(p(em("scRNAseq Database (Version:", VERSION, ")"),
               HTML("&copy;"), "2020 -",
               format(Sys.Date(), "%Y"),
               "jianhong@duke"), class="rightAlign"),
    ### Tab1.a1: cellInfo vs geneExpr on dimRed
    tab1a1(),
    ### Tab1.a2: cellInfo vs cellInfo on dimRed
    tab1a2(),
    ### Tab1.a3: geneExpr vs geneExpr on dimRed
    tab1a3(),
    ### Tab1.b2: Gene coexpression plot
    tab1b2(),
    ### Tab1.c1: violinplot / boxplot
    tab1c1(),
    ### Tab1.c2: Proportion plot
    tab1c2(),
    ### Tab1.d1: Multiple gene expr
    tab1d1(),
    ### Tab1.e1: change dataset
    tab1e1(req),
    ### Tab1.f1: Login form
    tab1f1(),
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
                               Username="",
                               Password="")
  observeEvent(input$Login, {
        dataSource$Username <- isolate(input$userName)
        dataSource$Password <- isolate(input$passwd)
        if(!checkLockedDataset(dataSource$dataset)){
          output$loginmsg <- renderText("No need to login yet.")
          updateTabsetPanel(session, "topnav", selected = "ChangeDataset")
        }
        if (checkUserNameAndPassword(dataSource$Username, dataSource$Password, dataSource$dataset)) {
          dataSource$Logged <- TRUE
          output$loginmsg <- renderText("Logged!")
          updateTabsetPanel(session, "topnav", selected = "ChangeDataset")
        }else{
          dataSource$Logged <- FALSE
          dataSource$Username <- ""
          dataSource$Password <- ""
          output$loginmsg <- renderText("Wrong Username, Password or Dataset!")
        }
  })

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['data']])){
      updateSelectInput(session, "availableDatasets", selected=query[['data']])
    }
  })
  observeEvent(input$availableDatasets,{
    dataSource$dataset <- input$availableDatasets
    dataSource$geoAcc <- sub("^(.*?)_.*$", "\\1", input$availableDatasets)
    if(checkLockedDataset(dataSource$dataset)){
      if(!dataSource$Logged){
        if(dataSource$Username!="" && dataSource$Password!=""){
          if(checkUserNameAndPassword(dataSource$Username, dataSource$Password, dataSource$dataset)){
            dataSource$Logged <- TRUE
            output$loginmsg <- renderText("Logged!")
          }else{
            updateTabsetPanel(session, "topnav", selected = "Login")
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

    updateSelectInput(session, "sc1a1drX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session, "sc1a1drY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session, "sc1a1sub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session, "sc1a1inp1", "Cell information:",
                      choices = dataSource$sc1conf$UI,
                      selected = dataSource$sc1def$meta1)
    updateSelectizeInput(session, "sc1a1inp2",
                         choices = sort(names(dataSource$sc1gene)),
                         server = TRUE,
                         selected = dataSource$sc1def$gene1,
                         options = list(maxOptions = 6, create = TRUE,
                                        persist = TRUE, render = I(optCrt)))

    updateSelectInput(session, "sc1a2drX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session, "sc1a2drY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session, "sc1a2sub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session, "sc1a2inp1", "Cell information:",
                      choices = dataSource$sc1conf$UI,
                      selected = dataSource$sc1def$meta1)
    updateSelectInput(session, "sc1a2inp2", "Cell information:",
                      choices = dataSource$sc1conf$UI,
                      selected = dataSource$sc1def$meta2)
    updateSelectInput(session, "sc1a3drX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session,"sc1a3drY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session,"sc1a3sub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)

    updateSelectizeInput(session, "sc1a3inp1", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene1, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1a3inp2", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene2, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))

    updateSelectInput(session,"sc1b2drX", "X-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[1])
    updateSelectInput(session,"sc1b2drY", "Y-axis:",
                      choices = dataSource$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource$sc1def$dimred[2])
    updateSelectInput(session,"sc1b2sub1", "Cell information to subset:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)

    updateSelectizeInput(session, "sc1b2inp1", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene1, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1b2inp2", choices = sort(names(dataSource$sc1gene)), server = TRUE,
                         selected = dataSource$sc1def$gene2, options = list(
                           maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt)))

    updateSelectInput(session,"sc1c1inp1", "Cell information (X-axis):",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)
    updateSelectInput(session,"sc1c1inp1a", "Cell information to subset by:",
                      choices = c("N/A", dataSource$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")
    updateSelectizeInput(session, "sc1c1inp2", server = TRUE,
                         choices = c(dataSource$sc1conf[is.na(fID)]$UI,sort(names(dataSource$sc1gene))),
                         selected = dataSource$sc1conf[is.na(fID)]$UI[1], options = list(
                           maxOptions = length(dataSource$sc1conf[is.na(fID)]$UI) + 3,
                           create = TRUE, persist = TRUE, render = I(optCrt)))

    updateSelectInput(session,"sc1c2inp1", "Cell information to plot (X-axis):",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp2)
    updateSelectInput(session,"sc1c2inp2", "Cell information to group / colour by:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1def$grp1)

    updateSelectInput(session,"sc1d1grp", "Group by:",
                      choices = dataSource$sc1conf[grp == TRUE]$UI,
                      selected = dataSource$sc1conf[grp == TRUE]$UI[1])
    updateSelectInput(session,"sc1d1grp1a", "Cell information to subset by:",
                      choices = c("N/A", dataSource$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")

    updateTextAreaInput(session, "sc1d1inp",
                        value = paste0(dataSource$sc1def$genes, collapse = ", "))

    ### Plots for tab a1
    output$sc1a1sub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$sc1a1sub1]$fID, "\\|")
      if(length(sub)){
        sub <- sub[[1]]
      }
      checkboxGroupInput("sc1a1sub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$sc1a1oup1 <- renderPlot({
      scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
               input$sc1a1sub1, input$sc1a1sub2,
               input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
               input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1)
    })
    output$sc1a1oup1.ui <- renderUI({
      plotOutput("sc1a1oup1", height = pList[input$sc1a1psz])
    })
    output$sc1a1oup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w, useDingbats = FALSE,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
                        input$sc1a1sub1, input$sc1a1sub2,
                        input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1) )
      })
    output$sc1a1oup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
                        input$sc1a1sub1, input$sc1a1sub2,
                        input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1) )
      })
    output$sc1a1.dt <- renderDataTable({
      ggData = scDRnum(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1inp1, input$sc1a1inp2,
                       input$sc1a1sub1, input$sc1a1sub2,
                       dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$sc1a1splt)
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("pctExpress"), digits = 2)
    })

    output$sc1a1oup2 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
               input$sc1a1sub1, input$sc1a1sub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
               input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt,
               input$sc1a1type2, if(input$sc1a1xlim0 %% 2==0) 0 else input$sc1a1xlim1)
    })
    output$sc1a1oup2.ui <- renderUI({
      plotOutput("sc1a1oup2", height = pList[input$sc1a1psz])
    })
    output$sc1a1oup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
                        input$sc1a1sub1, input$sc1a1sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt,
                        input$sc1a1type2, if(input$sc1a1xlim0 %% 2==0) 0 else input$sc1a1xlim1) )
      })
    output$sc1a1oup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
                        input$sc1a1sub1, input$sc1a1sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt,
                        input$sc1a1type2, if(input$sc1a1xlim0 %% 2==0) 0 else input$sc1a1xlim1) )
      })


    ### Plots for tab a2
    output$sc1a2sub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$sc1a2sub1]$fID, "\\|")[[1]]
      checkboxGroupInput("sc1a2sub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$sc1a2oup1 <- renderPlot({
      scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp1,
               input$sc1a2sub1, input$sc1a2sub2,
               input$sc1a2siz, input$sc1a2col1, input$sc1a2ord1,
               input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, input$sc1a2lab1)
    })
    output$sc1a2oup1.ui <- renderUI({
      plotOutput("sc1a2oup1", height = pList[input$sc1a2psz])
    })
    output$sc1a2oup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                     input$sc1a2inp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a2oup1.h, width = input$sc1a2oup1.w, useDingbats = FALSE,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp1,
                        input$sc1a2sub1, input$sc1a2sub2,
                        input$sc1a2siz, input$sc1a2col1, input$sc1a2ord1,
                        input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, input$sc1a2lab1) )
      })
    output$sc1a2oup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                     input$sc1a2inp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a2oup1.h, width = input$sc1a2oup1.w,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp1,
                        input$sc1a2sub1, input$sc1a2sub2,
                        input$sc1a2siz, input$sc1a2col1, input$sc1a2ord1,
                        input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, input$sc1a2lab1) )
      })

    output$sc1a2oup2 <- renderPlot({
      scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp2,
               input$sc1a2sub1, input$sc1a2sub2,
               input$sc1a2siz, input$sc1a2col2, input$sc1a2ord2,
               input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, input$sc1a2lab2)
    })
    output$sc1a2oup2.ui <- renderUI({
      plotOutput("sc1a2oup2", height = pList[input$sc1a2psz])
    })
    output$sc1a2oup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                     input$sc1a2inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a2oup2.h, width = input$sc1a2oup2.w, useDingbats = FALSE,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp2,
                        input$sc1a2sub1, input$sc1a2sub2,
                        input$sc1a2siz, input$sc1a2col2, input$sc1a2ord2,
                        input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, input$sc1a2lab2) )
      })
    output$sc1a2oup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a2drX,"_",input$sc1a2drY,"_",
                                     input$sc1a2inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a2oup2.h, width = input$sc1a2oup2.w,
        plot = scDRcell(dataSource$sc1conf, dataSource$sc1meta, input$sc1a2drX, input$sc1a2drY, input$sc1a2inp2,
                        input$sc1a2sub1, input$sc1a2sub2,
                        input$sc1a2siz, input$sc1a2col2, input$sc1a2ord2,
                        input$sc1a2fsz, input$sc1a2asp, input$sc1a2txt, input$sc1a2lab2) )
      })


    ### Plots for tab a3
    output$sc1a3sub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$sc1a3sub1]$fID, "\\|")[[1]]
      checkboxGroupInput("sc1a3sub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$sc1a3oup1 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp1,
               input$sc1a3sub1, input$sc1a3sub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$sc1a3siz, input$sc1a3col1, input$sc1a3ord1,
               input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt)
    })
    output$sc1a3oup1.ui <- renderUI({
      plotOutput("sc1a3oup1", height = pList[input$sc1a3psz])
    })
    output$sc1a3oup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                     input$sc1a3inp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a3oup1.h, width = input$sc1a3oup1.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp1,
                        input$sc1a3sub1, input$sc1a3sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1a3siz, input$sc1a3col1, input$sc1a3ord1,
                        input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt) )
      })
    output$sc1a3oup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                     input$sc1a3inp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a3oup1.h, width = input$sc1a3oup1.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp1,
                        input$sc1a3sub1, input$sc1a3sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1a3siz, input$sc1a3col1, input$sc1a3ord1,
                        input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt) )
      })

    output$sc1a3oup2 <- renderPlot({
      scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp2,
               input$sc1a3sub1, input$sc1a3sub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$sc1a3siz, input$sc1a3col2, input$sc1a3ord2,
               input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt)
    })
    output$sc1a3oup2.ui <- renderUI({
      plotOutput("sc1a3oup2", height = pList[input$sc1a3psz])
    })
    output$sc1a3oup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                     input$sc1a3inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a3oup2.h, width = input$sc1a3oup2.w, useDingbats = FALSE,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp2,
                        input$sc1a3sub1, input$sc1a3sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1a3siz, input$sc1a3col2, input$sc1a3ord2,
                        input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt) )
      })
    output$sc1a3oup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1a3drX,"_",input$sc1a3drY,"_",
                                     input$sc1a3inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a3oup2.h, width = input$sc1a3oup2.w,
        plot = scDRgene(dataSource$sc1conf, dataSource$sc1meta, input$sc1a3drX, input$sc1a3drY, input$sc1a3inp2,
                        input$sc1a3sub1, input$sc1a3sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1a3siz, input$sc1a3col2, input$sc1a3ord2,
                        input$sc1a3fsz, input$sc1a3asp, input$sc1a3txt) )
      })


    ### Plots for tab b2
    output$sc1b2sub1.ui <- renderUI({
      sub = strsplit(dataSource$sc1conf[UI == input$sc1b2sub1]$fID, "\\|")[[1]]
      checkboxGroupInput("sc1b2sub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    output$sc1b2oup1 <- renderPlot({
      scDRcoex(dataSource$sc1conf, dataSource$sc1meta, input$sc1b2drX, input$sc1b2drY,
               input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, input$sc1b2sub2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
               input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1,
               input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt)
    })
    output$sc1b2oup1.ui <- renderUI({
      plotOutput("sc1b2oup1", height = pList2[input$sc1b2psz])
    })
    output$sc1b2oup1.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1b2drX,"_",input$sc1b2drY,"_",
                                     input$sc1b2inp1,"_",input$sc1b2inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1b2oup1.h, width = input$sc1b2oup1.w, useDingbats = FALSE,
        plot = scDRcoex(dataSource$sc1conf, dataSource$sc1meta, input$sc1b2drX, input$sc1b2drY,
                        input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, input$sc1b2sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1,
                        input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt) )
      })
    output$sc1b2oup1.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1b2drX,"_",input$sc1b2drY,"_",
                                     input$sc1b2inp1,"_",input$sc1b2inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1b2oup1.h, width = input$sc1b2oup1.w,
        plot = scDRcoex(dataSource$sc1conf, dataSource$sc1meta, input$sc1b2drX, input$sc1b2drY,
                        input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, input$sc1b2sub2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                        input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1,
                        input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt) )
      })
    output$sc1b2oup2 <- renderPlot({
      scDRcoexLeg(input$sc1b2inp1, input$sc1b2inp2, input$sc1b2col1, input$sc1b2fsz)
    })
    output$sc1b2oup2.ui <- renderUI({
      plotOutput("sc1b2oup2", height = "300px")
    })
    output$sc1b2oup2.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1b2drX,"_",input$sc1b2drY,"_",
                                     input$sc1b2inp1,"_",input$sc1b2inp2,"_leg.pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = 3, width = 4, useDingbats = FALSE,
        plot = scDRcoexLeg(input$sc1b2inp1, input$sc1b2inp2, input$sc1b2col1, input$sc1b2fsz) )
      })
    output$sc1b2oup2.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1b2drX,"_",input$sc1b2drY,"_",
                                     input$sc1b2inp1,"_",input$sc1b2inp2,"_leg.png") },
      content = function(file) { ggsave(
        file, device = "png", height = 3, width = 4,
        plot = scDRcoexLeg(input$sc1b2inp1, input$sc1b2inp2, input$sc1b2col1, input$sc1b2fsz) )
      })
    output$sc1b2.dt <- renderDataTable({
      ggData = scDRcoexNum(dataSource$sc1conf, dataSource$sc1meta, input$sc1b2inp1, input$sc1b2inp2,
                           input$sc1b2sub1, input$sc1b2sub2, dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene)
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("percent"), digits = 2)
    })


    ### Plots for tab c1
    output$sc1c1inp1b.ui <- renderUI({
      if(input$sc1c1inp1a!="N/A"){
        sub = strsplit(dataSource$sc1conf[UI == input$sc1c1inp1a]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1c1inp1b", "Select which cells to show", inline = TRUE,
                           choices = sub)
      }else{
        sub = NULL
      }
    })
    output$sc1c1inp1c.ui <- renderUI({
      if(!input$sc1c1inp2 %in% dataSource$sc1conf$UI){
        h5file <- H5File$new(file.path(datafolder, dataSource$dataset, "sc1gexpr.h5"), mode = "r")
        h5data <- h5file[["grp"]][["data"]]
        val = h5data$read(args = list(dataSource$sc1gene[input$sc1c1inp2], quote(expr=)))
        val <- max(val, na.rm = TRUE)
        h5file$close_all()
      }else{
          val = dataSource$sc1meta[[dataSource$sc1conf[UI == input$sc1c1inp2]$ID]]
          val <- max(val, na.rm = TRUE)
      }
      if(val<=1) maxv <- round(val, digits = 3)
      if(val>1 && val<=10) maxv <- round(val, digits = 1)
      if(val>10) maxv <- round(val, digits = 0)
      sliderInput("sc1c1inp1c", "Filter the cells by value",
                  min = 0, max = maxv, value = 0)
    })
    output$sc1c1oup <- renderPlot({
      scVioBox(dataSource$sc1conf, dataSource$sc1meta, input$sc1c1inp1, input$sc1c1inp1a, input$sc1c1inp1b, input$sc1c1inp1c, input$sc1c1inp2,
               dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$sc1c1typ, input$sc1c1pts,
               input$sc1c1siz, input$sc1c1fsz)
    })
    output$sc1c1oup.ui <- renderUI({
      plotOutput("sc1c1oup", height = pList2[input$sc1c1psz])
    })
    output$sc1c1oup.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$sc1c1typ,"_",input$sc1c1inp1,"_",
                                     input$sc1c1inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1c1oup.h, width = input$sc1c1oup.w, useDingbats = FALSE,
        plot = scVioBox(dataSource$sc1conf, dataSource$sc1meta, input$sc1c1inp1, input$sc1c1inp1a, input$sc1c1inp1b, input$sc1c1inp1c, input$sc1c1inp2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$sc1c1typ, input$sc1c1pts,
                        input$sc1c1siz, input$sc1c1fsz) )
      })
    output$sc1c1oup.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$sc1c1typ,"_",input$sc1c1inp1,"_",
                                     input$sc1c1inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1c1oup.h, width = input$sc1c1oup.w,
        plot = scVioBox(dataSource$sc1conf, dataSource$sc1meta, input$sc1c1inp1, input$sc1c1inp1a, input$sc1c1inp1b, input$sc1c1inp1c, input$sc1c1inp2,
                        dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene, input$sc1c1typ, input$sc1c1pts,
                        input$sc1c1siz, input$sc1c1fsz) )
      })


    ### Plots for tab c2
    output$sc1c2oup <- renderPlot({
      scProp(dataSource$sc1conf, dataSource$sc1meta, input$sc1c2inp1, input$sc1c2inp2,
             input$sc1c2typ, input$sc1c2flp, input$sc1c2fsz)
    })
    output$sc1c2oup.ui <- renderUI({
      plotOutput("sc1c2oup", height = pList2[input$sc1c2psz])
    })
    output$sc1c2oup.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$sc1c2typ,"_",input$sc1c2inp1,"_",
                                     input$sc1c2inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1c2oup.h, width = input$sc1c2oup.w, useDingbats = FALSE,
        plot = scProp(dataSource$sc1conf, dataSource$sc1meta, input$sc1c2inp1, input$sc1c2inp2,
                      input$sc1c2typ, input$sc1c2flp, input$sc1c2fsz) )
      })
    output$sc1c2oup.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_", input$sc1c2typ,"_",input$sc1c2inp1,"_",
                                     input$sc1c2inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1c2oup.h, width = input$sc1c2oup.w,
        plot = scProp(dataSource$sc1conf, dataSource$sc1meta, input$sc1c2inp1, input$sc1c2inp2,
                      input$sc1c2typ, input$sc1c2flp, input$sc1c2fsz) )
      })
    output$sc1c2.dt <- renderDataTable({
      ggData = scProp(dataSource$sc1conf, dataSource$sc1meta, input$sc1c2inp1, input$sc1c2inp2,
                      input$sc1c2typ, input$sc1c2flp, input$sc1c2fsz)
      datatable(ggData$data, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel")))
    })


    ### Plots for tab d1
    output$sc1d1grp1b.ui <- renderUI({
      if(input$sc1d1grp1a!="N/A"){
        sub = strsplit(dataSource$sc1conf[UI == input$sc1d1grp1a]$fID, "\\|")[[1]]
        checkboxGroupInput("sc1d1grp1b", "Select which cells to show", inline = TRUE,
                           choices = sub)
      }else{
        sub = NULL
      }
    })
    output$sc1d1oupTxt <- renderUI({
      geneList = scGeneList(input$sc1d1inp, dataSource$sc1gene)
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
    output$sc1d1oup <- renderPlot({
      scBubbHeat(dataSource$sc1conf, dataSource$sc1meta, input$sc1d1inp, input$sc1d1grp,
                 input$sc1d1grp1a, input$sc1d1grp1b, input$sc1d1plt,
                 dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                 input$sc1d1scl, input$sc1d1row, input$sc1d1col,
                 input$sc1d1cols, input$sc1d1fsz)
    })
    output$sc1d1oup.ui <- renderUI({
      plotOutput("sc1d1oup", height = pList3[input$sc1d1psz])
    })
    output$sc1d1oup.pdf <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1d1plt,"_",input$sc1d1grp,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1d1oup.h, width = input$sc1d1oup.w,
        plot = scBubbHeat(dataSource$sc1conf, dataSource$sc1meta, input$sc1d1inp, input$sc1d1grp,
                          input$sc1d1grp1a, input$sc1d1grp1b, input$sc1d1plt,
                          dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                          input$sc1d1scl, input$sc1d1row, input$sc1d1col,
                          input$sc1d1cols, input$sc1d1fsz, save = TRUE) )
      })
    output$sc1d1oup.png <- downloadHandler(
      filename = function() { paste0(input$availableDatasets, "_",input$sc1d1plt,"_",input$sc1d1grp,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1d1oup.h, width = input$sc1d1oup.w,
        plot = scBubbHeat(dataSource$sc1conf, dataSource$sc1meta, input$sc1d1inp, input$sc1d1grp,
                          input$sc1d1grp1a, input$sc1d1grp1b, input$sc1d1plt,
                          dataSource$dataset, "sc1gexpr.h5", dataSource$sc1gene,
                          input$sc1d1scl, input$sc1d1row, input$sc1d1col,
                          input$sc1d1cols, input$sc1d1fsz, save = TRUE) )
      })
  }
}

shinyApp(ui=ui, server = server)



