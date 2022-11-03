## BiocManager::install(c("shinyhelper", "DT", "ggplot2", "ggrepel", "Matrix",
##                        "hdf5r", "ggdendro", "gridExtra", "ggridges"))
#' @import shiny
#' @import shinyhelper
#' @import data.table
#' @import Matrix
#' @import DT
#' @import magrittr
#' @import ggplot2
#' @import ggrepel
#' @import hdf5r
#' @import ggdendro
#' @import gridExtra
#' @import ggridges
# library(shiny)
# library(shinyhelper)
# library(data.table)
# library(Matrix)
# library(DT)
# library(magrittr)
# library(ggplot2)
# library(ggrepel)
# library(hdf5r)
# library(ggdendro)
# library(gridExtra)
# library(ggridges)

if(names(dev.cur())!= "null device") dev.off()
pdf(NULL)

#' @include lang.R
#' @include userdata.R
#' @include datalist.R

#library(sysfonts)
#font_paths(file.path(getwd(), "inst/extdata/fonts"))
#font_add(family = "Arial", regular = "Arial.ttf")
#library(showtext)
#showtext.auto()

#' @include define.R
#' @include g_legend.R
#' @include sctheme.R
#' @include sortLevels.R
#' @include loadData.R

### Common plotting functions
# source("R/scDRcell.R")
# source("R/scDRnum.R")
# source("R/scDRgene.R")
# source("R/scDRcoex.R")
# source("R/scDRcoexLeg.R")
# source("R/scDRcoexNum.R")
# source("R/scVioBox.R")
# source("R/scProp.R")
# source("R/scGeneList.R")
# source("R/scBubbHeat.R")

### load modules
## source("R/tab.R")
# source("R/tabUIs.R")
# source("R/helpers.R")
# source("R/downloaders.R")
## source("R/modules.R")

scRNAseqApp <- function(...){
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
        cellInfoGeneExprUI("cellInfoGeneExpr"),
        ### Tab: cellInfo vs cellInfo on dimRed
        cellInfoCellInfoUI("cellInfoCellInfo"),
        ### Tab: geneExpr vs geneExpr on dimRed
        geneExprGeneExprUI("geneExprGeneExpr"),
        ### Tab: Gene coexpression plot
        coExprUI("coExpr"),
        ### Tab: subset gene expr
        subsetGeneExprUI("subsetGeneExpr"),
        ### Tab: violinplot / boxplot
        plotVioBoxUI("vioBoxPlot"),
        ### Tab: Proportion plot
        plotProportionUI("proportion"),
        ### Tab: Multiple gene expr
        plotBubbleHeatmapUI("bubbleHeatmap"),
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
                                 sc1conf=NULL,
                                 sc1def=NULL,
                                 sc1gene=NULL,
                                 sc1meta=NULL,
                                 Logged=FALSE,
                                 terms=terms[["scRNAseq"]],
                                 Username="",
                                 Password="",
                                 token="")
    ## login
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
    ## parse query strings
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
    ## change dataset
    observeEvent(input$availableDatasets,{
      dataSource$dataset <- input$availableDatasets
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
    observeEvent(input$search, {
      if(input$search != '' && input$search != "search key words in data name"){
        key_words = strsplit(input$search, '\\s+')[[1]]
        key_words = gsub("[^a-zA-Z0-9._-]+", "", key_words)
        key_words <- paste(key_words, collapse='|')
        res_data <- lapply(appconf, function(.ele){
          if(grepl(key_words, paste(.ele$title, .ele$id, do.call(paste, .ele$ref)))){
            return(c(.ele$id, .ele$title))
          }else{
            return(NULL)
          }
        })
        ## update search_res
        if(!is.null(res_data)){
          output$search_res <- renderUI(HTML(
            paste("<ul>",
                  vapply(res_data, function(.ele){
                    return(paste0("<li><a href='?data='", .ele[1], "'>", .ele[2], "</a>"))
                  }, character(1L)),
                  "</ul>")
          ))
        }
      }
    })
    ## update visitor stats
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
      counter <- counter[as.numeric(difftime(as.Date(Sys.time()), counter, units = 'days'))<730]
      counter <- table(format(counter, "%y-%m"))
      counter <- as.data.frame(counter)
      ggplot(counter, aes(x=Var1, y=Freq)) +
        geom_bar(stat = "identity", fill="darkorchid4") +
        theme_minimal() + xlab("") + ylab("visitor counts") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    ## refresh data when change dataset
    refreshData <- function(input, output, session){
      hasRef <- !is.na(getRef(dataSource$dataset, "title"))
      if(dataSource$dataset %in% names(data_types)){
        dataSource$terms <- terms[[data_types[[dataSource$dataset]]]]
      }
      dataSource <- loadData(dataSource, datafolder)
      output$dataTitle <- renderUI({HTML(names(datasets)[datasets==input$availableDatasets])})
      output$ref_author <- renderText(ifelse(hasRef, getRef(dataSource$dataset, "authors"), ""))
      output$ref_title <- renderText(ifelse(hasRef, getRef(dataSource$dataset, "title"), ""))
      output$ref_journal <- renderText(ifelse(hasRef, getRef(dataSource$dataset, "journals"), ""))
      output$ref_year <- renderText(ifelse(hasRef, getRef(dataSource$dataset, "years"), ""))
      output$ref_pmid <- renderUI({
        if(hasRef){
          a(getRef(dataSource$dataset, "pmids"),
            href = paste0("https://www.ncbi.nlm.nih.gov/pubmed/",
                          getRef(dataSource$dataset, "pmids")))
        }else{
          br()
        }
      })

      ### Plots for tab cell info vs gene expression
      cellInfoGeneExprServer("cellInfoGeneExpr", reactive({dataSource}),
                             optCrt, input$availableDatasets)

      ### Plots for tab cell info vs cell info
      cellInfoCellInfoServer("cellInfoCellInfo", reactive({dataSource}),
                             optCrt, input$availableDatasets)

      ### Plots for tab gene expression vs gene expression
      geneExprGeneExprServer("geneExprGeneExpr", reactive({dataSource}),
                             optCrt, input$availableDatasets)

      ### Plots for tab co-expression
      coExprServer("coExpr", reactive({dataSource}),
                   optCrt, input$availableDatasets)

      ### Plots for tab subset
      subsetGeneExprServer("subsetGeneExpr", reactive({dataSource}),
                           optCrt, input$availableDatasets)

      ### Plots for tab violion
      plotVioBoxServer("vioBoxPlot", reactive({dataSource}),
                       optCrt, input$availableDatasets, datafolder)


      ### Plots for tab proportion
      plotProportionServer("proportion", reactive({dataSource}),
                           optCrt, input$availableDatasets)


      ### Plots for tab bubble heatmap
      plotBubbleHeatmapServer("bubbleHeatmap", reactive({dataSource}),
                              optCrt, input$availableDatasets)
    }
  }

  shinyApp(ui=ui, server = server, ...)
}




