uploadUI <- function (id) {
  ns <- NS(id)
  tagList(
    column(
      width = 10, offset = 1,
      fluidRow(actionButton(ns('upload'),
                            label = "Upload",
                            icon = icon("upload")),
               textOutput(ns('message'))),
      fluidRow(
        column(width = 6,
               fileInput(ns("file"),
                         label = "Seurat object",
                         multiple = FALSE),
               textInput(ns("title"),
                         label = "Title for the data"),
               textInput(ns("dir"),
                         label = "data folder (must be unique)"),
               checkboxInput(ns("locker"),
                             label = "Require privilege for the data",
                             value = TRUE),
               selectInput(ns("datatype"),
                           label = "Data type",
                           choices = c("scRNAseq", "scATACseq"),
                           selected = "scRNAseq"),
               textInput(ns("gene1"),
                         label = "Primary default gene to show"),
               textInput(ns("gene2"),
                         label = "Secondary default gene to show")),
        column(width = 6,
               checkboxGroupInput(
                 ns("meta_to_include"),
                 label = "Columns to include from the metadata"
               ),
               textAreaInput(
                 ns("multigene"),
                 label = "Default genes to show in bubbleplot/heatmap"
               ),
               textAreaInput(
                 ns("reference"),
                 label = "BibTeX string"
               ),
               textInput(
                 ns("doi"),
                 label = "DOI"
               ),
               textInput(
                 ns("pmid"),
                 label = "PubMed ID"
               ))
      )
    )
  )
}

adminMsg <- function(msg, type, duration=30, close=TRUE){
  showNotification(toString(msg)[1], duration = duration,
                   closeButton = close,
                   type = type)
}
#' @importFrom xml2 read_xml as_list
idConverter <-
  function(id, type=c("doi", "pmid"),
           url='https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/'){
  type <- match.arg(type)
  res <- read_xml(paste0(url, "?format=xml&ids=", id))
  res <- as_list(res)
  if(!is.null(res$pmcids$record)){
    return(attr(res$pmcids$record, type))
  }
  return(NULL)
  }
updateRefIDs <- function(element, input, output, session){
  o_ele <- ifelse(element=="doi", "pmid", "doi")
  updateID <- FALSE
  updateID <-is.null(input[[o_ele]])
  if(!updateID){
    updateID <- gsub("\\s+", '', input[[o_ele]])==""
  }

  if(updateID){
    res <- idConverter(input[[element]], type=o_ele)
    print(res)
    if(!is.null(res)){
      updateTextInput(session,
                      o_ele,
                      value = res)
    }else{
      adminMsg(paste('Please update the correct', toupper(o_ele)), 'warning')
    }
  }
}

#' @importFrom methods is
updateRefById <- function(id, element, FUN, input, output, session){
  if(!is.null(input[[element]])){
    if(input[[element]]!=""){
      tryCatch(
        {
          bibentry <- FUN(input[[element]])
          if(is(bibentry, "bibentry")){
            updateTextAreaInput(session,
                                id,
                                value = format(bibentry, style = 'text'))
          }
          updateRefIDs(element, input, output, session)
        },
        error = function(e){
          adminMsg(e, 'error')
        },
        warning = function(w){
          adminMsg(w, 'warning')
        }
      )

    }
  }
}


#' @importFrom tools file_ext
#' @importFrom SeuratObject Reductions Idents Assays DefaultAssay GetAssayData
#'  `DefaultAssay<-` VariableFeatures
#' @importFrom Seurat FindAllMarkers FindVariableFeatures ScaleData
#' @importFrom ShinyCell makeShinyApp createConfig
#' @importFrom RefManageR GetBibEntryWithDOI GetPubMedByID
#' @importFrom utils head
uploadServer <- function(id, datafolder) {
  moduleServer(id, function(input, output, session){
    global <- reactiveValues(filepath=NULL,
                             seu=NULL)
    observeEvent(input$file, {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message="Uploading data",
                   value=0)
      file <- input$file
      ext <- tolower(file_ext(file$datapath))
      filename <- sub(paste0(".", ext), "", basename(file$name))
      uniqueFilename <- make.names(c(dir(datafolder), filename),
                                   unique = TRUE,
                                   allow_ = TRUE)[length(dir(datafolder))+1]
      req(file)
      validate(need(ext=="rds",
                    "Please upload a seurat object saved in rds file"))
      isolate(global$filepath <- file$datapath)
      isolate(global$seu <- readRDS(file$datapath))
      progress$set(message="Handling data",
                   value=5)
      updateTextInput(session, "dir",
                      value = uniqueFilename)
      updateTextInput(session, "title",
                      value = filename)
      cellInfo <- colnames(global$seu[[]])
      updateCheckboxGroupInput(
        session,
        "meta_to_include",
        label = "Columns to include from the metadata",
        choices = cellInfo,
        selected = cellInfo
      )
      assays <- Assays(global$seu)
      validate(need(any(c("SCT", "RNA") %in% assays),
                    "Please upload a seurat object with 'SCT' or 'RNA' assay"))
      if(!DefaultAssay(global$seu) %in% c("SCT", "RNA")){
        DefaultAssay(global$seu) <- match.arg(assays, choices = c("SCT", "RNA"),
                                       several.ok = TRUE)[1]
      }
      defaultAssay <- DefaultAssay(global$seu)
      if(length(GetAssayData(global$seu, "scale.data"))==0){
        global$seu <- FindVariableFeatures(global$seu, selection.method = "vst",
                                    nfeatures=1000)
        global$seu <- ScaleData(global$seu)
      }
      top2 <- head(VariableFeatures(global$seu), 2)
      if(length(top2)==2){
        updateTextInput(session, "gene1", value = top2[1])
        updateTextInput(session, "gene2", value = top2[2])
      }else{
        updateTextInput(session, "gene1", value = rownames(global$seu)[1])
        updateTextInput(session, "gene2", value = rownames(global$seu)[2])
      }
      markers <- FindAllMarkers(global$seu, only.pos=TRUE,
                                min.pct=.25, logfc.threshold =.25)
      markers <- split(markers, markers$cluster)
      markers <- lapply(markers, head, n=min(5, ceiling(50/length(markers))))
      markers <- lapply(markers, rownames)
      markers <- unique(unlist(markers))
      if(length(markers)>1){
        updateTextAreaInput(
          session, "multigene",
          value = markers)
      }else{
        updateTextAreaInput(
          session, "multigene",
          value = rownames(global$seu)[seq.int(min(20, nrow(global$seu)))])
      }
    })
    observeEvent(input$doi,
                 updateRefById("reference", "doi", GetBibEntryWithDOI,
                               input, output, session))
    observeEvent(input$pmid,
                 updateRefById("reference", "pmid", GetPubMedByID,
                               input, output, session))
    observeEvent(input$upload, {
      if(!is.null(global$seu)){
          # Create a Progress object
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message="Create data by ShinyCell::makeShinyApp",
                     value=0)
        dir.create(file.path(datafolder, input$dir))
        makeShinyApp(global$seu,
                     scConf = createConfig(
                       global$seu,
                       meta.to.include = unique(input$meta_to_include)
                     ),
                     shiny.title = input$title,
                     shiny.dir = file.path(datafolder, input$dir),
                     default.gene1 = input$gene1,
                     default.gene2 = input$gene2,
                     default.multigene = strsplit(input$multigene, "\\s+|,|;")[[1]])
        progress$set(message="set data config file", value=.97)
        appconf <- list(title=input$title,
                        id=input$dir,
                        ref=list(
                          bib=input$reference,
                          doi=input$doi,
                          pmid=input$pmid
                        ),
                        types=input$datatype)
        saveRDS(appconf, file.path(datafolder, input$dir, "appconf.rds"))
        progress$set(message="Check file LOCKER", value=.98)
        if(input$locker){
          writeLines("", file.path(datafolder, input$dir, "LOCKER"))
        }
        progress$set(message="Clean up unused files", value=.99)
        unlink(file.path(datafolder, input$dir, "ui.R"))
        unlink(file.path(datafolder, input$dir, "server.R"))
        updateTextInput(session, "dir",
                        value = "")
        updateTextInput(session, "title",
                        value = "")
        updateCheckboxGroupInput(
          session,
          "meta_to_include",
          label = "Columns to include from the metadata",
          choices = character(0),
          selected = character(0))
        updateTextInput(session, "gene1", value = "")
        updateTextInput(session, "gene2", value = "")
        updateTextAreaInput(
          session, "multigene",
          value = "")
        updateTextInput(session, "doi", value = "")
        updateTextInput(session, "pmid", value = "")
        updateTextInput(session, "reference", value = "")
        progress$close()
        on.exit()
        output$message <- renderText("Upload done!")
        adminMsg("Upload done!", "message")
      }
    })
  })
}
