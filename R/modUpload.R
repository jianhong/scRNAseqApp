uploadUI <- function (id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
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
                         label = "Secondary default gene to show"),
               checkboxInput(ns("save"),
                             label = "Save object for further analysis",
                             value = FALSE)),
        column(width = 6,
               selectInput(
                 ns("species"),
                 label = "Species",
                 choices = supported_organisms,
                 selected = "Homo sapiens"
               ),
               selectInput(
                 ns("gexAssay"),
                 label = "Gene expression assay",
                 choices = c("SCT", "RNA"),
                 selected = "SCT"
               ),
               selectInput(
                 ns("gexSlot"),
                 label = "Slot in single-cell assay",
                 choices = c("data", "scale.data", "counts"),
                 selected = "data"
               ),
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
      ),
      hr(),
      fluidRow(
        column(width = 3,
               h4("Assign cell cycle"),
               actionButton(
                 ns("cellcycle"),
                 label = "CellCycleScoring"
               ),
               actionButton(
                 ns("tricycle"),
                 label = "Tricycle"
               )),
        column(width = 3,
               h4("Assign cell type"),
               actionButton(
                 ns("singler"),
                 label = "singleR"
               ),
               actionButton(
                 ns("sctype"),
                 label = "scType"
               )),
        column(width = 3,
               h4("Trajectories"),
               actionButton(
                 ns("monocole"),
                 label = "monocole"
               ),
               actionButton(
                 ns("velocity"),
                 label = "velocity"
               ),
               actionButton(
                 ns("stream"),
                 label = "stream"
               ),
               actionButton(
                 ns("URD"),
                 label = "URD"
               )),
        column(width = 3,
               h4("Cell communications"),
               actionButton(
                 ns("cellchat"),
                 label = "CellChat"
               ),
               actionButton(
                 ns("nichenetr"),
                 label = "NicheNet"
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
#'  CellCycleScoring GetAssayData as.SingleCellExperiment
#' @importFrom ShinyCell makeShinyApp createConfig
#' @importFrom RefManageR GetBibEntryWithDOI GetPubMedByID
#' @importFrom utils head
uploadServer <- function(id, datafolder) {
  moduleServer(id, function(input, output, session){
    global <- reactiveValues(filepath=NULL,
                             seu=NULL)
    getSeuObj <- function(){
      seu <- global$seu
      DefaultAssay(seu) <- ifelse("SCT" %in% Assays(seu), "SCT", "RNA")
      seu
    }
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
      validate(need(is(global$seu, "Seurat"),
                    "Please upload a seurat object"))
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
      updateSelectInput(session, "gexAssay",
                        choices = assays,
                        selected = defaultAssay)
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
    observeEvent(input$gexAssay,{
      if(is(global$seu, "Seurat")){
        slots <- c("data", "scale.data", "counts")
        d <- lapply(slots, GetAssayData, object=global$seu)
        d <- lapply(d, nrow)
        d <- vapply(d, FUN=function(.d) .d>0, FUN.VALUE = logical(1L))
        slots <- slots[d]
        if(length(slots)==0){
          adminMsg("There is no slots named as data, scale.data or counts",
                   type = "error")
        }else{
          updateSelectInput(session, "gexSlot",
                            choices = slots,
                            selected = slots[1])
        }
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
                     gex.assay = input$gexAssay,
                     gex.slot = input$gexSlot,
                     shiny.title = input$title,
                     shiny.dir = file.path(datafolder, input$dir),
                     default.gene1 = input$gene1,
                     default.gene2 = input$gene2,
                     default.multigene = strsplit(input$multigene, "\\s+|,|;")[[1]])
        progress$set(message="set data config file", value=.97)
        appconf <- list(title=input$title,
                        id=input$dir,
                        species=input$species,
                        ref=list(
                          bib=input$reference,
                          doi=input$doi,
                          pmid=input$pmid
                        ),
                        types=input$datatype)
        saveRDS(appconf, file.path(datafolder, input$dir, "appconf.rds"))
        if(input$save){
          file.rename(input$file$datapath,
                      file.path(datafolder, input$dir, "seu.rds"))
        }
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
    observeEvent(input$cellcycle, {
      cc.genes.list <- readRDS(system.file("extdata", "cc.genes.list.rds",
                                           package="scRNAseqApp"))
      cc.genes <- cc.genes.list[[input$species]]
      isENS <- vapply(cc.genes, FUN=function(.ele){
        .ele <- unlist(.ele)
        sum(.ele %in% rownames(global$seu))
      }, FUN.VALUE = numeric(1L))
      cc.genes <- cc.genes[[which.max(isENS)]]
      tryCatch({
        seu <- getSeuObj()
        isolate(global$seu <- CellCycleScoring(seu,
                                               s.features = cc.genes$s.genes,
                                               g2m.features = cc.genes$g2m.genes,
                                               set.ident = FALSE))
        cellInfo <- colnames(global$seu[[]])
        updateCheckboxGroupInput(
          session,
          "meta_to_include",
          choices = cellInfo,
          selected = cellInfo
        )
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$tricycle, {
      if(!input$species %in% c("Homo sapiens", "Mus musculus")){
        adminMsg("Only support human and mouse data.", type="error")
        return()
      }
      if(!require("tricycle")){
        adminMsg("The tricycle package is required for this function!",
                 type = "error")
        return()
      }
      if(!require("SummarizedExperiment")){
        adminMsg("The SummarizedExperiment package is
                 required for this function!",
                 type = "error")
        return()
      }
      tryCatch({
        seu <- getSeuObj()
        exp <- as.SingleCellExperiment(seu)
        exp <- tricycle::project_cycle_space(
          exp,
          gname.type=ifelse(grepl("ENS", rownames(exp)[1]),
                            "ENSEMBL",
                            "SYMBOL"),
          species=ifelse(input$species=="Mus musculus",
                         "mouse",
                         "human")
        )
        exp <- tricycle::estimate_cycle_position(exp)
        stopifnot(identical(rownames(seu[[]]),
                            rownames(colData(exp))))
        seu$tricyclePosition <- colData(exp)$tricyclePosition
        seu$CCStage <- colData(exp)$CCStage
        isolate(global$seu <- seu)
        cellInfo <- colnames(global$seu[[]])
        updateCheckboxGroupInput(
          session,
          "meta_to_include",
          choices = cellInfo,
          selected = cellInfo
        )
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$monocole, {
      if(!require("monocle3")){
        adminMsg("The monocle3 package is required for this function!",
                 type = "error")
        return()
      }
      if(!require("SeuratWrappers")){
        adminMsg("The SeuratWrappers package is required for this function!",
                 type = "error")
        return()
      }
      tryCatch({
        seu <- getSeuObj()
        cds <- SeuratWrappers::as.cell_data_set(seu)
        reduction_method <- Reductions(seu)
        if("umap" %in% reduction_method){
          reduction_method <- "UMAP"
        }else{
          if("tsne" %in% reduction_method){
            reduction_method <- "tSNE"
          }else{
            if("pca" %in% reduction_method){
              reduction_method <- "PCA"
            }else{
              reduction_method <- "UMAP"
            }
          }
        }
        cds <- monocle3::cluster_cells(cds=cds,
                                        reduction_method = reduction_method)

        cds <- monocle3::learn_graph(cds)
        ## TODO, return the metadata back to seu
        getMiscData <- function(cds_x){
          p <- plot_cells(cds_x,
                          color_cells_by="pseudotime",
                          show_trajectory_graph=TRUE)

          # full data
          meta_data <- p$data
          #trajectory graph segment is layer 3
          segments_layer_data <- p$layers[[3]]$data
          # principal_points is layer 4
          principal_points_data <- p$layer[[4]]$data
          # leaves_lable is layer 6
          mst_leaf_nodes <- p$layers[[6]]$data
          # root lable is layer 8
          mst_root_nodes <- p$layers[[8]]$data
        }
        # by principal points
        ica_space_df <- t(cds@principal_graph_aux[[reduction_method]]$dp_mst)
        cds_x <- lapply(rownames(ica_space_df),
                        function(root_nodes){
                          order_cells(cds,
                                      reduction_method = reduction_method,
                                      root_pr_nodes = root_nodes)})
        # by group points
        root_cells <- rownames(seu[[]][seu$ident %in% 'Glial-0', , drop=FALSE])
        cds_x <- order_cells(cds, reduction_method = reduction_method,
                             root_cells = root_cells)

      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$cellchat, {
      if(!input$species %in% c("Homo sapiens", "Mus musculus", "Danio rerio")){
        adminMsg("Only support human, mouse and fish data.", type="error")
        return()
      }
      if(!require("CellChat")){
        adminMsg("The CellChat package is required for this function!",
                 type = "error")
        return()
      }
      if(!require("future")){
        adminMsg("The future package is required for this function!",
                 type = "error")
        return()
      }
      tryCatch({
        seu <- getSeuObj()
        cellchat <- CellChat::createCellChat(object = seu, group.by="CellType")
        cellchat <- CellChat::setIdent(cellchat, ident.use = "CellType")
        groupSize <- as.numeric(table(cellchat@idents))
        cellchat@DB <- switch(input$species,
                              'Homo sapiens'= CellChatDB.human,
                              'Mus musculus'= CellChatDB.mouse,
                              'Danio rerio' = CellChatDB.zebrafish)
        cellchat <- CellChat::subsetData(cellchat)
        future::plan("multiprocess", workers = future::availableWorkers())
        cellchat <- identifyOverExpressedGenes(cellchat)
        cellchat <- identifyOverExpressedInteractions(cellchat)
        cellchat <- computeCommunProb(cellchat)
        cellchat <- filterCommunication(cellchat, min.cells = 10)
        cellchat <- computeCommunProbPathway(cellchat)
        cellchat <- aggregateNet(cellchat)
        cellchat <- netAnalysis_computeCentrality(cellchat, slot.name = "netP")
        ## TODO, return the metadata back to seu
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$nichenetr, {
      if(!input$species %in% c("Homo sapiens", "Mus musculus", "Danio rerio")){
        adminMsg("Only support human, mouse and fish data.", type="error")
        return()
      }
      if(!require("CellChat")){
        adminMsg("The CellChat package is required for this function!",
                 type = "error")
        return()
      }
      if(!require("future")){
        adminMsg("The future package is required for this function!",
                 type = "error")
        return()
      }
      tryCatch({
        seu <- getSeuObj()

        ## TODO, return the metadata back to seu
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
  })
}
