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
                 ns("keywords"),
                 label = "The key words for this work"
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
                 ns("urd"),
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
                                value = format(bibentry, style = 'html'))
          }
          updateRefIDs(element, input, output, session)
          return(bibentry)
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
#'  `DefaultAssay<-` VariableFeatures Misc
#' @importFrom Seurat FindAllMarkers FindVariableFeatures ScaleData
#'  CellCycleScoring GetAssayData as.SingleCellExperiment
#' @importFrom ShinyCell makeShinyApp createConfig
#' @importFrom RefManageR GetBibEntryWithDOI GetPubMedByID
#' @importFrom utils head
#' @importFrom methods slot
uploadServer <- function(id, datafolder) {
  moduleServer(id, function(input, output, session){
    global <- reactiveValues(filepath = NULL,
                             seu = NULL,
                             config = NULL,
                             ref = NULL,
                             markers = NULL)
    getSeuObj <- function(){
      seu <- global$seu
      DefaultAssay(seu) <- ifelse("SCT" %in% Assays(seu), "SCT", "RNA")
      seu
    }
    getGrpIDs <- function(){
      global$config$ID[global$config$grp]
    }
    saveMisc <- function(slot){
      misc <- Misc(global$seu, slot)
      if(!is.null(misc)){
        saveRDS(misc,
                file.path(datafolder, input$dir,
                          paste0(slot, ".rds")))
      }
    }
    askNamespace <- function(...){
      pkgs <- list(...)
      lapply(pkgs, function(pkg){
        if(!requireNamespace(pkg)){
          adminMsg(paste("The", pkg, "package is required for this function!"),
                   type = "error")
          return()
        }
      })
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
      progress$set(message="Check scale.data slot",
                   value=10)
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
      progress$set(message="Find all markers",
                   value=20)
      markers <- FindAllMarkers(global$seu, only.pos=TRUE,
                                min.pct=.25, logfc.threshold =.25)
      markers <- split(markers, markers$cluster)
      markers <- lapply(markers, head, n=min(5, ceiling(50/length(markers))))
      markers <- lapply(markers, rownames)
      markers <- unique(unlist(markers))
      progress$set(message="Update all inputs",
                   value=99)
      if(length(markers)>1){
        updateTextAreaInput(
          session, "multigene",
          value = markers)
      }else{
        updateTextAreaInput(
          session, "multigene",
          value = rownames(global$seu)[seq.int(min(20, nrow(global$seu)))])
      }
      progress$close()
      on.exit()
    })
    observeEvent(input$meta_to_include, {
      global$config <- createConfig(
        global$seu,
        meta.to.include = unique(input$meta_to_include)
      )
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
    observeEvent(input$pmid,{
      global$ref <- updateRefById("reference", "pmid", GetPubMedByID,
                                  input, output, session)
    })
    observeEvent(input$multigene, {
      global$markers <- strsplit(input$multigene, "\\s+|,|;")[[1]]
    })
    observeEvent(input$upload, {
      if(!is.null(global$seu)){
          # Create a Progress object
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message="Create data by ShinyCell::makeShinyApp",
                     value=0)
        dir.create(file.path(datafolder, input$dir))
        makeShinyApp(global$seu,
                     scConf = global$config,
                     gex.assay = input$gexAssay,
                     gex.slot = input$gexSlot,
                     shiny.title = input$title,
                     shiny.dir = file.path(datafolder, input$dir),
                     default.gene1 = input$gene1,
                     default.gene2 = input$gene2,
                     default.multigene = global$markers)
        progress$set(message="set data config file", value=.97)
        updateAppConf(datafolder, input, reactive({global}))
        saveMisc("monocle3_pseudotime")
        saveMisc("cellchat")
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
    ## cell cycle
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
        adminMsg("Cell phase updated!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$tricycle, {
      if(!input$species %in% c("Homo sapiens", "Mus musculus")){
        adminMsg("Only support human and mouse data.", type="error")
        return()
      }
      askNamespace("tricycle", "SummarizedExperiment")
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
        adminMsg("Tricycle done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    ## cell type
    observeEvent(input$singler, {
      askNamespace("SingleR")
      tryCatch({
        seu <- getSeuObj()
        ## assign singler to meta data
        adminMsg("SingleR done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$sctype, {
      tryCatch({
        seu <- getSeuObj()
        misc_nichenetr <- NULL
        ## assign SingleR to meta data
        adminMsg("ScType done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    ## pseudo time
    observeEvent(input$monocole, {
      askNamespace("monocle3", "SeuratWrappers")
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
        ## return the metadata back to seu
        getMiscData <- function(cds_x){
          p <- plot_cells(cds_x,
                          color_cells_by="pseudotime",
                          show_trajectory_graph=TRUE)
          list(
            # full data
            meta_data = p$data[, !colnames(p$data) %in% global$config$ID,
                               drop=FALSE]
            ,#trajectory graph segment is layer 3
            segments_layer_data = p$layers[[3]]$data
            ,# principal_points is layer 4
            principal_points_data = p$layer[[4]]$data
            ,# leaves_lable is layer 6
            mst_leaf_nodes = p$layers[[6]]$data
            ,# root lable is layer 8
            mst_root_nodes = p$layers[[8]]$data
          )
        }
        # by principal points
        ica_space_df <- t(cds@principal_graph_aux[[reduction_method]]$dp_mst)
        cds_x <- lapply(rownames(ica_space_df),
                        function(root_nodes){
                          order_cells(cds,
                                      reduction_method = reduction_method,
                                      root_pr_nodes = root_nodes)})
        # by group points
        grp_ids <- getGrpIDs()
        root_cells <- lapply(grp_ids, function(.ele){
          split(rownames(seu[[]]), seu[[]][, .ele])
        })
        names(root_cells) <- grp_ids
        cds_x1 <- lapply(root_cells, function(.ele){
          lapply(.ele, function(.e){
            order_cells(cds,
                        reduction_method = reduction_method,
                        root_cells = .e)
          })
        })
        cds_x <- c(list(root_nodes=cds_x), cds_x1)
        miscData <- lapply(cds_x, function(.ele) lapply(.ele, getMiscData))
        ## assign pseudotime to miscellaneous data
        Misc(global$seu, "monocle3_pseudotime") <- miscData
        adminMsg("Monocle3 done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$velocity, {
      askNamespace("velocyto.R")
      tryCatch({
        seu <- getSeuObj()
        misc_velocyto <- NULL
        ## assign velocyto.R to miscellaneous data
        Misc(global$seu, "velocyto.R") <- misc_velocyto
        adminMsg("velocyto.R done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$stream, {
      askNamespace("stream")
      tryCatch({
        seu <- getSeuObj()
        misc_stream <- NULL
        ## assign stream to miscellaneous data
        Misc(global$seu, "stream") <- misc_stream
        adminMsg("stream done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$urd, {
      askNamespace("URD")
      tryCatch({
        seu <- getSeuObj()
        misc_urd <- NULL
        ## assign URD to miscellaneous data
        Misc(global$seu, "urd") <- misc_urd
        adminMsg("URD done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    ## cell communication
    observeEvent(input$cellchat, {
      if(!input$species %in% c("Homo sapiens", "Mus musculus", "Danio rerio")){
        adminMsg("Only support human, mouse and fish data.", type="error")
        return()
      }
      askNamespace("CellChat", "future")
      tryCatch({
        seu <- getSeuObj()
        grp_ids <- getGrpIDs()
        misc_cellchat <- lapply(grp_ids, function(grp){
          cellchat <- CellChat::createCellChat(object = seu, group.by=grp)
          cellchat <- CellChat::setIdent(cellchat, ident.use = grp)
          groupSize <- as.numeric(table(cellchat@idents))
          cellchat@DB <- switch(input$species,
                                'Homo sapiens'= CellChat::CellChatDB.human,
                                'Mus musculus'= CellChat::CellChatDB.mouse,
                                'Danio rerio' = CellChat::CellChatDB.zebrafish)
          cellchat <- CellChat::subsetData(cellchat)
          future::plan("multisession", workers =
                         max(1, length(future::availableWorkers())-1))
          cellchat <- CellChat::identifyOverExpressedGenes(cellchat)
          cellchat <- CellChat::identifyOverExpressedInteractions(cellchat)
          cellchat <- CellChat::computeCommunProb(cellchat)
          cellchat <- CellChat::filterCommunication(cellchat, min.cells = 10)
          cellchat <- CellChat::computeCommunProbPathway(cellchat)
          cellchat <- CellChat::aggregateNet(cellchat)
          cellchat <-
            CellChat::netAnalysis_computeCentrality(cellchat,
                                                    slot.name = "netP")
          slots <- c("LR", "net", "DB", "netP", "idents")
          names(slots) <- slots
          lapply(slots, function(.ele){
            slot(cellchat, name = .ele)
          })
        })
        names(misc_cellchat) <- grp_ids
        ## assign cellchat to miscellaneous data
        Misc(global$seu, "cellchat") <- misc_cellchat
        adminMsg("CellChat done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })
    observeEvent(input$nichenetr, {
      askNamespace("nichenetr")
      tryCatch({
        seu <- getSeuObj()
        misc_nichenetr <- NULL
        ## assign nichenetr to miscellaneous data
        Misc(global$seu, "nichenetr") <- misc_nichenetr
        adminMsg("nichenetr done!", type = "message", duration=5)
      },
      error = function(.e) adminMsg(.e, type = "error")
      )
    })

  })
}
