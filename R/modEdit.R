#' @importFrom DT DTOutput
editUI <- function (id) {
  ns <- NS(id)
  tagList(
    column(
      width = 10, offset = 1,
      fluidRow(
        column(
          width = 6,
          selectInput(ns("data"),
                      label = "Available data",
                      choices = c()),
          actionButton(ns("refresh"),
                       label = "Refresh available data",
                       icon = icon("refresh"),
                       inline=TRUE)
        ),
        column(
          width = 6,
          actionButton(ns('edit'), label = "Apply change", icon = icon("edit")),
          actionButton(ns('reset'), label = "Rollback", icon = icon("refresh")),
          actionButton(ns('delete'), label = "Delete", icon = icon("remove")),
          textOutput(ns('message'))
        )
      ),
      fluidRow(
        column(width = 6,
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
               selectInput(ns("meta1"),
                           label = "Primary default meta to show",
                           choices = c()),
               selectInput(ns("meta2"),
                           label = "Secondary default meta to show",
                           choices = c()),
               textInput(ns("gene1"),
                         label = "Primary default gene to show"),
               textInput(ns("gene2"),
                         label = "Secondary default gene to show"),
               selectInput(ns("dimred1"),
                           label = "Primary default reduction dim to show",
                           choices = c()),
               selectInput(ns("dimred2"),
                           label = "Secondary default reduction dim to show",
                           choices = c()),
               selectInput(ns("grp1"),
                         label = "Primary default info to show",
                         choices = c()),
               selectInput(ns("grp2"),
                         label = "Secondary default info to show",
                         choices = c()),
               checkboxInput(ns("save"),
                             label = "Save object for further analysis",
                             value = FALSE)),
        column(width = 6,
               selectInput(
                 ns("species"),
                 label = "Species",
                 choices = supported_organisms,
                 selected = NULL
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
      fluidRow(
        h4("Danger Zone"),
        DTOutput(ns('conf'))
      )
    )
  )
}
updateAppConf <- function(datafolder, input){
  appconf <- list(title=input$title,
                  id=input$dir,
                  species=input$species,
                  ref=list(
                    bib=input$reference,
                    doi=input$doi,
                    pmid=input$pmid
                  ),
                  types=input$datatype)
  if(!is.null(input$data)){
    if(input$data!=""){
      saveRDS(appconf, file.path(datafolder, input$data, "appconf.rds"))
    }
  }
}
updateDef <- function(datafolder, input){
  sc1def <- list(
    meta1 = input$meta1,
    meta2 = input$meta2,
    gene1 = input$gene1,
    gene2 = input$gene2,
    genes = input$multigene,
    dimred = c(input$dimred1, input$dimred2),
    grp1 = input$grp1,
    grp2 = input$grp2
  )
  if(!is.null(input$data)){
    if(input$data!=""){
      saveRDS(sc1def, file.path(datafolder, input$data, "sc1def.rds"))
    }
  }
}
formatfID_CL <- function(x, rev=FALSE){
  if(rev){
    x[!is.na(x)] <- gsub(";\\s+", "|", x[!is.na(x)])
    return(x)
  }
  x[!is.na(x)] <- gsub("\\|", "; ", x[!is.na(x)])
  x
}
#' @importFrom DT renderDT JS
editServer <- function(id, datafolder) {
  moduleServer(id, function(input, output, session){
    global <- reactiveValues(sc1conf_data=NULL,
                             sc1conf_orig=NULL,
                             sc1meta=NULL,
                             sc1def=NULL,
                             appconf=NULL,
                             locker=TRUE,
                             save=FALSE)
    updateSelectInput(session,
                      "data",
                      choices = getDataSets(datafolder),
                      selected = getDataSets(datafolder)[1])
    observeEvent(input$refresh, {
      updateSelectInput(session,
                        "data",
                        choices = getDataSets(datafolder),
                        selected = getDataSets(datafolder)[1])
    })
    observeEvent(input$data, {
      if(!is.null(input$data)){
        if(input$data %in% getDataSets(datafolder)){
          global$sc1meta <- readRDS(file.path(
            datafolder, input$data, "sc1meta.rds"
          ))
          global$sc1conf_orig <-
            readRDS(file.path(datafolder,
                              input$data,
                              "sc1conf.rds"))
          global$sc1conf_orig$fID <- formatfID_CL(global$sc1conf_orig$fID)
          global$sc1conf_orig$fCL <- formatfID_CL(global$sc1conf_orig$fCL)
          global$sc1conf_data <- global$sc1conf_orig
          cellInfo <- global$sc1conf_data$ID
          dimred <- gsub("_", "", cellInfo[global$sc1conf_data$dimred])
          grp <- cellInfo[global$sc1conf_data$grp]
          output$conf <- renderDT({
            datatable(global$sc1conf_data, editable = TRUE,
                      options = list(
                        columnDefs = list(
                          list(
                            targets = c(6, 7),
                            render = JS(
                              "function(data, type, row, meta){",
                              "  if(type === 'display'){",
                              "    return data ? '<input type=\"checkbox\" checked/>' : '<input type=\"checkbox\"/>';",
                              "  }",
                              "  return data;",
                              "}"
                            )
                          )
                        )
                      ))
          })
          updateCheckboxGroupInput(
            session,
            "meta_to_include",
            label = "Columns to include from the metadata",
            choices = cellInfo,
            selected = cellInfo
          )
          global$sc1def <- readRDS(file.path(datafolder,
                                             input$data,
                                             "sc1def.rds"))
          updateTextInput(session, "gene1", value=global$sc1def$gene1)
          updateTextInput(session, "gene2", value=global$sc1def$gene2)
          updateTextAreaInput(session, "multigene",
                              value=paste(global$sc1def$genes,
                                          collapse = ", "))
          updateSelectInput(session, "meta1", choices =cellInfo,
                            selected = global$sc1def$meta1)
          updateSelectInput(session, "meta2", choices = cellInfo,
                            selected = global$sc1def$meta2)
          updateSelectInput(session, "dimred1", choices =dimred,
                            selected = global$sc1def$dimred[1])
          updateSelectInput(session, "dimred2", choices = dimred,
                            selected = global$sc1def$dimred[2])
          updateSelectInput(session, "grp1", choices =grp,
                            selected = global$sc1def$grp1)
          updateSelectInput(session, "grp2", choices = grp,
                            selected = global$sc1def$grp2)
          global$appconf <- readRDS(file.path(datafolder,
                                              input$data,
                                              "appconf.rds"))
          updateTextInput(session, "title", value=global$appconf$title)
          updateTextInput(session, "dir", value=global$appconf$id)
          updateSelectInput(session, "datatype",
                            selected = global$appconf$types)
          updateSelectInput(session, "species",
                            selected = global$appconf$species)
          updateTextInput(session, "reference", value=global$appconf$ref$bib)
          updateTextInput(session, "doi", value=global$appconf$ref$doi)
          updateTextInput(session, "pmid", value=global$appconf$ref$pmid)
          global$locker <-
            file.exists(file.path(datafolder, input$data, "LOCKER"))
          updateCheckboxInput(
            session, "locker",
            value = global$locker)
          global$save <-
            file.exists(file.path(datafolder, input$data, "seu.rds"))
          updateCheckboxInput(
            session, "save",
            value = global$save)
        }
      }
    })
    observeEvent(input$delete,{
      showModal(modalDialog(
        tagList(
          p("Are you sure you want to delete the data: ",
            input$data)
        ),
        title=paste("Delete data", input$data),
        footer=tagList(
          actionButton(NS(id, "confirmDelete"),
                       "Delete"),
          modalButton("Cancel")
        )
      ))
    })
    observeEvent(input$confirmDelete, {
      req(input$data)
      unlink(file.path(datafolder, input$data), recursive = TRUE)
      updateSelectInput(session,
                        "data",
                        choices = getDataSets(datafolder),
                        selected = getDataSets(datafolder)[1])
      removeModal()
    })
    observeEvent(input$doi,{
      updateRefById("reference", "doi", GetBibEntryWithDOI,
                    input, output, session)
      })
    observeEvent(input$pmid,{
      updateRefById("reference", "pmid", GetPubMedByID,
                    input, output, session)
    })

    observeEvent(input$conf_cell_edit, {
      row  <- input$conf_cell_edit$row
      clmn <- input$conf_cell_edit$col
      global$sc1conf_data[row, clmn] <- input$conf_cell_edit$value
      saveRDS(global$sc1conf_data,
              file.path(datafolder, input$data, "sc1conf.rds"))
    })

    observeEvent(input$reset, {
      global$sc1conf_data <- global$sc1conf_orig
      saveRDS(global$sc1meta,
              file.path(datafolder, input$data, "sc1meta.rds"))
      saveRDS(global$sc1conf_orig,
              file.path(datafolder, input$data, "sc1conf.rds"))
      saveRDS(global$appconf,
              file.path(datafolder, input$data, "appconf.rds"))
      saveRDS(global$sc1def,
              file.path(datafolder, input$data, "sc1def.rds"))
      if(global$locker){
        writeLines(character(0),
                   file.path(datafolder, input$data, "LOCKER"))
      }else{
        unlink(file.path(datafolder, input$data, "LOCKER"))
      }
      adminMsg("Rollback done!", "message")
    })

    observeEvent(input$edit, {
      if(!identical(global$sc1conf_data, global$sc1conf_orig)){
        ## update meta if conf changed
        meta <- global$sc1meta
        conf_new <- global$sc1conf_data
        conf_orig <- global$sc1conf_orig
        if(!identical(conf_new$ID, conf_orig$ID)){
          colnames(meta)[match(conf_orig$ID, colnames(meta))] <- conf_new$ID
        }
        conf_orig$fID <- formatfID_CL(conf_orig$fID, rev = TRUE)
        conf_orig$fCL <- formatfID_CL(conf_orig$fCL, rev = TRUE)
        conf_new$fID <- formatfID_CL(conf_new$fID, rev = TRUE)
        conf_new$fCL <- formatfID_CL(conf_new$fCL, rev = TRUE)
        if(!identical(conf_new$fID, conf_orig$fID)){
          o_fID <- strsplit(conf_orig$fID, "\\|")
          n_fID <- strsplit(conf_new$fID, "\\|")
          if(!identical(lengths(o_fID), lengths(n_fID))){
            adminMsg("fID lengths are not identical!", "error")
            return(NULL)
          }
          mapply(o_fID, n_fID, conf_new$ID, FUN=function(.old, .new, .id){
            if(!all(.old==.new)){
              for(i in which(.old!=.new)){
                meta[meta[, .id]==.old[i], .id] <- .new[i]## need to check
              }
            }
          })
        }
        if(!identical(conf_new$fCL, conf_orig$fCL)){
          o_fCL <- strsplit(conf_orig$fCL, "\\|")
          n_fCL <- strsplit(conf_new$fCL, "\\|")
          n_fID <- strsplit(conf_new$fID, "\\|")
          if(!identical(lengths(o_fCL), lengths(n_fCL))){
            adminMsg("fCL lengths are not identical!", "error")
            return(NULL)
          }
          if(!identical(lengths(n_fID), lengths(n_fCL))){
            adminMsg("fCL lengths are not identical with fID!", "error")
            return(NULL)
          }
        }
        saveRDS(meta,
                file.path(datafolder, input$data, "sc1meta.rds"))
        saveRDS(conf_new,
                file.path(datafolder, input$data, "sc1conf.rds"))
      }
      sc1conf <- readRDS(file.path(datafolder, input$data, "sc1conf.rds"))
      sc1conf <- sc1conf[sc1conf$ID %in% input$meta_to_include, ]
      saveRDS(sc1conf,
              file.path(datafolder, input$data, "sc1conf.rds"))
      updateAppConf(datafolder, input)
      updateDef(datafolder, input)
      if(input$locker){
        writeLines(character(0),
                   file.path(datafolder, input$data, "LOCKER"))
      }else{
        unlink(file.path(datafolder, input$data, "LOCKER"))
      }
      if(input$save){
        adminMsg("There is no seurat object available", "warning")
      }else{
        unlink(file.path(datafolder, input$data, "seu.rds"))
      }
      adminMsg("Update done!", "message")
    })
  })
}
