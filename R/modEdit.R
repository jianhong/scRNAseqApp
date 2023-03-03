#' @importFrom DT DTOutput
editUI <- function (id) {
    ns <- NS(id)
    tagList(column(
        width = 10,
        offset = 1,
        fluidRow(
            column(
                width = 6,
                selectInput(ns("dir"),
                            label = "Available data",
                            choices = c()),
                actionButton(
                    ns("refresh"),
                    label = "Refresh available data",
                    icon = icon("refresh"),
                    inline = TRUE
                )
            ),
            column(
                width = 6,
                actionButton(
                    ns('edit'),
                    label = "Apply change",
                    icon = icon("edit")),
                actionButton(
                    ns('reset'),
                    label = "Rollback",
                    icon = icon("refresh")
                ),
                actionButton(
                    ns('delete'),
                    label = "Delete",
                    icon = icon("remove")
                ),
                textOutput(ns('message'))
            )
        ),
        fluidRow(
            column(
                width = 6,
                textAreaInput(
                    ns("title"),
                    label = "Title for the data"),
                checkboxInput(
                    ns("locker"),
                    label = "Require privilege for the data",
                    value = TRUE),
                selectInput(
                    ns("datatype"),
                    label = "Data type",
                    choices = c("scRNAseq", "scATACseq", "scMultiome"),
                    selected = "scRNAseq"
                ),
                selectInput(
                    ns("meta1"),
                    label = "Primary default meta to show",
                    choices = c()),
                selectInput(
                    ns("meta2"),
                    label = "Secondary default meta to show",
                    choices = c()),
                textInput(
                    ns("gene1"),
                    label = "Primary default gene to show"),
                textInput(
                    ns("gene2"),
                    label = "Secondary default gene to show"),
                selectInput(
                    ns("dimred1"),
                    label = "Primary default reduction dim to show",
                    choices = c()),
                selectInput(
                    ns("dimred2"),
                    label = "Secondary default reduction dim to show",
                    choices = c()),
                selectInput(
                    ns("grp1"),
                    label = "Primary default info to show",
                    choices = c()),
                selectInput(
                    ns("grp2"),
                    label = "Secondary default info to show",
                    choices = c())
            ),
            column(
                width = 6,
                selectInput(
                    ns("species"),
                    label = "Species",
                    choices = .globals$supported_organisms,
                    selected = NULL
                ),
                textInput(
                    ns("species2"),
                    label = "Other species"),
                checkboxGroupInput(
                    ns("meta_to_include"),
                    label = "Columns to include from the metadata"),
                textAreaInput(
                    ns("multigene"),
                    label = "Default genes to show in bubbleplot/heatmap"),
                textAreaInput(
                    ns("keywords"),
                    label = "The key words for this work"),
                textAreaInput(
                    ns("reference"),
                    label = "BibTeX string"),
                textInput(
                    ns("doi"),
                    label = "DOI"),
                textInput(
                    ns("pmid"),
                    label = "PubMed ID")
            )
        ),
        hr(),
        fluidRow(
            column(
                width = 3,
                h4("Assign cell cycle"),
                actionButton(
                    ns("tricycle"),
                    label = "Tricycle")
            ),
            column(
                width = 3,
                h4("Assign cell type"),
                selectInput(
                    ns("celldex"),
                    label = NULL,
                    choices = c(
                        "HumanPrimaryCellAtlasData",
                        "BlueprintEncodeData",
                        "MouseRNAseqData",
                        "ImmGenData",
                        "DatabaseImmuneCellExpressionData",
                        "NovershternHematopoieticData",
                        "MonacoImmuneData"
                    ),
                    selected = "HumanPrimaryCellAtlasData"
                ),
                actionButton(
                    ns("singler"),
                    label = "singleR")
            ),
            column(
                width = 3,
                h4("Trajectories"),
                actionButton(
                    ns("slingshot"),
                    label = "slingshot")
            ),
            column(
                width = 3,
                h4("Cell communications"),
                p('N/A')
            )
        ),
        hr(),
        fluidRow(
            h4("Danger Zone"),
            DTOutput(ns('conf')))
    ))
}
#' @importFrom DT renderDT JS
#' @importFrom S4Vectors SimpleList DataFrame
editServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        global <- reactiveValues(
            sc1conf_data = NULL,
            sc1conf_orig = NULL,
            sc1meta = NULL,
            sc1def = NULL,
            appconf = NULL,
            markers = NULL,
            metaAdditional = NULL,
            ref = NULL,
            locker = TRUE
        )
        updateSelectInput(
            session,
            "dir",
            choices = getDataSets(),
            selected = getDataSets()[1])
        observeEvent(input$refresh, {
            updateSelectInput(
                session,
                "dir",
                choices = getDataSets(),
                selected = getDataSets()[1])
        })
        observeEvent(input$multigene, {
            if (!is.list(global$markers)) {
                global$markers <- strsplit(input$multigene, "\\s+|,|;")[[1]]
            }
        })
        observeEvent(input$dir, {
            if (!is.null(input$dir)) {
                if (input$dir %in% getDataSets()) {
                    adminProcess({
                        global$sc1meta <- readData("sc1meta", input$dir)
                        global$sc1conf_orig <-
                            readData("sc1conf", input$dir)
                        global$sc1conf_orig$fID <-
                            formatfID_CL(global$sc1conf_orig$fID)
                        global$sc1conf_orig$fCL <-
                            formatfID_CL(global$sc1conf_orig$fCL)
                        global$sc1conf_data <- global$sc1conf_orig
                        cellInfo <- global$sc1conf_data$ID
                        dimred <-
                            gsub("_", "", cellInfo[global$sc1conf_data$dimred])
                        grp <- cellInfo[global$sc1conf_data$grp]
                        output$conf <- renderDT({
                            datatable(
                                global$sc1conf_data,
                                editable = TRUE,
                                options = list(columnDefs = list(
                                    list(
                                        targets = c(6, 7),
                                        render = JS(
                                            "function(data, type, row, meta){",
                                            " if(type === 'display'){",
                                            paste(
                                                "  return data",
                                                "? '<input type=\"checkbox\"",
                                                "checked/>'",
                                                ": '<input",
                                                "type=\"checkbox\"/>';"
                                            ),
                                            " }",
                                            " return data;",
                                            "}"
                                        )
                                    )
                                ))
                            )
                        })
                        updateCheckboxGroupInput(
                            session,
                            "meta_to_include",
                            label = "Columns to include from the metadata",
                            choices = cellInfo,
                            selected = cellInfo
                        )
                        global$sc1def <- readData("sc1def", input$dir)
                        updateTextInput(
                            session,
                            "gene1",
                            value = global$sc1def$gene1)
                        updateTextInput(
                            session,
                            "gene2",
                            value = global$sc1def$gene2)
                        updateTextAreaInput(
                            session,
                            "multigene",
                            value = paste(
                                global$sc1def$genes,
                                collapse = ", ")
                        )
                        updateSelectInput(
                            session,
                            "meta1",
                            choices = cellInfo,
                            selected = global$sc1def$meta1
                        )
                        updateSelectInput(
                            session,
                            "meta2",
                            choices = cellInfo,
                            selected = global$sc1def$meta2
                        )
                        updateSelectInput(
                            session,
                            "dimred1",
                            choices = dimred,
                            selected = global$sc1def$dimred[1]
                        )
                        updateSelectInput(
                            session,
                            "dimred2",
                            choices = dimred,
                            selected = global$sc1def$dimred[2]
                        )
                        updateSelectInput(
                            session,
                            "grp1",
                            choices = grp,
                            selected = global$sc1def$grp1
                        )
                        updateSelectInput(
                            session,
                            "grp2",
                            choices = grp,
                            selected = global$sc1def$grp2
                        )
                        global$appconf <- readData("appconf", input$dir)
                        if (length(global$appconf$markers)) {
                            global$markers <- global$appconf$markers
                        }
                        updateTextAreaInput(
                            session,
                            "keywords",
                            value = global$appconf$keywords)
                        updateTextAreaInput(
                            session,
                            "title",
                            value = global$appconf$title)
                        updateTextInput(
                            session, "dir",
                            value = global$appconf$id)
                        updateSelectInput(
                            session,
                            "datatype",
                            selected = global$appconf$type)
                        updateSelectInput(
                            session,
                            "species",
                            selected = global$appconf$species)
                        if (!global$appconf$species %in%
                            .globals$supported_organisms) {
                            updateTextInput(
                                session,
                                "species2",
                                value = global$appconf$species)
                        }
                        updateTextInput(
                            session,
                            "reference",
                            value = global$appconf$ref$bib)
                        updateTextInput(
                            session,
                            "doi",
                            value = global$appconf$ref$doi)
                        updateTextInput(
                            session,
                            "pmid",
                            value = global$appconf$ref$pmid)
                        global$locker <- checkLocker(input$dir)
                        updateCheckboxInput(
                            session, "locker",
                            value = global$locker)
                    }, "Loading data", "Done loading!")
                }
            }
        })
        observeEvent(input$delete, {
            showModal(modalDialog(
                tagList(p(
                    "Are you sure you want to delete the data: ",
                    input$dir)
                ),
                title = paste("Delete data", input$dir),
                footer = tagList(
                    actionButton(
                        NS(id, "confirmDelete"),
                        "Delete"),
                    modalButton("Cancel")
                )
            ))
        })
        observeEvent(input$confirmDelete, {
            req(input$dir)
            unlink(
                file.path(.globals$datafolder, input$dir),
                recursive = TRUE)
            updateSelectInput(
                session,
                "dir",
                choices = getDataSets(),
                selected = getDataSets()[1])
            removeModal()
        })
        observeEvent(input$doi, {
            updateRefById(
                "reference",
                "doi",
                GetBibEntryWithDOI,
                input,
                output,
                session)
        })
        observeEvent(input$pmid, {
            global$ref <- updateRefById(
                "reference",
                "pmid",
                GetPubMedByID,
                input,
                output,
                session)
        })
        
        observeEvent(input$conf_cell_edit, {
            row  <- input$conf_cell_edit$row
            clmn <- input$conf_cell_edit$col
            global$sc1conf_data[row, clmn] <- input$conf_cell_edit$value
            saveData(global$sc1conf_data, input$dir, "sc1conf")
        })
        
        observeEvent(input$reset, {
            adminProcess({
                global$sc1conf_data <- global$sc1conf_orig
                conf_orig <- global$sc1conf_orig
                conf_orig$fID <- formatfID_CL(conf_orig$fID, rev = TRUE)
                conf_orig$fCL <- formatfID_CL(conf_orig$fCL, rev = TRUE)
                saveData(global$sc1meta, input$dir, "sc1meta")
                saveData(conf_orig, input$dir, "sc1conf")
                saveData(global$appconf, input$dir, "appconf")
                saveData(global$sc1def, input$dir, "sc1def")
                if (global$locker) {
                    setLocker(input$dir)
                } else{
                    removeLocker(input$dir)
                }
            }, "Starting rollback", "Rollback done!")
        })
        
        getUpdatedMetaAndConf <- function() {
            meta <- global$sc1meta
            conf_new <- global$sc1conf_data
            conf_orig <- global$sc1conf_orig
            if (!identical(conf_new$ID, conf_orig$ID)) {
                colnames(meta)[match(conf_orig$ID, colnames(meta))] <-
                    conf_new$ID
            }
            conf_orig$fID <- formatfID_CL(conf_orig$fID, rev = TRUE)
            conf_orig$fCL <- formatfID_CL(conf_orig$fCL, rev = TRUE)
            conf_new$fID <- formatfID_CL(conf_new$fID, rev = TRUE)
            conf_new$fCL <- formatfID_CL(conf_new$fCL, rev = TRUE)
            if (!identical(conf_new$fID, conf_orig$fID)) {
                o_fID <- strsplit(conf_orig$fID, "\\|")
                n_fID <- strsplit(conf_new$fID, "\\|")
                if (!identical(lengths(o_fID), lengths(n_fID))) {
                    adminMsg("fID lengths are not identical!", "error")
                    return(NULL)
                }
                mapply(
                    o_fID,
                    n_fID,
                    conf_new$ID,
                    FUN = function(.old, .new, .id) {
                        if (!all(.old == .new, na.rm = TRUE)) {
                            for (i in which(.old != .new)) {
                                meta[meta[, .id] == .old[i], .id] <-
                                    .new[i]## need to check
                            }
                        }
                    }
                )
            }
            if (!identical(conf_new$fCL, conf_orig$fCL)) {
                o_fCL <- strsplit(conf_orig$fCL, "\\|")
                n_fCL <- strsplit(conf_new$fCL, "\\|")
                n_fID <- strsplit(conf_new$fID, "\\|")
                if (!identical(lengths(o_fCL), lengths(n_fCL))) {
                    adminMsg("fCL lengths are not identical!", "error")
                    return(NULL)
                }
                if (!identical(lengths(n_fID), lengths(n_fCL))) {
                    adminMsg(
                        "fCL lengths are not identical with fID!",
                        "error")
                    return(NULL)
                }
            }
            if (!is.null(global$metaAdditional)) {
                meta <- cbind(meta, do.call(cbind, global$metaAdditional))
            }
            return(list(meta = meta, config = conf_new))
        }
        getExprMetaConf <- function() {
            new_meta_conf <- getUpdatedMetaAndConf()
            meta <- new_meta_conf$meta
            rownames(meta) <- meta$sampleID
            meta$sampleID <- NULL
            gene <- readData("sc1gene", input$dir)
            expr <- readDataMatrix(input$dir, names(gene), rownames(meta))
            return(list(
                expr = expr,
                meta = meta,
                config = new_meta_conf$config
            ))
        }
        toSingleCellExperiment <- function(expr_meta_conf) {
            askNamespace("SingleCellExperiment", "S4Vectors")
            SingleCellExperiment::SingleCellExperiment(
                assays = S4Vectors::SimpleList(
                    counts = expr_meta_conf$expr,
                    logcounts = log2(expr_meta_conf$expr + 1)
                ),
                colData = S4Vectors::DataFrame(expr_meta_conf$meta),
                rowData = S4Vectors::DataFrame(
                    gene = rownames(expr_meta_conf$expr),
                    row.names = rownames(expr_meta_conf$expr)
                )
            )
        }
        findReductionMethod <- function() {
            global$sc1conf_data$ID[
                global$sc1conf_data$UI %in% global$sc1def$dimred]
        }
        getReductionData <- function() {
            data <- global$sc1meta[, mget(findReductionMethod())]
            rownames(data) <- global$sc1meta$sampleID
            data
        }
        findGrpIDs <- function() {
            x <- isolate(global$sc1conf_data$ID[global$sc1conf_data$grp])
            names(x) <- x
            x
        }
        addAditionalMeta <- function(additionalMeta) {
            global$metaAdditional <- c(
                global$metaAdditional,
                additionalMeta)
            cellInfo <- c(
                global$sc1conf_data$ID,
                names(global$metaAdditional))
            updateCheckboxGroupInput(
                session,
                "meta_to_include",
                choices = cellInfo,
                selected = cellInfo)
        }
        observeEvent(input$edit, {
            adminProcess({
                if (!identical(global$sc1conf_data, global$sc1conf_orig)) {
                    ## update meta if conf changed
                    new_meta_conf <- getUpdatedMetaAndConf()
                    saveData(new_meta_conf$meta, input$dir, "sc1meta")
                    saveData(
                        new_meta_conf$config,
                        input$dir,
                        "sc1conf")
                }
                sc1conf <- readData("sc1conf", input$dir)
                sc1conf <- sc1conf[sc1conf$ID %in% input$meta_to_include,]
                saveData(sc1conf, input$dir, "sc1conf")
                updateAppConf(input, reactive({
                    global
                }))
                updateDef(input)
                if (input$locker) {
                    setLocker(input$dir)
                } else{
                    removeLocker(input$dir)
                }
            }, "Starting Update.", "Update done!")
        })
        observeEvent(input$tricycle, {
            if (!input$species %in% c("Homo sapiens", "Mus musculus")) {
                adminMsg("Only support human and mouse data.", type = "error")
                return()
            }
            askNamespace("tricycle", "SingleCellExperiment", "S4Vectors")
            adminProcess({
                expr_meta_conf <- getExprMetaConf()
                gname.type <-
                    ifelse(
                        grepl("ENS", rownames(expr_meta_conf$expr)[1]),
                        "ENSEMBL",
                        "SYMBOL")
                species <- ifelse(
                    input$species == "Mus musculus",
                    "mouse",
                    "human")
                exp <- toSingleCellExperiment(expr_meta_conf)
                tricyle <- addTricycle(
                    exp = exp,
                    gname.type = gname.type,
                    species = species,
                    meta = expr_meta_conf$meta
                )
                addAditionalMeta(tricyle)
            }, "Starting Tricycle!", "Tricycle done!")
        })
        observeEvent(input$singler, {
            askNamespace(
                "SingleR",
                "celldex",
                "SingleCellExperiment",
                "S4Vectors")
            if (!input$species %in% c(
                "Homo sapiens",
                "Mus musculus",
                "Danio rerio")) {
                adminMsg(
                    "Only support human, mouse and fish data.",
                    type = "error")
                return()
            }
            adminProcess({
                try(attachNamespace("celldex"))
                ref <-
                    get(input$celldex,
                        envir = as.environment("package:celldex"))()
                expr_meta_conf <- getExprMetaConf()
                sce <- toSingleCellExperiment(expr_meta_conf)
                pred <- SingleR::SingleR(
                    test = sce,
                    ref = ref,
                    labels = ref$label.fine
                )
                ## assign singler to meta data
                addAditionalMeta(list(SingleR_labels = pred$labels))
            }, "Starting SingleR!", "SingleR done!")
        })
        observeEvent(input$slingshot, {
            adminProcess({
                askNamespace("slingshot")
                misc_slingshot <- NULL
                dimred <- getReductionData()
                # by group points
                grp_ids <- lapply(findGrpIDs(), function(id) {
                    data <- global$sc1meta[, get(id)]
                    names(data) <- global$sc1meta$sampleID
                    data
                })
                lineages <- addSlingshot(dimred, grp_ids)
                message("Assign slingshot to miscellaneous data")
                ## assign slingshot to meta data
                writeMisc(lineages, input$dir, "slingshot")
            }, "Starting SlingShot", "SlingShot done!")
        })
    })
}
