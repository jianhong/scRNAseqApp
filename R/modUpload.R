uploadUI <- function (id) {
    ns <- NS(id)
    tagList(column(
        width = 10,
        offset = 1,
        fluidRow(
            actionButton(
                ns('upload'),
                label = "Upload",
                icon = icon("upload")
            ),
            textOutput(ns('message'))
        ),
        fluidRow(
            column(
                width = 6,
                fileInput(
                    ns("file"),
                    label = "Seurat object",
                    multiple = FALSE
                ),
                textInput(
                    ns("title"),
                    label = "Title for the data"
                ),
                textInput(
                    ns("dir"),
                    label = "data folder (must be unique)"
                ),
                checkboxInput(
                    ns("locker"),
                    label = "Require privilege for the data",
                    value = TRUE
                ),
                selectInput(
                    ns("datatype"),
                    label = "Data type",
                    choices = c("scRNAseq", "scATACseq", "scMultiome"),
                    selected = "scRNAseq"
                ),
                textInput(
                    ns("gene1"),
                    label = "Primary default gene to show"
                ),
                textInput(
                    ns("gene2"),
                    label = "Secondary default gene to show"
                ),
                div(class = "consoleOutput",
                    verbatimTextOutput(ns(
                        'consoleOutput'
                    )))
            ),
            column(
                width = 6,
                selectInput(
                    ns("species"),
                    label = "Species",
                    choices = .globals$supported_organisms,
                    selected = "Homo sapiens"
                ),
                textInput(
                    ns("species2"),
                    label = "Other species"
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
                selectInput(
                    ns("atacAssay"),
                    label = "ATAC peaks assay",
                    choices = c("ATAC", "peaks"),
                    selected = NULL
                ),
                selectInput(
                    ns("atacSlot"),
                    label = "Slot in ATAC assay",
                    choices = c("data", "scale.data", "counts"),
                    selected = "data"
                ),
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
                    ns("cellcycle"),
                    label = "CellCycleScoring"),
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
                p("N/A")
            )
        )
    ))
}

#' @importFrom tools file_ext
#' @importFrom SeuratObject Reductions Idents Assays DefaultAssay GetAssayData
#'  `DefaultAssay<-` VariableFeatures Misc `Misc<-` Embeddings `Idents<-`
#' @importFrom Seurat FindAllMarkers FindVariableFeatures ScaleData
#'  CellCycleScoring as.SingleCellExperiment
#' @importFrom RefManageR GetBibEntryWithDOI GetPubMedByID
#' @importFrom utils head
#' @importFrom methods slot
uploadServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        global <- reactiveValues(
            filepath = NULL,
            seu = NULL,
            config = NULL,
            ref = NULL,
            markers = NULL,
            tmpf = tempfile()
        )
        getSeuObj <- function() {
            seu <- global$seu
            DefaultAssay(seu) <-
                ifelse("SCT" %in% Assays(seu), "SCT", "RNA")
            seu
        }
        seuReductionMethod <- function(seu, forMonocole = TRUE) {
            reduction_method <- Reductions(seu)
            getReductionMethod(reduction_method, forMonocole)
        }
        saveMisc <- function(slot) {
            misc <- Misc(global$seu, slot)
            writeMisc(misc, input$dir, slot)
        }
        observeEvent(input$file, {
            adminProcess({
                file <- input$file
                ext <- tolower(file_ext(file$datapath))
                filename <- sub(paste0(".", ext), "", basename(file$name))
                uniqueFilename <-
                    make.names(
                        c(dir(.globals$datafolder), filename),
                        unique = TRUE,
                        allow_ = TRUE)[
                            length(dir(.globals$datafolder)) + 1]
                req(file)
                validate(need(
                    ext == "rds",
                    "Please upload a seurat object saved in rds file"
                ))
                isolate(global$filepath <- file$datapath)
                isolate(global$seu <- readRDS(file$datapath))
                validate(need(
                    is(global$seu, "Seurat"),
                    "Please upload a seurat object"
                ))
                message("Handling data")
                updateTextInput(
                    session, "dir",
                    value = uniqueFilename)
                updateTextInput(
                    session, "title",
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
                validate(need(
                    any(c("SCT", "RNA") %in% assays),
                    "Please upload a seurat object with 'SCT' or 'RNA' assay"
                ))
                if (!DefaultAssay(global$seu) %in% c("SCT", "RNA")) {
                    DefaultAssay(global$seu) <-
                        match.arg(
                            assays,
                            choices = c("SCT", "RNA"),
                            several.ok = TRUE
                        )[1]
                }
                defaultAssay <- DefaultAssay(global$seu)
                updateSelectInput(
                    session,
                    "gexAssay",
                    choices = assays,
                    selected = defaultAssay)
                message("Check scale.data slot")
                if (length(extAssayData(
                    global$seu, slot = "scale.data")) == 0) {
                    global$seu <- FindVariableFeatures(
                        global$seu,
                        selection.method = "vst",
                        nfeatures = 1000
                    )
                    global$seu <- ScaleData(global$seu)
                }
                top2 <- head(VariableFeatures(global$seu), 2)
                if (length(top2) == 2) {
                    updateTextInput(session, "gene1", value = top2[1])
                    updateTextInput(session, "gene2", value = top2[2])
                } else{
                    updateTextInput(
                        session, "gene1",
                        value = rownames(global$seu)[1])
                    updateTextInput(
                        session, "gene2",
                        value = rownames(global$seu)[2])
                }
                message("Find all markers")
                if (!is.null(Misc(global$seu, "markers"))) {
                    ## the markers is available at Misc(seu, "markers") slot
                    markers <- Misc(global$seu, "markers")
                    if (is.list(markers) && !is.data.frame(markers)) {
                        markers <- as.data.frame(markers[[1]])
                    }
                } else{
                    if (!is.factor(Idents(global$seu))) {
                        grp <- cellInfo[
                            grepl(
                                'cluster|cell(.*)type',
                                cellInfo,
                                ignore.case = TRUE)]
                        grp_d <- adist('celltype', grp)
                        Idents(global$seu) <- grp[which.min(grp_d)][1]
                    }
                    markers <- FindAllMarkers(
                        global$seu,
                        only.pos = TRUE,
                        min.pct = .25,
                        logfc.threshold = .25
                    )
                    if (length(markers)) {
                        Misc(global$seu, "markers") <- markers
                    }
                }
                markers <- split(markers, markers$cluster)
                markers <-
                    lapply(markers, head, n = min(5, ceiling(50 / length(
                        markers
                    ))))
                markers <- lapply(markers, function(.ele)
                    (
                        if (!is.null(.ele$gene)) {
                            return(.ele$gene)
                        } else{
                            return(rownames(.ele))
                        }))
                markers <- unique(unlist(markers))
                message("Update all inputs")
                if (length(markers) > 1) {
                    updateTextAreaInput(
                        session, "multigene",
                        value = markers)
                } else{
                    updateTextAreaInput(
                        session,
                        "multigene",
                        value = rownames(global$seu)[
                            seq.int(min(20, nrow(global$seu)))])
                }
            }, "Uploading data", "Preprocessing Done!")
        })
        observeEvent(input$meta_to_include, {
            global$config <-
                createConfig(
                    global$seu,
                    meta.to.include = unique(input$meta_to_include))
        })
        observeEvent(input$gexAssay, {
            if (is(global$seu, "Seurat")) {
                slots <- c("data", "scale.data", "counts")
                d <- lapply(slots, function(.ele){
                    extAssayData(object = global$seu, slot=.ele)
                    })
                d <- lapply(d, nrow)
                d <-
                    vapply(
                        d,
                        FUN = function(.d)
                            .d > 0,
                        FUN.VALUE = logical(1L)
                    )
                slots <- slots[d]
                if (length(slots) == 0) {
                    adminMsg(
                        "There is no slots named as data, scale.data or counts",
                        type = "error")
                } else{
                    updateSelectInput(
                        session,
                        "gexSlot",
                        choices = slots,
                        selected = slots[1])
                }
            }
        })
        observeEvent(
            input$doi,
            updateRefById(
                "reference",
                "doi",
                GetBibEntryWithDOI,
                input,
                output,
                session
            )
        )
        observeEvent(input$pmid, {
            global$ref <- updateRefById(
                "reference",
                "pmid",
                GetPubMedByID,
                input,
                output,
                session)
        })
        observeEvent(input$multigene, {
            global$markers <- strsplit(input$multigene, "\\s+|,|;")[[1]]
        })
        observeEvent(input$upload, {
            if (!is.null(global$seu)) {
                adminProcess({
                    dir.create(file.path(.globals$datafolder, input$dir))
                    message("set data config file")
                    updateAppConf(input, reactive({
                        global
                    }))
                    message("makeShinyApp")
                    makeShinyFiles(
                        global$seu,
                        scConf = global$config,
                        assayName = input$gexAssay,
                        gexSlot = input$gexSlot,
                        atacAssayName = input$atacAssay,
                        atacSlot = input$atacSlot,
                        appDir = file.path(.globals$datafolder, input$dir),
                        defaultGene1 = input$gene1,
                        defaultGene2 = input$gene2,
                        default.multigene = global$markers
                    )
                    message("set misc files")
                    for (slot in names(Misc(global$seu))) {
                        saveMisc(slot)
                    }
                    message("Check file LOCKER")
                    if (input$locker) {
                        setLocker(input$dir)
                    }
                    message("reset the inputs")
                    updateTextInput(session, "dir",
                                    value = "")
                    updateTextInput(session, "title",
                                    value = "")
                    updateCheckboxGroupInput(
                        session,
                        "meta_to_include",
                        label = "Columns to include from the metadata",
                        choices = character(0),
                        selected = character(0)
                    )
                    updateTextInput(session, "gene1", value = "")
                    updateTextInput(session, "gene2", value = "")
                    updateTextAreaInput(session, "multigene",
                                        value = "")
                    updateTextInput(session, "doi", value = "")
                    updateTextInput(session, "pmid", value = "")
                    updateTextInput(session, "reference", value = "")
                }, "Create data by ShinyCell::makeShinyApp", "Upload done!")
            }
        })
        ## cell cycle
        observeEvent(input$cellcycle, {
            adminProcess({
                cc.genes.list <- readRDS(
                    system.file(
                        "extdata", "cc.genes.list.rds",
                        package = "scRNAseqApp")
                )
                cc.genes <- cc.genes.list[[input$species]]
                isENS <- vapply(
                    cc.genes,
                    FUN = function(.ele) {
                        .ele <- unlist(.ele)
                        sum(.ele %in% rownames(global$seu))
                    },
                    FUN.VALUE = numeric(1L)
                )
                cc.genes <- cc.genes[[which.max(isENS)]]
                seu <- getSeuObj()
                isolate(
                    global$seu <- CellCycleScoring(
                        seu,
                        s.features = cc.genes$s.genes,
                        g2m.features = cc.genes$g2m.genes,
                        set.ident = FALSE
                    )
                )
                cellInfo <- colnames(global$seu[[]])
                updateCheckboxGroupInput(
                    session,
                    "meta_to_include",
                    choices = cellInfo,
                    selected = cellInfo
                )
            }, "Starting CellCycleScoring", "Cell phase updated!")
        })
        observeEvent(input$tricycle, {
            if (!input$species %in% c("Homo sapiens", "Mus musculus")) {
                adminMsg("Only support human and mouse data.", type = "error")
                return()
            }
            askNamespace("tricycle", "SummarizedExperiment")
            adminProcess({
                seu <- getSeuObj()
                exp <- as.SingleCellExperiment(seu)
                gname.type <- ifelse(
                    grepl("ENS", rownames(exp)[1]),
                    "ENSEMBL",
                    "SYMBOL")
                species <- ifelse(
                    input$species == "Mus musculus",
                    "mouse",
                    "human")
                tricyle <- addTricycle(
                    exp = exp,
                    gname.type = gname.type,
                    species = species,
                    meta = seu[[]]
                )
                seu$tricyclePosition <- tricyle$tricyclePosition
                seu$CCStage <- tricyle$CCStage
                isolate(global$seu <- seu)
                cellInfo <- colnames(global$seu[[]])
                updateCheckboxGroupInput(
                    session,
                    "meta_to_include",
                    choices = cellInfo,
                    selected = cellInfo
                )
            }, "Starting Tricycle!", "Tricycle done!")
        })
        ## cell type
        observeEvent(input$singler, {
            askNamespace("SingleR", "celldex", "SummarizedExperiment")
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
                seu <- getSeuObj()
                try(attachNamespace("celldex"))
                ref <-
                    get(input$celldex,
                        envir = as.environment("package:celldex"))()
                sce <- as.SingleCellExperiment(seu)
                pred <- SingleR::SingleR(
                    test = sce,
                    ref = ref,
                    labels = ref$label.fine
                )
                ## assign singler to meta data
                seu$SingleR_labels <- pred$labels
                isolate(global$seu <- seu)
                cellInfo <- colnames(global$seu[[]])
                updateCheckboxGroupInput(
                    session,
                    "meta_to_include",
                    choices = cellInfo,
                    selected = cellInfo
                )
            }, "Starting SingleR!", "SingleR done!")
        })
        observeEvent(input$slingshot, {
            adminProcess({
                askNamespace("slingshot")
                seu <- getSeuObj()
                misc_slingshot <- NULL
                reduction_method <- seuReductionMethod(seu, FALSE)
                dimred <- Embeddings(seu, reduction = reduction_method)
                # by group points
                grp_ids <- lapply(getGrpIDs(global$config), function(id) {
                    seu[[]][[id]]
                })
                lineages <- addSlingshot(dimred, grp_ids)
                message("Assign slingshot to miscellaneous data")
                ## assign slingshot to meta data
                Misc(global$seu, "slingshot") <- lineages
            }, "Starting SlingShot", "SlingShot done!")
        })
    })
}
