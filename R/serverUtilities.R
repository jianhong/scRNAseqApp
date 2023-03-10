updateDimRedSelInput <-
    function(session, inputId, label, conf, selected) {
        updateSelectInput(
            session,
            inputId,
            label,
            choices = conf[conf$dimred == TRUE]$UI,
            selected = selected)
    }
updateDimRedSelInputPair <-
    function(session, dataSource) {
        updateDimRedSelInput(
            session,
            "GeneExprdrX",
            "X-axis:",
            dataSource()$sc1conf,
            dataSource()$sc1def$dimred[1]
        )
        updateDimRedSelInput(
            session,
            "GeneExprdrY",
            "Y-axis:",
            dataSource()$sc1conf,
            dataSource()$sc1def$dimred[2]
        )
    }
getGroupUI <- function(dataSource) {
    dataSource()$sc1conf[dataSource()$sc1conf$grp == TRUE]$UI
}
getNonGroupUI <- function(dataSource) {
    dataSource()$sc1conf[is.na(dataSource()$sc1conf$fID)]$UI
}
updateSubsetCellUI <-
    function(
        id,
        input,
        output,
        session,
        dataSource,
        addNA = FALSE) {
        choices <- dataSource()$sc1conf[dataSource()$sc1conf$grp == TRUE]$UI
        if (addNA) {
            selected  <- "N/A"
            choices <- c("N/A", choices)
        } else{
            selected <- dataSource()$sc1def$grp1
        }
        updateSelectInput(
            session,
            "subsetCell",
            "Cell information to subset:",
            choices = choices,
            selected = selected
        )
        
        output$subsetCell.ui <- renderUI({
            if (input$subsetCell != "N/A") {
                x <- dataSource()$sc1conf[
                    dataSource()$sc1conf$UI == input$subsetCell]$fID
                if (!is.null(x)) {
                    sub <-
                        strsplit(dataSource()$sc1conf[
                            dataSource()$sc1conf$UI == input$subsetCell]$fID,
                                    "\\|")
                    if (length(sub)) {
                        sub <- sub[[1]]
                    } else{
                        sub <- NULL
                    }
                    div(
                        style = 
                            paste(
                                "max-height: 150px; display:flex;",
                                "flex-direction: column; overflow-y: auto;"),
                        checkboxGroupInput(
                            NS(id, "subsetCellVal"),
                            "Select which cells to show",
                            inline = TRUE,
                            choices = sub,
                            selected = sub
                        ),
                        div(
                            style = "visibility:hidden;",
                            textInput(
                                NS(id, "subsetCellValChoices"),
                                label = NULL,
                                value = paste(sub, collapse = "|")
                            )
                        )
                    )
                }
            }
        })
        
        output$subsetCell.uncheckLab <- renderPrint(cat('Uncheck All'))
        observeEvent(input$subsetCell.uncheck, {
            sub <- strsplit(input$subsetCellValChoices, "\\|")[[1]]
            if(length(input$subsetCellVal)>0){
                selected <- NULL
                uncheckLab <- 'Check All'
            }else{
                selected <- sub
                uncheckLab <- 'Uncheck All'
            }
            output$subsetCell.uncheckLab <- renderPrint(cat(uncheckLab))
            updateCheckboxGroupInput(
                session = session,
                inputId = 'subsetCellVal',
                inline = TRUE,
                choices = sub,
                selected = selected
            )
        }) 
    }
updateFilterCellUI <-
    function(
        id,
        optCrt,
        input,
        output,
        session,
        dataSource) {
        updateSelectizeInput(
            session,
            "filterCell",
            server = TRUE,
            choices = c(
                getNonGroupUI(dataSource),
                sort(names(
                    dataSource()$sc1gene
                ))),
            selected = getNonGroupUI(dataSource)[1],
            options = list(
                maxOptions =
                    length(getNonGroupUI(dataSource)) + 3,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        output$filterCell.ui <- renderUI({
            if (!input$filterCell %in% dataSource()$sc1conf$UI) {
                val <- read_exprs(
                    dataSource()$dataset,
                    dataSource()$sc1gene[input$filterCell],
                    valueOnly = TRUE)
            } else{
                val <- dataSource()$sc1meta[[
                    dataSource()$sc1conf[
                        dataSource()$sc1conf$UI == input$filterCell]$ID]]
            }
            val <- max(val, na.rm = TRUE)
            if (val <= 1)
                maxv <- round(val, digits = 3)
            if (val > 1 && val <= 10)
                maxv <- round(val, digits = 1)
            if (val > 10)
                maxv <- round(val, digits = 0)
            sliderInput(
                NS(id, "filterCellVal"),
                "Filter the cells by value",
                min = 0,
                max = maxv,
                value = 0
            )
        })
    }

updateGeneExprDotPlotUI <-
    function(
        postfix = 1,
        id,
        input,
        output,
        session,
        plotX,
        height,
        ...,
        handlerFUN = plotsDownloadHandler) {
        output[[paste0("GeneExproup", postfix)]] <- renderPlot({
            plotX()
        })
        output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
            plotOutput(
                NS0(id, "GeneExproup", postfix),
                height = height)
        })
        output[[paste0("GeneExproup.pdf", postfix)]] <-
            handlerFUN(
                "pdf",
                width = input[[paste0("GeneExproup.w", postfix)]],
                height = input[[paste0("GeneExproup.h", postfix)]],
                plotX(),
                ...)
        output[[paste0("GeneExproup.png", postfix)]] <-
            handlerFUN(
                "png",
                width = input[[paste0("GeneExproup.w", postfix)]],
                height = input[[paste0("GeneExproup.h", postfix)]],
                plotX(),
                ...)
    }

updateCellInfoPlot <-
    function(
        postfix = 1,
        id,
        input,
        output,
        session,
        dataSource) {
        cellInfoLabel <- paste0('CellInfo', postfix)
        updateSelectInput(
            session,
            cellInfoLabel,
            "Cell information:",
            choices = dataSource()$sc1conf$UI,
            selected = dataSource()$sc1def[[paste0("meta", postfix)]]
        )
        plotX <- reactive({
            scDRcell(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$GeneExprdrX,
                input$GeneExprdrY,
                input[[cellInfoLabel]],
                input$subsetCell,
                input$subsetCellVal,
                input$GeneExprsiz,
                input[[paste0("CellInfocol", postfix)]],
                input[[paste0("CellInfoord", postfix)]],
                input$GeneExprfsz,
                input$GeneExprasp,
                input$GeneExprtxt,
                input[[paste0("CellInfolab", postfix)]],
                inpSlingshot = input[[paste0("CellInfoslingshot", postfix)]],
                slingshotFilename = file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["slingshot"]]
                )
            )
        })
        updateGeneExprDotPlotUI(
            postfix,
            id,
            input,
            output,
            session,
            plotX,
            .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input[[cellInfoLabel]]
        )
    }

#' @importFrom GenomicRanges strand start end `strand<-` `start<-` `end<-`
getCoordByGeneSymbol <- function(symbol, genes, links){
    genes0 <- genes[genes$gene_name %in% symbol | genes$gene_id %in% symbol]
    if(length(genes0)<1) return(NULL)
    strand(genes0) <- "*"
    coor <- range(genes0)[1]
    genes0 <- c(start(genes0), end(genes0))
    links0 <- links[links$gene %in% symbol]
    if(length(links)>0){
        peaks0 <- do.call(rbind, strsplit(links0$peak, "-"))
        peaks0 <- as.numeric(peaks0[, c(2, 3)])
    }else{
        peaks0 <- NULL
    }
    g0 <- range(c(genes0, peaks0))
    start(coor) <- g0[1] - 500
    end(coor) <- g0[2] + 500
    coor
}

updateGeneAccPlot <-
    function(
        postfix = 1,
        genePostfix = 2,
        optCrt,
        id,
        input,
        output,
        session,
        dataSource){
        GeneNameLabel <- paste0('GeneName', genePostfix)
        coordLabel <- paste0('coord', postfix)
        genes <- readData("sc1anno", dataSource()$dataset)
        links <- readData("sc1link", dataSource()$dataset)
        observeEvent(input[[GeneNameLabel]], {
            coor <- getCoordByGeneSymbol(input[[GeneNameLabel]], genes, links)
            updateTextInput(
                session,
                coordLabel,
                value = as(coor, "character"))
        })
        
        plotX <- reactive({
            scDRatac(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$GeneExprdrX,
                input$GeneExprdrY,
                input[[GeneNameLabel]],
                input[[coordLabel]],
                input$subsetCell,
                input$subsetCellVal,
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$GeneExprsiz,
                input[[paste0("GeneExprcol", postfix)]],
                input$GeneExprfsz,
                input$GeneExprasp,
                input$GeneExprtxt)
        })
        updateGeneExprDotPlotUI(
            postfix,
            id,
            input,
            output,
            session,
            plotX,
            .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input[[GeneNameLabel]]
        )
    }
        
updateGeneExprPlot <-
    function(
        postfix = 1,
        selectedGene,
        optCrt,
        id,
        input,
        output,
        session,
        dataSource) {
        GeneNameLabel <- paste0('GeneName', postfix)
        updateSelectizeInput(
            session,
            GeneNameLabel,
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = selectedGene,
            options = list(
                maxOptions = 6,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        ### plots
        plotX <- reactive({
            scDRgene(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$GeneExprdrX,
                input$GeneExprdrY,
                input[[GeneNameLabel]],
                input$subsetCell,
                input$subsetCellVal,
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$GeneExprsiz,
                input[[paste0("GeneExprcol", postfix)]],
                input[[paste0("GeneExprord", postfix)]],
                input$GeneExprfsz,
                input$GeneExprasp,
                input$GeneExprtxt,
                input[[paste0("GeneExprtype", postfix)]],
                if (input[[paste0("GeneExprxlimb", postfix)]] %% 2 == 0)
                    0
                else
                    input[[paste0("GeneExprxlim", postfix)]],
                inpColRange =
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0)
                        0
                    else
                        input[[paste0("GeneExprrg", postfix)]]
            )
        })
        updateGeneExprDotPlotUI(
            postfix,
            id,
            input,
            output,
            session,
            plotX,
            .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input[[GeneNameLabel]]
        )
    }

updateSubsetGeneExprPlot <-
    function(
        postfix = 1,
        subgrp,
        optCrt,
        inpColRange,
        id,
        input,
        output,
        session,
        dataSource) {
        GeneNameLabel <- paste0("GeneExprsub", postfix, "b")
        ### sub region title
        output[[paste0("subPlotTitle", postfix)]] <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression']))
            })
        ### select which cells to show
        output[[paste0("GeneExprgrp.ui", postfix)]] <- renderUI({
            subgrp <- subgrp(dataSource, input)
            selected <- ifelse(
                postfix == 1,
                subgrp[1],
                ifelse(length(subgrp) > 1, subgrp[2], subgrp[1]))
            checkboxGroupInput(
                NS(id, GeneNameLabel),
                "Select which cells to show",
                inline = TRUE,
                choices = subgrp,
                selected = selected
            )
        })
        ### plots
        plotX <- reactive({
            scDRgene(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$GeneExprdrX,
                input$GeneExprdrY,
                input$GeneName,
                input$CellInfo,
                input[[GeneNameLabel]],
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$GeneExprsiz,
                input[[paste0("GeneExprcol", postfix)]],
                input[[paste0("GeneExprord", postfix)]],
                input$GeneExprfsz,
                input$GeneExprasp,
                input$GeneExprtxt,
                input[[paste0("GeneExprtype", postfix)]],
                if (input[[paste0("GeneExprxlimb", postfix)]] %% 2 == 0)
                    0
                else
                    input[[paste0("GeneExprxlim", postfix)]],
                inpColRange = 
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0) {
                        inpColRange()
                    } else{
                        input[[paste0("GeneExprrg", postfix)]]
                    },
                infoFilterKey = input$subsetCell,
                infoFilterVal = input$subsetCellVal,
                valueFilterKey = input$filterCell,
                valueFilterCutoff = input$filterCellVal
            )
        })
        updateGeneExprDotPlotUI(
            postfix,
            id,
            input,
            output,
            session,
            plotX,
            .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input$GeneName,
            input$CellInfo
        )
    }

# sub module related
updateSubModulePlotUI <-
    function(
        postfix = 1,
        pid,
        id,
        input,
        output,
        session,
        interactive,
        plotX,
        height,
        ...) {
        if (isTRUE(interactive)) {
            output[[paste0("GeneExproup", postfix)]] <-
                renderPlotly({
                    plotX()
                })
            output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
                plotlyOutput(
                    NS0(NS(pid, id), "GeneExproup", postfix),
                    height = height)
            })
        } else{
            output[[paste0("GeneExproup", postfix)]] <- renderPlot({
                plotX()
            })
            output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
                plotOutput(
                    NS0(NS(pid, id), "GeneExproup", postfix),
                    height = height)
            })
            output[[paste0("GeneExproup.pdf", postfix)]] <-
                plotsDownloadHandler(
                    "pdf",
                    width = input[[paste0("GeneExproup.w", postfix)]],
                    height = input[[paste0("GeneExproup.h", postfix)]],
                    plotX(),
                    ...)
            output[[paste0("GeneExproup.png", postfix)]] <-
                plotsDownloadHandler(
                    "png",
                    width = input[[paste0("GeneExproup.w", postfix)]],
                    height = input[[paste0("GeneExproup.h", postfix)]],
                    plotX(),
                    ...)
        }
    }

subModuleMenuObservor <- function(
        id,
        input,
        p_session,
        dataSource,
        observeEvtList) {
    observeEvent(input$close, {
        updateTextInput(p_session, "removePlotModule", value = id)
    })
    observeEvent(input$movedown, {
        updateTextInput(p_session, "movedownPlotModule", value = id)
    })
    observeEvent(input$moveup, {
        updateTextInput(p_session, "moveupPlotModule", value = id)
    })
    observeEvent(input$resize, {
        updateTextInput(p_session, "resizePlotModule", value = id)
    })
    if (is.null(p_session$userData$defaults[[dataSource()$dataset]][[id]]))
        p_session$userData$defaults[[dataSource()$dataset]][[id]] <-
        list()
    lapply(observeEvtList, function(evt) {
        observeEvent(input[[evt]], {
            p_session$userData$defaults[[dataSource()$dataset]][[id]][[evt]] <-
                input[[evt]]
        })
    })
}

## plots related
getRatio <- function(ggData) {
    ## help function
    return((
        max(ggData$X, na.rm = TRUE) - min(ggData$X, na.rm = TRUE)) / (
            max(ggData$Y, na.rm = TRUE) - min(ggData$Y, na.rm = TRUE)))
}
orderGeneExpr <- function(ggData, GeneExprDotOrd, coln) {
    if (GeneExprDotOrd == "Max-1st") {
        ggData <- ggData[order(ggData[, coln, with = FALSE])]
    } else if (GeneExprDotOrd == "Min-1st") {
        ggData <- ggData[order(-ggData[, coln, with = FALSE])]
    } else if (GeneExprDotOrd == "Random") {
        ggData <- ggData[sample(nrow(ggData))]
    }
    return(ggData)
}
subGrp <- function(ggData, ui_key, grpVal, config) {
    if (ui_key != "N/A" && length(grpVal)) {
        ggData <- ggData[
            ggData[[config[
                config$UI == ui_key]$ID]] %in% grpVal, , drop = FALSE]
    }
    return(ggData)
}
subsetData <- function(ggData, subKey, subValue) {
    if (length(subValue) != 0 & length(subValue) !=
        nlevels(ggData[, subKey, with = FALSE])) {
        ggData <- ggData[ggData[, subKey, with = FALSE] %in% subValue]
    }
    return(ggData)
}
relevelData <- function(ggData, coln) {
    ggLvl <- levels(ggData[[coln]])
    if (length(ggLvl)) {
        ggLvl <- ggLvl[ggLvl %in% unique(ggData[[coln]])]
        ggLvl <- sortLevels(ggLvl)
        ggData[[coln]] <- factor(ggData[[coln]], levels = ggLvl)
    }
    return(ggData)
}
extractGrpColor <- function(config, ui_key) {
    ggCol <- strsplit(config[config$UI == ui_key]$fCL, "\\|")[[1]]
    names(ggCol) <-
        strsplit(config[config$UI == ui_key]$fID, "\\|")[[1]]
    return(ggCol)
}
relevelCol <- function(inpConf, ui_key, ggData, coln) {
    ggCol <- NULL
    if (!is.na(inpConf[inpConf$UI == ui_key]$fCL)) {
        ggCol <- extractGrpColor(inpConf, ui_key)
        ggCol <- ggCol[levels(ggData[[coln]])]
    }
    return(ggCol)
}
fixCoord <- function(ggOut, aspectRatio, ratio) {
    if (aspectRatio == "Square") {
        ggOut <- ggOut + coord_fixed(ratio = ratio)
    } else if (aspectRatio == "Fixed") {
        ggOut <- ggOut + coord_fixed()
    }
    return(ggOut)
}
labelBackgroundCells <- function(
        ggOut,
        ggData,
        pointSize,
        color = "snow2",
        shape = 16) {
    ggOut + geom_point(
        data = ggData,
        color = color,
        size = pointSize,
        shape = shape
    )
}
pointPlot <- function(
        ggOut,
        pointSize,
        fontSize,
        dimRedX,
        dimRedY,
        keepXYlables,
        shape = 16) {
    ggOut + geom_point(size = pointSize, shape = 16) +
        xlab(dimRedX) + ylab(dimRedY) +
        sctheme(base_size = .globals$sList[fontSize],
                XYval = keepXYlables)
}
ggXYplot <- function(ggData) {
    ggplot(ggData, aes(
        .data[["X"]], .data[["Y"]], color = .data[["val"]]))
}
getTotalNumber <- function(nGrid = 16, nPad = 2) {
    return(nGrid + nPad * 2)
}
getCoexpCol <- function(
        colorPairs,
        nGrid = 16,
        nPad = 2) {
    cInp <- strsplit(colorPairs, "; ")[[1]]
    if (cInp[1] == "Red (Gene1)") {
        c10 <- c(255, 0, 0)
    } else if (cInp[1] == "Orange (Gene1)") {
        c10 <- c(255, 140, 0)
    } else {
        c10 <- c(0, 255, 0)
    }
    if (length(cInp) > 1) {
        if (cInp[2] == "Green (Gene2)") {
            c01 <- c(0, 255, 0)
        } else {
            c01 <- c(0, 0, 255)
        }
    } else{
        c01 <- c(0, 0, 255)
    }
    
    c00 <- c(217, 217, 217)
    c11 <- c10 + c01
    nTot <- getTotalNumber(nGrid, nPad)
    gg <- data.table(
        v1 = rep(0:nTot, nTot + 1),
        v2 = sort(rep(0:nTot, nTot + 1)))
    gg$vv1 <- gg$v1 - nPad
    gg[gg$vv1 < 0]$vv1 <- 0
    gg[gg$vv1 > nGrid]$vv1 <- nGrid
    gg$vv2 <- gg$v2 - nPad
    gg[gg$vv2 < 0]$vv2 <- 0
    gg[gg$vv2 > nGrid]$vv2 <- nGrid
    gg$cR <-
        bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
    gg$cG <-
        bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
    gg$cB <-
        bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
    gg$cMix <- rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
    gg <- gg[, c("v1", "v2", "cMix")]
    return(gg)
}
getCoexpVal <- function(ggData, dataset, geneIdMap, gene1, gene2) {
    ggData$val1 <- read_exprs(dataset, geneIdMap[gene1], valueOnly = TRUE)
    ggData$val2 <-
        read_exprs(dataset, geneIdMap[gene2], valueOnly = TRUE)
    ggData[ggData$val1 < 0]$val1 <- 0
    ggData[ggData$val2 < 0]$val2 <- 0
    return(ggData)
}
cbindFilterValues <-
    function(
        ggData,
        config,
        meta,
        coln,
        geneIdMap,
        dataset,
        valueFilterKey,
        valueFilterCutoff) {
        if (!missing(valueFilterKey) && !missing(valueFilterCutoff)) {
            if (valueFilterKey %in% config$UI) {
                ggData <-
                    cbind(
                        ggData,
                        subValue =
                            meta[, config[
                                config$UI == valueFilterKey]$ID,
                                with = FALSE])
                colnames(ggData)[ncol(ggData)] <- coln
            } else if (valueFilterKey %in% names(geneIdMap)) {
                subValue <- read_exprs(
                    dataset,
                    geneIdMap[valueFilterKey],
                    valueOnly = TRUE)
                if (any(subValue < 0))
                    subValue[subValue < 0] <- 0
                ggData <- cbind(ggData, subValue = subValue)
                colnames(ggData)[ncol(ggData)] <- coln
            }
        }
        return(ggData)
    }
filterCells <- function(
        ggData,
        subsetCellKey,
        subsetCellVal,
        valueFilterKey,
        valueFilterCutoff,
        infoFilterKey,
        infoFilterVal) {
    keep <- rep(TRUE, nrow(ggData))
    if (!missing(subsetCellKey) && !missing(subsetCellVal)) {
        if (length(subsetCellVal) != 0 &&
            length(subsetCellVal) != nlevels(ggData[[subsetCellKey]])) {
            keep <- ggData[[subsetCellKey]] %in% subsetCellVal
        }
    }
    if (!missing(infoFilterKey) && !missing(infoFilterVal)) {
        if (length(infoFilterVal) != 0 &&
            length(infoFilterVal) != nlevels(ggData[[infoFilterKey]])) {
            keep <- keep & ggData[[infoFilterKey]] %in% infoFilterVal
        }
    }
    if (!missing(valueFilterKey) && !missing(valueFilterCutoff)) {
        if (length(valueFilterCutoff) != 0) {
            keep <- keep & ggData[[valueFilterKey]] >= valueFilterCutoff[1]
        }
    }
    return(keep)
}