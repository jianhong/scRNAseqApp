updateDimRedSelInput <-
    function(session, inputId, label, conf, selected) {
        updateSelectInput(
            session,
            inputId,
            label,
            choices = conf[conf$dimred == TRUE]$UI,
            selected = selected)
    }
#' @importFrom utils adist
updateDimRedSelInputPair <-
    function(session, input, dataSource) {
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
        observeEvent(input$GeneExprdrX, {
            try({
                conf <- dataSource()$sc1conf
                choices <- conf[conf$dimred == TRUE]$UI
                choices <- choices[choices!=input$GeneExprdrX]
                dist <- adist(input$GeneExprdrX, choices)
                updateDimRedSelInput(
                    session, 
                    "GeneExprdrY",
                    "Y-axis:",
                    dataSource()$sc1conf,
                    choices[which.min(dist)])
            })
        })
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
        addNA = FALSE,
        ABcolumns = "") {
        choices <- dataSource()$sc1conf[dataSource()$sc1conf$grp == TRUE]$UI
        if (addNA) {
            selected  <- "N/A"
            choices <- c("N/A", choices)
        } else{
            selected <- dataSource()$sc1def$grp1
        }
        subsetCell.ui <- reactiveValues()
        lapply(ABcolumns, function(ABcolumn){
            output[[paste0("subsetCellSel.ui", ABcolumn)]] <- renderUI({
                selectInput(
                    NS0(id, "subsetCell", ABcolumn),
                    "Cell information to subset:",
                    choices = choices,
                    selected = selected,
                    multiple = 
                        if(length(input[[paste0("subsetCell.multi",
                                                ABcolumn)]])>0)
                            as.logical(input[[paste0("subsetCell.multi",
                                                     ABcolumn)]]%%2)
                        else FALSE)
            })
            observeEvent(
                input[[paste0("subsetCell.multi", ABcolumn)]],
                updateActionButton(
                    session = session,
                    inputId = paste0("subsetCell.multi", ABcolumn),
                    label = ifelse(
                        as.logical(input[[paste0("subsetCell.multi",
                                                 ABcolumn)]]%%2),
                        "single", "multiple"))
            )
            
            subsetCell.ui[[paste0("uis", ABcolumn)]] <- list()
            observeEvent(input[[paste0("subsetCell", ABcolumn)]],{
                subsetCell <- input[[paste0("subsetCell", ABcolumn)]][
                    input[[paste0("subsetCell", ABcolumn)]]!="N/A"]
                sub_name <- dataSource()$sc1conf$UI %in% subsetCell
                x <- dataSource()$sc1conf[sub_name]$fID
                if(length(x)!=length(subsetCell)){
                    return(NULL)
                }
                sub <-strsplit(x, "\\|")
                names(sub) <- dataSource()$sc1conf[sub_name]$UI
                subsetCell.ui[[paste0("uis", ABcolumn)]] <- list()
                for(subid in subsetCell){
                    choices <- sub[[subid]]
                    if(!is.null(choices)){
                        subid1 <- paste0(subid, ABcolumn)
                        subsetCell.ui[[paste0("uis", ABcolumn)]][[subid]] <- 
                            tagList(
                                div(
                                    style = 
                                        paste(
                                            "max-height: 150px; display:flex;",
                                            "flex-direction: column;",
                                            "overflow-y: auto;")
                                    ,
                                    actionButton(
                                        NS0(id, 'subsetCell.uncheck', subid1),
                                        label=textOutput(
                                            NS0(id, 'subsetCell.uncheckLab',
                                                subid1),
                                            inline = TRUE)),
                                    checkboxGroupInput(
                                        NS0(id, "subsetCellVal", subid1),
                                        subid,
                                        inline = TRUE,
                                        choices = choices,
                                        selected = choices
                                    ),
                                    div(
                                        style = "visibility:hidden;",
                                        textInput(
                                            NS0(id, "subsetCellValChoices",
                                                subid1),
                                            label = NULL,
                                            value = paste(choices,
                                                          collapse = "|")
                                        )
                                    )
                                )
                            )
                        output[[paste0("subsetCell.uncheckLab", subid1)]] <-
                            renderPrint(cat("Uncheck All"))
                        observeEvent(input[[paste0("subsetCell.uncheck", 
                                                   subid1)]],
                          {
                              sub <- strsplit(
                                  input[[paste0("subsetCellValChoices",
                                                subid1)]],
                                  "\\|")[[1]]
                              if(length(input[[paste0("subsetCellVal",
                                                      subid1)]])>0){
                                  selected <- NULL
                                  uncheckLab <- 'Check All'
                              }else{
                                  selected <- sub
                                  uncheckLab <- 'Uncheck All'
                              }
                              output[[paste0("subsetCell.uncheckLab",
                                             subid1)]] <-
                                  renderPrint(cat(uncheckLab))
                              updateCheckboxGroupInput(
                                  session = session,
                                  inputId = paste0('subsetCellVal', subid1),
                                  inline = TRUE,
                                  choices = sub,
                                  selected = selected
                              )
                          })
                    }
                }
            })
            
            output[[paste0("subsetCell.ui", ABcolumn)]] <- 
                renderUI({subsetCell.ui[[paste0("uis", ABcolumn)]]})
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

#' @importFrom colourpicker colourInput
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
        handlerFUN = plotsDownloadHandler,
        isInfoPlot = FALSE,
        dataSource = NULL) {
        output[[paste0("GeneExproup", postfix)]] <- renderPlot({
            plotX()
        })
        output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
            plotOutput(
                NS0(id, "GeneExproup", postfix),
                width = ifelse(
                    input[[paste0("GeneExproup.w", postfix)]]==
                        .globals$figWidth,
                    '100%', input[[paste0("GeneExproup.w", postfix)]]*72),
                height = ifelse(
                    input[[paste0("GeneExproup.h", postfix)]]==
                        .globals$figHeight, height,
                    input[[paste0("GeneExproup.h", postfix)]]*72),
                dblclick = NS0(id, 'GeneExproup.dbl', postfix),
                click = NS0(id, 'GeneExproup.clk', postfix))
        })
        nearest_element <- function(e){
            if(is.null(e)) return("undefined", 'undefined')
            p <- plotX()
            ggp1 <- ggplot_build(p)
            xrg <- ggp1$layout$panel_params[[1]]$x.range
            yrg <- ggp1$layout$panel_params[[1]]$y.range
            text_layer_id <- vapply(p$layers, function(.ele){
                is(.ele$geom, 'GeomTextRepel')
            }, FUN.VALUE = logical(1L))
            text_layers <- 
                do.call(rbind, 
                        lapply(ggp1$data[text_layer_id],
                               function(.ele){
                                   .ele[, c('x', 'y', 'label', 'size')]
                               }))
            points_layers <- 
                do.call(rbind,
                        lapply(ggp1$data[!text_layer_id],
                               function(.ele){
                                   .ele[, c('x', 'y', 'colour')]
                               }))
            text_layers$width <-
                grid::convertWidth(grid::stringWidth('W'),
                                   'npc', valueOnly = TRUE)*
                (nchar(as.character(text_layers$label))+2)
            text_layers$height <-
                grid::convertHeight(grid::stringHeight('H'),
                                    'npc', valueOnly = TRUE)*2.25
            nearestLabel <- (e$x - text_layers$x)^2 + (e$y - text_layers$y)^2
            ## TODO: fix it, the text are in bottom left
            inRange <- abs((e$x - text_layers$x)/diff(xrg))<=text_layers$width &
                abs((e$y - text_layers$y)/diff(yrg))<= text_layers$height
            nearestLabel <- nearestLabel==min(nearestLabel) & inRange
            if(any(nearestLabel)){
                nearestLabel <- 
                    c('text',
                      as.character(text_layers$label[which(nearestLabel)[1]]))
            }else{
                nearestLabel <- (e$x - points_layers$x)^2 +
                    (e$y - points_layers$y)^2
                nearestLabel <- c('colour',
                                  points_layers$colour[which.min(nearestLabel)[1]])
            }
            return(nearestLabel)
        }
        if(isInfoPlot && checkPrivilege(dataSource()$auth$privilege,
                                        dataSource()$dataset)){
            ## make the duplicate button available
            # updateActionButton(session,
            #                    paste0("CellInfodup", postfix),
            #                    disabled = FALSE)
            # updateActionButton(session,
            #                    paste0("CellInforename", postfix),
            #                    disabled = FALSE)
            # updateActionButton(session,
            #                    paste0("CellInfodel", postfix),
            #                    disabled = FALSE)
            session$sendCustomMessage("toggle_div",
                                      paste0(NS0(id, "CellInfodup", postfix),
                                             'container'))
            observeEvent(input[[paste0("CellInfodup",postfix)]], {
                ## duplicated current cell info
                if(input[[paste0('CellInfodname', postfix)]]!=""){
                    updated <- updateMetaData(
                        dataset = dataSource()$dataset,
                        inpConf = dataSource()$sc1conf,
                        inpMeta = dataSource()$sc1meta,
                        privilege = dataSource()$auth$privilege,
                        info = input[[paste0('CellInfodname', postfix)]],
                        oldvalue = input[[paste0('CellInfo', postfix)]],
                        newvalue = 'duplicate')
                    if(updated){
                        session$sendCustomMessage(
                            type='updateEditorStatus',
                            message = list(id=id, postfix=postfix))
                        updateSelectInput(
                            session,
                            inputId = paste0('CellInfo', postfix),
                            choices = c(dataSource()$sc1conf$UI,
                                        input[[paste0('CellInfodname',
                                                      postfix)]]),
                            selected = input[[paste0('CellInfodname', postfix)]]
                        )
                    }else{
                        adminMsg('Something wrong! Please check the name.',
                                 "error")
                    }
                }else{
                    adminMsg('New name is not provided!',
                             "error")
                }
            })
            observeEvent(input[[paste0("CellInforename",postfix)]], {
                ## rename current cell info
                if(input[[paste0('CellInfodname', postfix)]]!=""){
                    updated <- updateMetaData(
                        dataset = dataSource()$dataset,
                        inpConf = dataSource()$sc1conf,
                        inpMeta = dataSource()$sc1meta,
                        privilege = dataSource()$auth$privilege,
                        info = input[[paste0('CellInfodname', postfix)]],
                        oldvalue = input[[paste0('CellInfo', postfix)]],
                        newvalue = 'rename')
                    if(updated){
                        session$sendCustomMessage(
                            type='updateEditorStatus',
                            message = list(id=id, postfix=postfix))
                        updateSelectInput(
                            session,
                            inputId = paste0('CellInfo', postfix),
                            choices = c(dataSource()$sc1conf$UI[
                                dataSource()$sc1conf$UI!=
                                    input[[paste0('CellInfo', postfix)]]
                            ],
                            input[[paste0('CellInfodname', postfix)]]),
                            selected = input[[paste0('CellInfodname', postfix)]]
                        )
                    }else{
                        adminMsg('Something wrong! Please check the name.',
                                 "error")
                    }
                }else{
                    adminMsg('New name is not provided!',
                             "error")
                }
            })
            observeEvent(input[[paste0("CellInfodel",postfix)]], {
                showModal(modalDialog(
                    tagList(p(
                        "Are you sure you want to delete the cell info: ",
                        input[[paste0("CellInfo",postfix)]])
                    ),
                    title = paste("Delete info",
                                  input[[paste0("CellInfo",postfix)]]),
                    footer = tagList(
                        actionButton(
                            NS0(id, "CellInfoConfirmDelete",postfix),
                            "Delete"),
                        modalButton("Cancel")
                    )
                ))
            })
            observeEvent(input[[paste0("CellInfoConfirmDelete",  postfix)]], {
                removeModal()
                ## delete current cell info
                updated <- updateMetaData(
                    dataset = dataSource()$dataset,
                    inpConf = dataSource()$sc1conf,
                    inpMeta = dataSource()$sc1meta,
                    privilege = dataSource()$auth$privilege,
                    info = 'CellInfoConfirmDelete',
                    oldvalue = input[[paste0('CellInfo', postfix)]],
                    newvalue = 'delete')
                if(updated){
                    session$sendCustomMessage(
                        type='updateEditorStatus',
                        message = list(id=id, postfix=postfix))
                    choices <- c(dataSource()$sc1conf$UI[
                        dataSource()$sc1conf$UI!=
                            input[[paste0('CellInfo', postfix)]]
                    ])
                    updateSelectInput(
                        session,
                        inputId = paste0('CellInfo', postfix),
                        choices = choices,
                        selected = choices[1]
                    )
                }else{
                    adminMsg('Something wrong!',
                             "error")
                }
                removeModal()
            })
            observeEvent(input[[paste0("GeneExproup.dbl", postfix)]],{
                evt <- input[[paste0("GeneExproup.dbl", postfix)]]
                if(!is.null(evt)){
                    session$sendCustomMessage(
                        type='placeGeneExproupInfoEditorBox',
                        message = id)
                    output[[paste0("GeneExproup.info", postfix)]] <- renderUI({
                        val <- nearest_element(evt)
                        fluidRow(
                            column(4,
                                   if(val[1]=='colour'){
                                       colourInput(
                                           NS0(id, "GeneExproup.upd",
                                               postfix),
                                           label = NULL,
                                           value = val[2]
                                       )
                                   }else{
                                       textInput(NS0(id, "GeneExproup.upd",
                                                     postfix),
                                                 label = NULL,
                                                 value = val[2])
                                   },
                                   div(
                                       style = "visibility:hidden;",
                                       textInput(NS0(id, 'GeneExproup.vtp', 
                                                     postfix),
                                                 label = NULL,
                                                 value = val[1]),
                                       textInput(NS0(id, "GeneExproup.old",
                                                     postfix),
                                                 label = NULL,
                                                 value = val[2]))),
                            column(4, actionButton(NS0(id, "GeneExproup.submit",
                                                       postfix),
                                                   label = 'update')),
                            column(4),
                            style=paste0('position:absolute; left:',
                                         input$current_mouseX,'px; top:',
                                         input$current_mouseY, 'px;')
                        )
                    })
                }
            })
            observeEvent(input[[paste0("GeneExproup.clk", postfix)]],{
                output[[paste0("GeneExproup.info", postfix)]] <- 
                    renderUI({div()})
            })
            observeEvent(input[[paste0("GeneExproup.submit",postfix)]], {
                if(!is.null(dataSource)){
                    updated <- updateMetaData(
                        dataset = dataSource()$dataset,
                        inpConf = dataSource()$sc1conf,
                        inpMeta = dataSource()$sc1meta,
                        privilege = dataSource()$auth$privilege,
                        info = input[[paste0('CellInfo', postfix)]],
                        oldvalue = input[[paste0("GeneExproup.old",
                                                 postfix)]],
                        newvalue = input[[paste0("GeneExproup.upd",
                                                 postfix)]])
                    if(updated){
                        session$sendCustomMessage(
                            type='updateEditorStatus',
                            message = list(id=id, postfix=postfix))
                    }
                    output[[paste0("GeneExproup.info", postfix)]] <- 
                        renderUI({div()})
                }
            })
        }
        
        output[[paste0("GeneExproup.dwn", postfix)]] <-
            handlerFUN(
                input = input,
                postfix = postfix,
                plotX,
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
        cellInfoName <- paste0('CellInfoname', postfix)
        observeEvent(input[[cellInfoLabel]],{
            updateSelectInput(
                session,
                cellInfoName,
                "Cell info labels",
                choices = c(dataSource()$sc1conf$UI),
                selected = input[[cellInfoLabel]]
            )
        })
        updateSelectInput(
            session,
            cellInfoLabel,
            "Cell information:",
            choices = dataSource()$sc1conf$UI,
            selected = dataSource()$sc1def[[paste0("meta", postfix)]]
        )
        plotX <- reactive({
            scDRcell(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                cellinfoID=input[[cellInfoLabel]],
                cellinfoName=input[[cellInfoName]],
                subsetCellKey=input$subsetCell,
                subsetCellVal=getSubsetCellVal(input),
                pointSize=input$GeneExprsiz,
                gradientCol=input[[paste0("CellInfocol", postfix)]],
                GeneExprDotOrd=input[[paste0("CellInfoord", postfix)]],
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                inplab=input[[paste0("CellInfolab", postfix)]],
                hideFilterCell=input[[paste0("CellInfohid", postfix)]],
                inpSlingshot = input[[paste0("CellInfoslingshot", postfix)]],
                slingshotFilename = file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["slingshot"]]
                ),
                inpShowEdge = input[[paste0("CellInfoedge", postfix)]],
                edgeFilename = file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["sc1edge"]]
                ),
                editorStatus = ifelse(
                    length(input[[paste0('editorStatus', postfix)]]),
                    input[[paste0('editorStatus', postfix)]], NA)
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
            input[[cellInfoLabel]],
            isInfoPlot = TRUE,
            dataSource = dataSource
        )
    }

expandGR <- function(coor, ext){
    start(coor) <- max(1, start(coor) - ext)
    end(coor) <- max(start(coor), end(coor) + ext)
    coor
}
#' @importFrom GenomicRanges strand start end `strand<-` `start<-` `end<-`
#' @importFrom IRanges shift
getCoordByGeneSymbol <- function(symbol, genes, links){
    if(!missing(genes)){
        genes0 <- genes[genes$gene_name %in% symbol | genes$gene_id %in% symbol]
        if(length(genes0)<1) return(NULL)
        strand(genes0) <- "*"
        coor <- range(genes0)[1]
        genes0 <- c(start(genes0), end(genes0))
        links0 <- links[links$gene %in% symbol]
        if(length(links)>0){
            if(length(links0$peak)){
                peaks0 <- do.call(rbind, strsplit(links0$peak, "-"))
                peaks0 <- as.numeric(peaks0[, c(2, 3)])
            }else{
                peaks0 <- cbind(start(links0), end(links0))
            }
            
        }else{
            peaks0 <- NULL
        }
        g0 <- range(c(genes0, peaks0))
        start(coor) <- max(1, g0[1] - round(diff(g0)/5))
        end(coor) <- g0[2] + round(diff(g0)/5)
    }else{
        links0 <- links[links$gene %in% symbol]
        if(length(links0)>0){
            if(length(links$pvalue)==length(links0)){
                coor <- links0[order(links0$pvalue)]
            }else{
                coor <- links0
            }
            if(length(coor$peak)!=length(coor)){
                coor$peak <-paste(as.character(seqnames(coor)),
                                  start(coor),
                                  end(coor),
                                  sep='-')
            }
        }else{
            coor <- NULL
        }
    }
    coor
}
getGeneSymbolByCoord <- function(coor, links){
    if(length(links$peak)!=length(links$peak)){
        links$peak <- paste(as.character(seqnames(links)),
                            start(links), end(links),
                            sep='-')
    }
    links0 <- links[links$peak %in% coor]
    if(length(links0)>0){
        if(length(links0$pvalue)==length(links0)){
            return(links0[order(links0$pvalue)]$gene)
        }else{
            return(links0$gene)
        }
    }else{
        return(NULL)
    }
}

updateAccCoordInputs <- function(session, coordLabel, coor){
    if(is(coor, "GRanges")){
        updateTextInput(
            session,
            coordLabel,
            value = as(coor, "character"))
        updateSliderInput(
            session,
            'regionselector',
            value = c(start(coor), end(coor)),
            step = max(1, round(width(coor)/100)),
            min = start(coor),
            max = end(coor)
        )
    }
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
            updateAccCoordInputs(session, coordLabel, coor)
        })
        getCoor <- function(){
            coor <- GRanges()
            tryCatch({coor <- GRanges(input[[coordLabel]])},
                     error=function(e){
                         showNotification(
                             as.character(e),
                             duration = 5,
                             type = 'warning'
                         )
                     })
            return(coor)
        }
        observeEvent(input$zoomin, {
            coor <- getCoor()
            if(length(coor)){
                updateAccCoordInputs(session, coordLabel,
                                     expandGR(coor, -width(coor)/4))
            }
            
        })
        observeEvent(input$zoomout, {
            coor <- getCoor()
            if(length(coor)){
                updateAccCoordInputs(session, coordLabel,
                                     expandGR(coor, width(coor)*2))
            }
        })
        observeEvent(input$moveleft, {
            coor <- getCoor()
            if(length(coor)){
                updateAccCoordInputs(session, coordLabel,
                                     shift(coor, -width(coor)/2))
            }
        })
        observeEvent(input$moveright, {
            coor <- getCoor()
            if(length(coor)){
                updateAccCoordInputs(session, coordLabel,
                                     shift(coor, width(coor)/2))
            }
        })
        observeEvent(input$regionsubmit, {
            if(grepl(":", input[[coordLabel]])){
                coor <- getCoor()
                change <- FALSE
                if(start(coor) != input$regionselector[1]){
                    start(coor) <- input$regionselector[1]
                    change <- TRUE
                }
                if(end(coor) != input$regionselector[2]){
                    end(coor) <- input$regionselector[2]
                    change <- TRUE
                }
                if(change){
                    updateAccCoordInputs(session, coordLabel, coor)
                }
            }
        })
        
        plotX <- reactive({
            scDRatac(
                inpConf = dataSource()$sc1conf,
                inpMeta = dataSource()$sc1meta,
                dimRedX = input$GeneExprdrX,
                dimRedY = input$GeneExprdrY,
                gene1 = input[[GeneNameLabel]],
                coord = input[[coordLabel]],
                subsetCellKey = input$subsetCell,
                subsetCellVal = 
                    input[[paste0("subsetCellVal", input$subsetCell)]],
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                pointSize = input$GeneExprsiz,
                gradientCol = input[[paste0("GeneExprcol", postfix)]],
                labelsFontsize = input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio = input$GeneExprasp,
                keepXYlables = input$GeneExprtxt)
        })
        updateGeneExprDotPlotUI(
            postfix,
            id,
            input,
            output,
            session,
            plotX,
            paste0((length(input$subsetCellVal)+4)*150, "px"),
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
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        ### plots
        plotX <- reactive({
            scDRgene(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                gene1=input[[GeneNameLabel]],
                subsetCellKey=input$subsetCell,
                subsetCellVal=getSubsetCellVal(input),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                pointSize=input$GeneExprsiz,
                gradientCol=input[[paste0("GeneExprcol", postfix)]],
                GeneExprDotOrd=input[[paste0("GeneExprord", postfix)]],
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                inpPlt=input[[paste0("GeneExprtype", postfix)]],
                inpXlim=if (input[[paste0("GeneExprxlimb", postfix)]] %% 2 == 0)
                    0
                else
                    input[[paste0("GeneExprxlim", postfix)]],
                inpColRange =
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0)
                        0
                    else
                        input[[paste0("GeneExprrg", postfix)]],
                hideFilterCell = input[[paste0("GeneExprhid", postfix)]]
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
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                gene1=input$GeneName,
                subsetCellKey=c(input$CellInfo, input$subsetCell),
                subsetCellVal=getSubsetCellVal(
                    input, list(input[[GeneNameLabel]]), input$CellInfo),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                pointSize=input$GeneExprsiz,
                gradientCol=input[[paste0("GeneExprcol", postfix)]],
                GeneExprDotOrd=input[[paste0("GeneExprord", postfix)]],
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                inpPlt=input[[paste0("GeneExprtype", postfix)]],
                inpXlim=if (input[[paste0("GeneExprxlimb", postfix)]] %% 2 == 0)
                    0
                else
                    input[[paste0("GeneExprxlim", postfix)]],
                inpColRange = 
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0) {
                        inpColRange()
                    } else{
                        input[[paste0("GeneExprrg", postfix)]]
                    },
                valueFilterKey = input$filterCell,
                valueFilterCutoff = input$filterCellVal,
                hideFilterCell = input[[paste0("GeneExprhid", postfix)]]
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
                    ggplotly(plotX()) %>% event_register("plotly_click")
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
                    width = ifelse(
                        input[[paste0("GeneExproup.w", postfix)]]==
                            .globals$figWidth,
                        '100%', input[[paste0("GeneExproup.w", postfix)]]*72),
                    height = ifelse(
                        input[[paste0("GeneExproup.h", postfix)]]==
                            .globals$figHeight, height,
                        input[[paste0("GeneExproup.h", postfix)]]*72))
            })
            output[[paste0("GeneExproup.dwn", postfix)]] <-
                plotsDownloadHandler(
                    input = input,
                    postfix = postfix,
                    plotX,
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
    observeEvent(input$CellInfosubgrp1, {
        updateTextInput(p_session, "changeSubsetContext",
                        value = paste(id, input$CellInfosubgrp1, sep='___'))
    })
    #if subset group B is selected but the B have no value
    # send click message to the TogT B group
    observeEvent(input$CellInfosubgrp1, {
        if(length(p_session$input[[paste0("subsetCell",
                                          input$CellInfosubgrp1)]])==0){
            p_session$sendCustomMessage(
                'click_subset_btn',
                paste0("explorer-subsetTogT", input$CellInfosubgrp1))
        }
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
    for(k in ui_key){
        if (k != "N/A" && length(grpVal[[k]])) {
            ggData <- ggData[
                ggData[[config[
                    config$UI == k]$ID]] %in% grpVal[[k]],
                , drop = FALSE]
        }
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
    if (!is.na(inpConf[inpConf$UI == ui_key[1]]$fCL)) {
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
        shape = 16,
        hide = FALSE) {
    if(hide) return(ggOut)
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
        fontSize = 24,
        labelsFontFamily = 'Helvetica',
        dimRedX,
        dimRedY,
        keepXYlables,
        shape = 16) {
    ggOut + geom_point(size = pointSize, shape = 16) +
        xlab(dimRedX) + ylab(dimRedY) +
        sctheme(base_size = fontSize,
                family = labelsFontFamily,
                XYval = keepXYlables)
}
ggXYplot <- function(ggData) {
    ggplot(ggData, aes(
        .data[["X"]], .data[["Y"]],
        color = .data[["val"]] #,customdata = data[["sampleID"]]
        ))
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
# get the subsetCellVals
getSubsetCellVal <- function(input, extralist, extralistname, group=""){
    if(!missing(extralist)) stopifnot(is.list(extralist)&&length(extralist)==1)
    subsetCell <- input[[paste0("subsetCell", group)]]
    subsetCell <- subsetCell[subsetCell!="N/A"]
    names(subsetCell) <- subsetCell
    subsetCell <- lapply(subsetCell, function(subid){
        input[[paste0("subsetCellVal", subid, group)]]
    })
    if(!missing(extralist) && !missing(extralistname)){
        names(extralist) <- extralistname
        if(extralistname %in% names(subsetCell)){
            subsetCell[[extralistname]] <- 
                intersect(subsetCell[[extralistname]], extralist[[1]])
        }else{
            subsetCell <- c(extralist, subsetCell)
        }
        
    }
    return(subsetCell)
}
# check the pairs of subsetCellKey and subsetCellVals
namedSubsetCellVals <- function(subsetCellKey, subsetCellVal){
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    if(length(subsetCellKey)==1 && !is.list(subsetCellVal)){
        subsetCellVal <- list(subsetCellVal)
        names(subsetCellVal) <- subsetCellKey
        return(subsetCellVal)
    }
    stopifnot(is.list(subsetCellVal))
    stopifnot(length(names(subsetCellVal))==length(subsetCellVal))
    subsetCellVal[unique(subsetCellKey)]
}

filterCells <- function(
        ggData,
        subsetCellKey,
        subsetCellVal,
        valueFilterKey,
        valueFilterCutoff,
        inpConf) {
    keep <- rep(TRUE, nrow(ggData))
    if (!missing(subsetCellKey) && !missing(subsetCellVal)) {
        if(length(subsetCellKey)==1){
            if(!is.list(subsetCellVal)){
                subsetCellVal <- list(subsetCellVal)
                names(subsetCellVal) <- subsetCellKey
            }
        }
        if(length(names(subsetCellVal))==0){
            names(subsetCellVal) <- subsetCellKey
        }
        for(skey in subsetCellKey){
            sid <- inpConf[inpConf$UI == skey]$ID
            if(length(sid)==0) next
            if(length(colnames(ggData))==0) next
            if(!sid %in% colnames(ggData)) next
            if (length(subsetCellVal[[skey]]) != nlevels(ggData[[sid]])) {
                keep <- keep & ggData[[sid]] %in% subsetCellVal[[skey]]
            }
        }
    }
    if (!missing(valueFilterKey) && !missing(valueFilterCutoff)) {
        if (length(valueFilterCutoff) != 0) {
            keep <- keep & ggData[[valueFilterKey]] >= valueFilterCutoff[1]
        }
    }
    return(keep)
}

updateRankList <- function(input, output, dataSource, uid, pid, input_id){
    observeEvent(input[[uid]], {
        output[[pid]] <- renderUI(rank_list(
            text = "Drag, drop and re-order the following items:",
            labels = sort(as.character(unlist(unique(dataSource()$sc1meta[
                , dataSource()$sc1conf[
                    dataSource()$sc1conf$UI == input[[uid]]]$ID, with = FALSE])))),
            input_id = input_id
        ))
    })
}
