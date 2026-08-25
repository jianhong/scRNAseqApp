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
    function(session, input, dataSource, ABcolumn='') {
        idx <- paste0("GeneExprdrX", ABcolumn)
        idy <- paste0("GeneExprdrY", ABcolumn)
        idz <- paste0("GeneExprdrZ", ABcolumn)
        updateDimRedSelInput(
            session,
            idx,
            "X-axis:",
            dataSource()$sc1conf,
            dataSource()$sc1def$dimred[1]
        )
        updateDimRedSelInput(
            session,
            idy,
            "Y-axis:",
            dataSource()$sc1conf,
            dataSource()$sc1def$dimred[2]
        )
        try({
            updateSelectInput(
                session,
                idz,
                "Z-axis:",
                choices = c(NA, dataSource()$sc1conf[
                    dataSource()$sc1conf$dimred == TRUE]$UI),
                selected = character(0L))
        })
        observeEvent(input[[idx]], {
            try({
                conf <- dataSource()$sc1conf
                choices <- conf[conf$dimred == TRUE]$UI
                choices <- choices[choices!=input[[idx]]]
                dist <- adist(input[[idx]], choices)
                updateDimRedSelInput(
                    session, 
                    idy,
                    "Y-axis:",
                    dataSource()$sc1conf,
                    choices[which.min(dist)])
                
                updateSelectInput(
                    session,
                    idz,
                    "Z-axis:",
                    choices = c(NA, dataSource()$sc1conf[
                        dataSource()$sc1conf$dimred == TRUE]$UI),
                    selected = character(0L))
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
getM <- function(val){
    val <- max(val, na.rm = TRUE)
    if (val <= 1)
        maxv <- round(val, digits = 3)
    if (val > 1 && val <= 10)
        maxv <- round(val, digits = 1)
    if (val > 10)
        maxv <- ceiling(val)
    return(maxv)
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
                val2 <- c()
            } else{
                val <- dataSource()$sc1meta[[
                    dataSource()$sc1conf[
                        dataSource()$sc1conf$UI == input$filterCell]$ID]]
                if(grepl('[12]$', input$filterCell) &&
                   sum(grepl(paste0('^', sub('.$', '',
                                             input$filterCell)),
                             dataSource()$sc1conf$UI))==2){
                    filterCellKey2 <- ifelse(
                        grepl('1$', input$filterCell),
                        sub('.$', '2', input$filterCell),
                        sub('.$', '1', input$filterCell)
                    )
                    val2 <- dataSource()$sc1meta[[
                        dataSource()$sc1conf[
                            dataSource()$sc1conf$UI == filterCellKey2]$ID]]
                }else{
                    val2 <- c()
                }
            }
            minv <- floor(min(val, na.rm = TRUE))
            maxv <- getM(val)
            if(length(val2)>1){
                minv2 <- floor(min(val2, na.rm = TRUE))
                maxv2 <- getM(val2)
                tagList(
                    sliderInput(
                        NS(id, "filterCellVal"),
                        paste("Filter the cells by", input$filterCell),
                        min = minv,
                        max = maxv,
                        value = c(minv, maxv)
                    ),
                    sliderInput(
                        NS(id, "filterCellVal2"),
                        paste("Filter the cells by", filterCellKey2),
                        min = minv2,
                        max = maxv2,
                        value = c(minv2, maxv2)
                    )
                )
            }else{
                tagList(
                    sliderInput(
                        NS(id, "filterCellVal"),
                        "Filter the cells by value",
                        min = minv,
                        max = maxv,
                        value = c(minv, maxv)
                    )
                )
            }
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
        dataSource = NULL,
        molecules = NULL) {
        # link the ranges to manuXlim and Ylim
        geneExprXYlimTog <- paste0('manuXYlimTog', postfix)
        cellInfoXlim <- paste0('manuXlim', postfix)
        cellInfoYlim <- paste0('manuYlim', postfix)
        reducedDimsChangeInMolecules <- reactiveVal(FALSE)
        observeEvent(list(input[[cellInfoXlim]], input[[cellInfoYlim]]), {
            if(isTRUE(input[[paste0('XYlimLinker', postfix)]]) &&
               !reducedDimsChangeInMolecules()){
                pairedX <- paste0('manuXlim', ifelse(postfix==1, 2, 1))
                if(!all(input[[cellInfoXlim]]==input[[pairedX]])){
                    updateSliderInput(
                        session,
                        pairedX,
                        value = input[[cellInfoXlim]]
                    )
                }
                pairedY <- paste0('manuYlim', ifelse(postfix==1, 2, 1))
                if(!all(input[[cellInfoYlim]]==input[[pairedY]])){
                    updateSliderInput(
                        session,
                        pairedY,
                        value = input[[cellInfoYlim]]
                    )
                }
            }
            if(reducedDimsChangeInMolecules()){
                reducedDimsChangeInMolecules(FALSE)
            }
        })
        
        observeEvent(input$GeneExprdrX, {
            if(length(molecules)==0){
                updateLimRange(postfix, input, session, dataSource,
                               cellInfoXlim, X=TRUE)
                if(length(input$fov2)>0){
                    reducedDimsChangeInMolecules(TRUE)
                }
            }
        })
        observeEvent(input$GeneExprdrY, {
            if(length(molecules)==0){
                updateLimRange(postfix, input, session, dataSource,
                               cellInfoYlim, X=FALSE)
                if(length(input$fov2)>0){
                    reducedDimsChangeInMolecules(TRUE)
                }
            }
        })
        observeEvent(input$fov2, {
            if(length(molecules)>0){
                updateLimRange(postfix, input, session, dataSource,
                               cellInfoXlim,
                               X=TRUE,
                               val = molecules[[
                                   input[[paste0('fov', postfix)]]
                               ]]$x)
                updateLimRange(postfix, input, session, dataSource,
                               cellInfoYlim,
                               X=FALSE,
                               val = molecules[[
                                   input[[paste0('fov', postfix)]]
                               ]]$y)
            }
        })
        observeEvent(input[[paste0('XYlimLinker', postfix)]], {
            thisXYlimLinker <- paste0('XYlimLinker', postfix)
            pairedXYlimLinker <- paste0('XYlimLinker', ifelse(postfix==1, 2, 1))
            if(input[[pairedXYlimLinker]]!=input[[thisXYlimLinker]]){
                if(isTRUE(!is.null(input[['fov2']]))){
                    rdim <- sub('.$', '', input[['GeneExprdrX']])
                    updateCheckboxInput(
                        inputId = pairedXYlimLinker,
                        value = isTRUE(rdim==input[['fov2']])
                    )
                }else{
                    updateCheckboxInput(
                        inputId = pairedXYlimLinker,
                        value = input[[thisXYlimLinker]]
                    )  
                }
            }
        })
        # save Ranges
        resetXYRanges <- function(X=TRUE){
            if(isTRUE(X)){
                label <- 'X'
            }else{
                label <- 'Y'
            }
            
            if(isTRUE(!is.na(input[[paste0('manu', label, 'limOriMin', postfix)]]) &&
               !is.na(input[[paste0('manu', label, 'limOriMax', postfix)]]))){
                updateSliderInput(
                    session,
                    paste0('manu', label, 'lim', postfix),
                    value = c(input[[paste0('manu', label, 'limOriMin', postfix)]],
                              input[[paste0('manu', label, 'limOriMax', postfix)]])
                )
                if(isTRUE(input[[paste0('XYlimLinker', postfix)]])){
                    updateSliderInput(
                        session,
                        paste0('manu', label, 'lim', ifelse(postfix==1, 2, 1)),
                        value = c(input[[paste0('manu', label, 'limOriMin', postfix)]],
                                  input[[paste0('manu', label, 'limOriMax', postfix)]])
                    )
                }
            }
        }
        resetRanges <- function(){
            resetXYRanges(X=TRUE)
            resetXYRanges(X=FALSE)
            if(!is.null(input[[paste0('GeneExpext.info', postfix)]])){
                updateTextInput(
                    inputId = paste0('GeneExpext.info', postfix),
                    value = '')
            }
        }
        refreshGeneExprUI <- reactiveValues(oldUI='2D')
        create2DUI <- function(){
            output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
                plotUI <- plotOutput(
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
                    click = clickOpts(NS0(id, 'GeneExproup.clk', postfix),
                                      clip = FALSE),
                    brush = brushOpts(NS0(id, 'GeneExproup.brush', postfix),
                                      resetOnNew = TRUE),
                    hover = hoverOpts(NS0(id, 'GeneExproup.hover', postfix),
                                      delay=500, delayType='debounce'))
                if(id %in% c('cellInfoGeneExpr', 'cellInfoCellInfo',
                             'subsetGeneExpr', 'geneExprGeneExpr',
                             'sunburst', 'deconvolution', 'coExpr')){
                    div(
                        #class = "wheel-zoomable-plot", # default close it
                        id = NS0(id, 'GeneExproupDIV', postfix),
                        plotUI,
                        uiOutput(NS0(id, 'GeneExproup.tooltip', postfix))
                    )
                }else{
                    plotUI
                }
            })
            
            # Handle hover to show the tooltips
            output[[paste0('GeneExproup.tooltip', postfix)]] <- renderUI({
                if(!id %in% c('cellInfoGeneExpr', 'cellInfoCellInfo',
                              'subsetGeneExpr', 'geneExprGeneExpr',
                              'sunburst', 'deconvolution', 'coExpr')){
                    return(NULL)
                }
                
                hover <- input[[paste0("GeneExproup.hover", postfix)]]
                if (is.null(hover)) {
                    return(NULL)
                }
                session$sendCustomMessage(
                    type='placeGeneExproupInfoEditorBox',
                    message = id)
                val <- nearest_element(hover, labelFirst = FALSE)
                req(length(val)>=3)
                req(val[1]=='colour')
                # hover$range the pixel range of the plot panel within the page
                wellPanel(
                    style = paste0(
                        "position: fixed; border-left: 6px solid ", val[2], ";",
                        "left:", input$current_clientX+5, "px;",
                        "top:", input$current_clientY+5, "px;"
                    ),
                    class="tooltip-box",
                    id = NS0(id, "GeneExproup_tooltip", postfix),
                    HTML(paste0(
                        "<b><i class='fa fa-circle' style='color: ",
                        val[2], ";'></i></b> ", val[3], "<br/>"
                    ))
                )
            })
            # Handle brush (drag selection) for zooming
            observeEvent(input[[paste0("GeneExproup.brush", postfix)]], {
                if(!isTRUE(input[[paste0("GeneExproup.isPanning", postfix)]])){
                    brush <- input[[paste0("GeneExproup.brush", postfix)]]
                    if (!is.null(brush)) {
                        updateSliderInput(
                            session,
                            cellInfoXlim,
                            value = c(brush$xmin, brush$xmax)
                        )
                        updateSliderInput(
                            session,
                            cellInfoYlim,
                            value = c(brush$ymin, brush$ymax)
                        )
                    }
                }
            })
            
            observeEvent(list(input[[cellInfoXlim]], input[[cellInfoYlim]]), {
                output[[paste0("GeneExproup", postfix)]] <- renderPlot({
                    setCurrentPlot()
                    addLimits(darkTheme(plotX(), dataSource=dataSource),
                              ranges=list(x=input[[cellInfoXlim]],
                                          y=input[[cellInfoYlim]]),
                              coord=input[[paste0('coord', postfix)]],
                              id=id, postfix=postfix, input=input)
                }, bg=darkTheme(returnBG=TRUE,
                                dataSource=dataSource))
            })
        }
        create2DUI()
        observeEvent(input$GeneExprdrZ, {
            req(dataSource)
            
            # Snapshot old state safely with isolate()
            oldUI <- isolate(refreshGeneExprUI$oldUI)
            newUI <- '2D'

            if(is(plotX(), 'plotly')){
                refresh <- TRUE
                newUI <- '3D'
            }else{
                if(oldUI=='3D'){
                    refresh <- TRUE
                }else{
                    refresh <- FALSE
                }
                newUI <- '2D'
            }
            if(refresh){
                removeUI(selector=paste0("#", id, "-GeneExproup\\.ui",
                                         postfix, " > *"), immediate=TRUE)
            }
            if(newUI=='3D'){
                output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
                    plotlyOutput(
                        NS0(id, "GeneExproup", postfix),
                        width = ifelse(
                            input[[paste0("GeneExproup.w", postfix)]]==
                                .globals$figWidth,
                            '100%', input[[paste0("GeneExproup.w", postfix)]]*72),
                        height = ifelse(
                            input[[paste0("GeneExproup.h", postfix)]]==
                                .globals$figHeight, height,
                            input[[paste0("GeneExproup.h", postfix)]]*72))
                })
                output[[paste0("GeneExproup", postfix)]] <- renderPlotly({
                    darkTheme(plotX(), dataSource=dataSource) %>%
                        event_register("plotly_click")
                })
                # hide or show the controllers
                session$sendCustomMessage(
                    'hide_div', 
                    paste0(NS0(id, "Cell3Div", postfix), 'Menucontainer'))
            }else{
                create2DUI()
                # show the controllers
                session$sendCustomMessage(
                    'show_div', 
                    paste0(NS0(id, "Cell3Div", postfix), 'Menucontainer'))
            }
            refreshGeneExprUI$oldUI <- newUI
        })
        
        # Handle pan switch
        observeEvent(input[[paste0("usingPan", postfix)]], {
            session$sendCustomMessage(
                type='updatePanStatus',
                message = list(id=id, postfix=postfix,
                               value=input[[paste0("usingPan",
                                                   postfix)]]))
        })
        
        # handle wheel zoom in and out
        observeEvent(input[[paste0("GeneExproup.scroll", postfix)]], {
            x <- input[[cellInfoXlim]]
            y <- input[[cellInfoYlim]]
            dx <- diff(x)/10
            dy <- diff(y)/10
            if(input[[paste0("GeneExproup.scroll", postfix)]]>0){
                updateSliderInput(
                    session,
                    cellInfoXlim,
                    value = x + c(-dx, dx)
                )
                updateSliderInput(
                    session,
                    cellInfoYlim,
                    value = y + c(-dy, dy)
                )
            }else{
                updateSliderInput(
                    session,
                    cellInfoXlim,
                    value = x + c(dx, -dx)
                )
                updateSliderInput(
                    session,
                    cellInfoYlim,
                    value = y + c(dy, -dy)
                )
            }
        })
        # handle pan (drag)
        observeEvent(input[[paste0("GeneExproup.pan", postfix)]], {
            if(isTRUE(input[[paste0("GeneExproup.isPanning", postfix)]])){
                dx <- input[[paste0("GeneExproup.pan", postfix)]]$dx
                dy <- input[[paste0("GeneExproup.pan", postfix)]]$dy
                w  <- input[[paste0("GeneExproup.pan", postfix)]]$width
                h  <- input[[paste0("GeneExproup.pan", postfix)]]$height
                x <- input[[cellInfoXlim]]
                y <- input[[cellInfoYlim]]
                xr <- diff(x)
                yr <- diff(y)
                x_shift <- -dx/w * xr
                y_shift <- dy/h * yr
                updateSliderInput(
                    session,
                    cellInfoXlim,
                    value = x + x_shift
                )
                updateSliderInput(
                    session,
                    cellInfoYlim,
                    value = y + y_shift
                )
            }
        })
        
        currentplot <- reactiveVal()
        setCurrentPlot <- function(){
            ## set current plot for nearest_element
            ## need to be run when plot updated
            p <- tryCatch(plotX(), error=function(.e){
                return(NULL)
            })
            if(is(p, 'ggplot')){
                ggp1 <- tryCatch(ggplot_build(p), error=function(.e){
                  return(NULL)
                })
                currentplot(ggp1)
            }
        }
        # replace nearPoints
        nearest_element <- function(e, labelFirst=TRUE){
            if(is.null(e)) return(c("undefined", 'undefined'))
            ggp1 <- isolate(currentplot())
            if(is.null(ggp1)||isTRUE(lengths(ggp1$data)[1]==0)){
                setCurrentPlot()
                ggp1 <- isolate(currentplot())
            }
            p <- ggp1$plot
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
                                   if(all(c('x', 'y', 'colour') %in%
                                          colnames(.ele))){
                                       .ele[, c('x', 'y', 'colour')] 
                                   }
                               }))
            if(!labelFirst){
                nearestLabel <- (e$x - points_layers$x)^2 +
                    (e$y - points_layers$y)^2
                maxDist <- (diff(xrg)/100)^2 + (diff(yrg)/100)^2
                if(isTRUE(min(nearestLabel)>maxDist)){
                    return(c("undefined", 'undefined'))
                }
                k <- which.min(nearestLabel)[1]
                nearestLabel <- c('colour',
                                  points_layers$colour[k])
                if('val' %in% colnames(p$data)){
                    d <- p$data
                    colnames(d) <- tolower(colnames(d))
                    val <- d[d$x==points_layers$x[k] &
                                              d$y==points_layers$y[k],
                    ]$val
                    if(length(val)){
                        if(is.numeric(val[1])) {
                            val <- prettyNum(val[1])
                        }else{
                            val <- as.character(val[1])
                        }
                        nearestLabel <- c(nearestLabel, val)
                    }
                }
                return(nearestLabel)
            }
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
                k <- which.min(nearestLabel)[1]
                nearestLabel <- c('colour',
                                  points_layers$colour[k])
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
            session$sendCustomMessage("show_div",
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
            })
            observeEvent(input[[paste0("GeneExproup.dbl", postfix)]],{
                if(!all(input[[cellInfoXlim]]==c(input[[paste0('manuXlimOriMin', postfix)]],
                                                 input[[paste0('manuXlimOriMax', postfix)]])) ||
                   !all(input[[cellInfoYlim]]==c(input[[paste0('manuYlimOriMin', postfix)]],
                                                 input[[paste0('manuYlimOriMax', postfix)]]))){
                    resetRanges()
                }else{
                    evt <- input[[paste0("GeneExproup.dbl", postfix)]]
                    req(evt)
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
                            style=paste0('position:fixed; left:',
                                         input$current_clientX,'px; top:',
                                         input$current_clientY, 'px;')
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
        }else{## not info
            ## zoom in for ATAC
            ## zoom in for gene expression
            observeEvent(input[[paste0("GeneExproup.dbl", postfix)]],{
                resetRanges()
            })
            observeEvent(input[[paste0("GeneExproup.clk", postfix)]],{
                # print(input[[paste0("GeneExproup.clk", postfix)]])
                
            })
        }
        observeEvent(list(input[[cellInfoXlim]], input[[cellInfoYlim]]), {
            output[[paste0("GeneExproup.dwn", postfix)]] <-
                handlerFUN(
                    input = input,
                    postfix = postfix,
                    plot = plotX,
                    dataSource = dataSource,
                    ranges = list(x=input[[cellInfoXlim]],
                                  y=input[[cellInfoYlim]]),
                    ...)
        })
        
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
        cellInfoXYlimTog <- paste0('manuXYlimTog', postfix)
        cellInfoXlim <- paste0('manuXlim', postfix)
        cellInfoYlim <- paste0('manuYlim', postfix)
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
        output[[paste0('subsetCellNum', postfix)]] <- 
            renderText(paste('% of', nrow(dataSource()$sc1meta), 'cells'))
        observeEvent({getSubsetCellVal(input)},{
            output[[paste0('subsetCellNum', postfix)]] <- 
                renderText(paste('% of', getFilteredCellNum(
                    inpConf=dataSource()$sc1conf,
                    inpMeta=dataSource()$sc1meta,
                    dimRedX=input$GeneExprdrX,
                    dimRedY=input$GeneExprdrY,
                    cellinfoID=input[[cellInfoLabel]],
                    cellinfoName=input[[cellInfoName]],
                    subsetCellKey=input$subsetCell,
                    subsetCellVal=getSubsetCellVal(input),
                    subsetCellPct=100
                ), 'cells'))
        })
        
        plotX <- reactive({
            scDRcell(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                dimRedZ=input$GeneExprdrZ,
                cellinfoID=input[[cellInfoLabel]],
                cellinfoName=input[[cellInfoName]],
                subsetCellKey=input$subsetCell,
                subsetCellVal=getSubsetCellVal(input),
                subsetCellPct=input[[paste0("subsetCellPct", postfix)]],
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
                    input[[paste0('editorStatus', postfix)]], NA),
                inpCellBorder=input[[paste0('CellInfoSegmentation', postfix)]],
                cellborderFilename=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["cellborder"]]),
                cellSegAlpha = input[[paste0('CellInfoSegAlpha', postfix)]],
                cellSegColor = ifelse(
                    input[[paste0('CellInfoSegBorderColor', postfix)]],
                    input[[paste0('CellInfoSegColor', postfix)]],
                    NA),
                inpBgImg=input[[paste0('CellInfoBgImg', postfix)]],
                backgroundImage=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["backgroundImage"]])
            )
        })
        updateGeneExprDotPlotUI(
            postfix = postfix,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plotX,
            height = .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input[[cellInfoLabel]],
            isInfoPlot = TRUE,
            dataSource = dataSource
        )
    }

expandGR <- function(coor, ext){
    if(is.na(ext[1])) ext <- 0
    start(coor) <- max(1, start(coor) - ext)
    end(coor) <- min(2^31-1, max(start(coor), end(coor) + ext))
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
            postfix = postfix,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plotX,
            height = paste0((length(input$subsetCellVal)+4)*150, "px"),
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input[[GeneNameLabel]],
            dataSource = dataSource
        )
    }

updateMoleculePlot <-
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
        FOVLabel <- paste0('fov', postfix)
        molecule_fs <- file.path(.globals$datafolder,
                                 dataSource()$dataset,
                                 .globals$filenames$molecules)
        if(!file.exists(molecule_fs)){
            warning('molecule file does not exists')
            return()
        }
        molecules <- readRDS(molecule_fs)
        available_FOV <- names(molecules)
        updateSelectInput(
            session = session,
            FOVLabel,
            choices = available_FOV,
            selected = available_FOV[1]
        )
        rdim <- input[[FOVLabel]] %||% available_FOV[1] ## issue
        updateSelectizeInput(
            session,
            GeneNameLabel,
            choices = sort(unique(molecules[[rdim]]$molecule)),
            server = TRUE,
            selected = input[[GeneNameLabel]] %||% molecules[[rdim]]$molecule[1],
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        
        ### plots
        plotX <- reactive({
            scDRmolecule(
                genes = input[[GeneNameLabel]],
                molecules = molecules,
                fov = input[[FOVLabel]],
                pointSize=input$GeneExprsiz,
                gradientCol=input[[paste0("GeneExprcol", postfix)]],
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                inpCellBorder=input[[paste0('GeneExprSegmentation', postfix)]],
                cellborderFilename=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["cellborder"]]),
                cellSegAlpha = input[[paste0('GeneExprSegAlpha', postfix)]],
                cellSegColor = ifelse(
                    input[[paste0('GeneExprSegBorderColor', postfix)]],
                    input[[paste0('GeneExprSegColor', postfix)]],
                    NA),
                cellColor = if(input[[paste0('GeneExprSegmentation', postfix)]])
                    list(# hard coding here, use cell information to color cells
                    cellinfoID=input[['CellInfo1']],
                    inpConf=dataSource()$sc1conf,
                    inpMeta=dataSource()$sc1meta) else NULL, 
                inpBgImg=input[[paste0('GeneExprBgImg', postfix)]],
                backgroundImage=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["backgroundImage"]])
            )
        })
        updateGeneExprDotPlotUI(
            postfix = postfix,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plotX,
            height = .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$FOVLabel,
            input[[GeneNameLabel]],
            dataSource = dataSource,
            molecules = molecules
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
        dataSource,
        geneType='gene'
        ) {
        GeneNameLabel <- paste0('GeneName', postfix)
        if(geneType=='gene'){
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
        }else{
            updateSelectizeInput(
                session,
                GeneNameLabel,
                choices = sort(names(dataSource()$sc1gsgene)),
                server = TRUE,
                selected = selectedGene,
                options = list(
                    maxOptions = .globals$maxNumGene,
                    create = TRUE,
                    persist = TRUE,
                    render = I(optCrt)
                )
            )
        }
        
        observeEvent(input[[GeneNameLabel]], {
            if(isTRUE(input[[GeneNameLabel]]!=dataSource()$sc1def$gene1 &&
                      input[[GeneNameLabel]]!=dataSource()$sc1def$gene2 &&
                      input[[GeneNameLabel]]!="")){
                updateSearchTable(input[[GeneNameLabel]])
            }
        }, ignoreInit=TRUE)
        
        geneExprXYlimTog <- paste0('manuXYlimTog', postfix)
        geneExprXlim <- paste0('manuXlim', postfix)
        geneExprYlim <- paste0('manuYlim', postfix)
        
        ### plots
        plotX <- reactive({
            scDRgene(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                dimRedZ=input$GeneExprdrZ,
                gene1=input[[GeneNameLabel]],
                subsetCellKey=input$subsetCell,
                subsetCellVal=getSubsetCellVal(input),
                dataset=dataSource()$dataset,
                geneIdMap=
                    if(geneType=='gene')
                        dataSource()$sc1gene
                    else
                        dataSource()$sc1gsgene
                ,
                pointSize=input$GeneExprsiz,
                gradientCol=input[[paste0("GeneExprcol", postfix)]],
                GeneExprDotOrd=input[[paste0("GeneExprord", postfix)]],
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                inpPlt=input[[paste0("GeneExprtype", postfix)]],
                inpXlim=if(input[[geneExprXYlimTog]] %% 2 == 1)
                    input[[geneExprXlim]] else 0,
                inpColRange =
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0)
                        0
                    else
                        input[[paste0("GeneExprrg", postfix)]],
                hideFilterCell = input[[paste0("GeneExprhid", postfix)]],
                geneType = geneType,
                inpCellBorder=input[[paste0('GeneExprSegmentation', postfix)]],
                cellborderFilename=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["cellborder"]]),
                cellSegAlpha = input[[paste0('GeneExprSegAlpha', postfix)]],
                cellSegColor = ifelse(
                    input[[paste0('GeneExprSegBorderColor', postfix)]],
                    input[[paste0('GeneExprSegColor', postfix)]],
                    NA),
                inpBgImg=input[[paste0('GeneExprBgImg', postfix)]],
                backgroundImage=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["backgroundImage"]])
            )
        })
        updateGeneExprDotPlotUI(
            postfix = postfix,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plotX,
            height = .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input[[GeneNameLabel]],
            dataSource = dataSource
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
        ## set xy lim
        geneExprXYlimTog <- paste0('manuXYlimTog', postfix)
        geneExprXlim <- paste0('manuXlim', postfix)
        geneExprYlim <- paste0('manuYlim', postfix)
        
        ### plots
        plotX <- reactive({
            scDRgene(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                dimRedZ=input$GeneExprdrZ,
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
                inpXlim=if(input[[geneExprXYlimTog]] %% 2 == 1)
                    input[[geneExprXlim]] else 0,
                inpColRange = 
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0) {
                        inpColRange()
                    } else{
                        input[[paste0("GeneExprrg", postfix)]]
                    },
                valueFilterKey = input$filterCell,
                valueFilterCutoff = input$filterCellVal,
                valueFilterCutoff2 = input$filterCellVal2,
                hideFilterCell = input[[paste0("GeneExprhid", postfix)]]
            )
        })
        updateGeneExprDotPlotUI(
            postfix = postfix,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plotX,
            height = .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input$GeneName,
            input$CellInfo,
            dataSource = dataSource
        )
    }

# sub module related
#' @importFrom plotly toWebGL ggplotly renderPlotly event_register plotlyOutput
updateSubModulePlotUI <-
    function(
        postfix = 1,
        pid,
        id,
        input,
        output,
        session,
        p_session,
        interactive,
        plotX,
        height,
        lasso=FALSE,
        dataSource=NULL,
        ...) {
        if (isTRUE(interactive)) {
            output[[paste0("GeneExproup", postfix)]] <-
                renderPlotly({
                    ggplotly(plotX()) %>% toWebGL() %>%
                        event_register("plotly_click")
                })
            output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
                plotlyOutput(
                    NS0(NS(pid, id), "GeneExproup", postfix),
                    height = ifelse(
                        input[[paste0("GeneExproup.h", postfix)]]==
                            .globals$figHeight, height,
                        input[[paste0("GeneExproup.h", postfix)]]*72))
            })
            if(isTRUE(lasso)|| is.character(lasso)){# TRUE for logged user, 'public' for public user
                ## update the download form
                if(isTRUE(lasso)){## control the users, if not login do not show download 
                    updateSelectInput(session=session,
                                      inputId = paste0("GeneExproup.fmt",postfix),
                                      choices = 'CSV', selected = 'CSV')
                    output[[paste0("GeneExproup.dwn", postfix)]] <- 
                        exprDownloadHandler(dataset=pid, lasso=TRUE,
                                            plot=plotX, session=session)
                }
                updateCheckboxInput(session = session,
                                    inputId = paste0('GeneExproupDimT', postfix),
                                    value = FALSE)
                observeEvent(input[[paste0('GeneExproupSelIDs', postfix)]], {
                    d <- get_lasso_selected_ids(session=session, plot=plotX)
                    if(length(d)){
                        if(all(!is.na(d$sampleID))){
                            p_session$userData$selectedCellIDs <- switch(
                                input[[paste0('GeneExproupSelMethod',
                                              postfix)]],
                                'new'=as.character(d$sampleID),
                                'add'=unique(c(
                                    p_session$userData$selectedCellIDs,
                                    as.character(d$sampleID))),
                                'del'= p_session$userData$selectedCellIDs[
                                    !p_session$userData$selectedCellIDs %in%
                                        as.character(d$sampleID) 
                                ]
                            )   
                            showNotification('Succeed setting lasso selection',
                                             id = 'setLasso',
                                             type='message')
                        }
                    }
                    updateActionButton(session = p_session,
                                       inputId = 'filterCellIDs',
                                       disabled = FALSE)
                }, ignoreInit = TRUE)
            }
        } else{
            output[[paste0("GeneExproup", postfix)]] <- renderPlot({
                darkTheme(plotX(), dataSource=dataSource)
            }, bg=darkTheme(returnBG=TRUE,
                            dataSource=dataSource))
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
                    plot = plotX,
                    dataSource = dataSource,
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
    observeEvent(input$CellInfoCoor1, {
        updateTextInput(p_session, "changeCoorContext",
                        value = paste(id, input$CellInfoCoor1, sep='___'))
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
                'click_btn',
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
    if(is.numeric(ggData$X) && is.numeric(ggData$Y)){
        return((
            max(ggData$X, na.rm = TRUE) - min(ggData$X, na.rm = TRUE)) / (
                max(ggData$Y, na.rm = TRUE) - min(ggData$Y, na.rm = TRUE)))
    }else{
        return(1)
    }
    
}
orderGeneExpr <- function(ggData, GeneExprDotOrd, coln) {
    if(nrow(ggData)>0 && coln %in% colnames(ggData)){
        if (GeneExprDotOrd == "Max-1st") {
            ggData <- ggData[order(ggData[, coln, with = FALSE])]
        } else if (GeneExprDotOrd == "Min-1st") {
            ggData <- ggData[order(-ggData[, coln, with = FALSE])]
        } else if (GeneExprDotOrd == "Random") {
            ggData <- ggData[sample(nrow(ggData))]
        }  
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
    if (isTRUE(!is.na(inpConf[inpConf$UI == ui_key[1]]$fCL))) {
        ggCol <- extractGrpColor(inpConf, ui_key)
        ggCol <- ggCol[levels(ggData[[coln]])]
    }
    return(ggCol)
}
#' @importFrom ggplot2 coord_fixed
fixCoord <- function(ggOut, aspectRatio, ratio) {
    # if (aspectRatio == "Square") {
    #     ggOut <- ggOut + coord_fixed(ratio = ratio)
    # } else if (aspectRatio == "Fixed") {
    #     ggOut <- ggOut + coord_fixed()
    # }
    ggOut$meta$fixCoord <- list(aspectRatio=aspectRatio, ratio=ratio)
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
#' @importFrom ggplot2 geom_polygon
pointPlot <- function(
        ggOut,
        pointSize,
        fontSize = 24,
        labelsFontFamily = 'Helvetica',
        dimRedX,
        dimRedY,
        keepXYlables,
        shape = 16,
        inpCellBorder = FALSE,
        cellborder = NULL,
        cellSegColor = NA,
        cellSegAlpha = 1) {
    ggOut <- ggOut + geom_point(size = pointSize, shape = 16) +
        xlab(dimRedX) + ylab(dimRedY) +
        sctheme(base_size = fontSize,
                family = labelsFontFamily,
                XYval = keepXYlables)
    if (isTRUE(inpCellBorder)) {
        if('sampleID' %in% colnames(ggOut$data)){
            cellborder <- merge(cellborder, ggOut$data,
                                by='sampleID')
        }else{#molecules without cell id, so can not merge
        }
        sampleID <- factor(cellborder$sampleID,
                           levels=ggOut$data$sampleID)
        cellborder <- cellborder[order(sampleID, 
                                       cellborder$idx),]
        ggOut <- ggOut +
            geom_polygon(aes(x=.data[["x"]],
                             y=.data[["y"]],
                             group=.data[["sampleID"]],
                             fill=.data[["val"]]),
                         color = cellSegColor,
                         alpha = cellSegAlpha,
                         inherit.aes = FALSE,
                         data = cellborder,
                         show.legend = FALSE)
    }
    ggOut
}
#' @importFrom ggplot2 geom_raster scale_fill_gradient annotate
#' @importFrom ggnewscale new_scale_fill
ggXYplot <- function(ggData, backgroundImageData) {
    p <- ggplot(ggData, aes(
        .data[["X"]], .data[["Y"]],
        color = .data[["val"]] #,customdata = data[["sampleID"]]
    ))
    if(!missing(backgroundImageData)){
        if(all(c('x','y','value') %in% colnames(backgroundImageData))){
            xmin <- min(backgroundImageData$x)
            xmax <- max(backgroundImageData$x)
            ymin <- min(backgroundImageData$y)
            ymax <- max(backgroundImageData$y)
            backgroundImageData <- 
                backgroundImageData[backgroundImageData$value!=0, ,drop=FALSE]
            
            p <- p +
                annotate("rect",
                         xmin = xmin, xmax = xmax,
                         ymin = ymin, ymax = ymax,
                         fill = "black") + 
                geom_raster(data = backgroundImageData,
                            aes(x = .data[["x"]], y = .data[["y"]],
                                fill = .data[["value"]]),
                            inherit.aes = FALSE,
                            show.legend = FALSE,
                            na.rm=TRUE) +
                scale_fill_gradient(na.value='black', low='black', high ='white') +
                ggnewscale::new_scale_fill()
        }
    }
    return(p)
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
getCoexpVal <- function(ggData, dataset, geneIdMap, gene1, gene2,
                        geneType='gene', geneIdMap2) {
    ggData$val1 <- read_exprs(dataset, geneIdMap[gene1], valueOnly = TRUE)
    if(geneType=='gene'){
        ggData$val2 <-
            read_exprs(dataset, geneIdMap[gene2], valueOnly = TRUE) 
    }else{## gene score
        ggData$val2 <- read_exprs(dataset, geneIdMap2[gene2], valueOnly = TRUE,
                                  h5_fn = ifelse(geneType=='gene',
                                                 .globals$filenames$sc1gexpr, 
                                                 .globals$filenames$sc1gscore))
    }
    ggData[ggData$val1 < 0]$val1 <- 0
    ggData[ggData$val2 < 0]$val2 <- 0
    return(ggData)
}
getFilterKey2 <- function(coln){
    paste0(coln, '2')
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
        valueFilterCutoff,
        valueFilterCutoff2) {
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
                if(!missing(valueFilterCutoff2)){
                    valueFilterKey2 <- ifelse(grepl('1$', valueFilterKey),
                                              sub('1$', '2', valueFilterKey),
                                              sub('2$', '1', valueFilterKey))
                    ggData <-
                        cbind(
                            ggData,
                            subValue =
                                meta[, config[
                                    config$UI == valueFilterKey2]$ID,
                                    with = FALSE])
                    colnames(ggData)[ncol(ggData)] <- getFilterKey2(coln)
                }
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

#' @importFrom data.table .I
filterCells <- function(
        ggData,
        subsetCellKey,
        subsetCellVal,
        valueFilterKey,
        valueFilterCutoff,
        valueFilterCutoff2,
        inpConf,
        subsetCellPct=100,
        lassoSelected) {
    if(missing(lassoSelected)){
        keep <- rep(TRUE, nrow(ggData))
    }else{
        keep <- lassoSelected
    }
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
            if(length(valueFilterCutoff)>1){
                keep <- keep & ggData[[valueFilterKey]] <= valueFilterCutoff[2]
            }
        }
        if (!missing(valueFilterCutoff2)) {
            if(length(valueFilterCutoff2)>0){
                valueFilterKey2 <- getFilterKey2(valueFilterKey)
                keep <- keep & ggData[[valueFilterKey2]] >= valueFilterCutoff2[1]
                if(length(valueFilterCutoff2)>1){
                    keep <- keep & ggData[[valueFilterKey2]] <= valueFilterCutoff2[2]
                }
            }
        }
    }
    if(isTRUE(subsetCellPct!=100)){
        subsetCellPct <- subsetCellPct[1]/100
        ## subset cells by each factors
        factor_columns <- ggData[, vapply(.SD, is.factor, logical(1L))]
        factor_columns <- colnames(ggData)[factor_columns]
        ggData$pre_filter_keep <- keep
        set.seed(42) ## keep the seed to make sure always same output
        sel <- ggData[, {
            idx <- rep(FALSE, .N)
            ok <- which(get('pre_filter_keep'))
            n <- length(ok)
            if(n>0){
                choose <- ok[sample(n, round(n*subsetCellPct))]
                idx[choose] <- TRUE
            }
            list(sel = idx, orig_order = .I)
        }, by=factor_columns]
        adminMsg(paste0('cell number after/before percentage filter is ',
                        sum(sel$sel),'/', sum(keep), ' (',
                       round(100*sum(sel$sel)/sum(keep), digits = 2), '%)'),
                 type = 'message')
        orig_order <- sel$orig_order
        sel <- sel[order(orig_order)]
        sel$orig_order <- NULL
        for(coln in colnames(sel)){
            if(coln!='sel'){
                stopifnot(identical(ggData[[coln]], sel[[coln]]))
            }
        }
        keep <- sel$sel
    }
    return(keep)
}

getFilteredCellNum <- function(
        inpConf,
        inpMeta,
        dimRedX,
        dimRedY,
        cellinfoID,
        cellinfoName=cellinfoID,
        subsetCellKey,
        subsetCellVal,
        subsetCellPct=100,
        dataset,
        geneIdMap,
        valueFilterKey,
        valueFilterCutoff,
        valueFilterCutoff2,
        ...) {
    subFilterColname <- 'subValue'
    subsetCellKey <- subsetCellKey[subsetCellKey!="N/A"]
    subsetCellVal <- namedSubsetCellVals(subsetCellKey, subsetCellVal)
    if(cellinfoName!=cellinfoID){
        if(is.na(cellinfoName)||cellinfoName==""){
            cellinfoName <- cellinfoID
        }
    }
    # Prepare ggData
    ggData <- inpMeta[, unique(c(
        inpConf[inpConf$UI == dimRedX]$ID,
        inpConf[inpConf$UI == dimRedY]$ID,
        inpConf[inpConf$UI == cellinfoID]$ID,
        inpConf[inpConf$UI %in% subsetCellKey]$ID,
        inpConf[inpConf$UI == cellinfoName]$ID)),
        with = FALSE]
    if (ncol(ggData) < 3)
        return(0)
    colnames(ggData)[c(1,2)] <- c("X", "Y")
    dots <- list(...)
    if('interactive' %in% names(dots)){
        if(isTRUE(dots$interactive)){
            ggData$sampleID <- inpMeta$sampleID
        }
    }
    lassoSelected <- rep(TRUE, nrow(ggData))
    if('selectedCellIDs' %in% names(dots)){
        if(length(dots$selectedCellIDs)){
            if(all(dots$selectedCellIDs %in% inpMeta$sampleID)){
                lassoSelected <- inpMeta$sampleID %in% dots$selectedCellIDs
            }
        }
    }
    ggData <-
        cbindFilterValues(
            ggData,
            inpConf,
            inpMeta,
            subFilterColname,
            geneIdMap,
            dataset,
            valueFilterKey,
            valueFilterCutoff,
            valueFilterCutoff2
        )
    rat <- getRatio(ggData)
    keep <- filterCells(
        ggData,
        subsetCellKey,
        subsetCellVal,
        subFilterColname,
        valueFilterCutoff,
        valueFilterCutoff2,
        inpConf, 
        subsetCellPct,
        lassoSelected)
    return(sum(keep))
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
