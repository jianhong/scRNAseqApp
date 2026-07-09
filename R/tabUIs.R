tabsubTitleUI <- function(id, title, description){
    tagList(
        htmlOutput(NS(id, paste0(title, "SubTitle"))),
        description,
        br(),br()
    )
}

#' @importFrom grDevices pdfFonts
fontUI <- function(id, fontsizePrefix='plot'){
    pdfs <- names(pdfFonts())
    if(capabilities()[["X11"]]){
        x11s <- names(grDevices::X11Fonts())
    } else {
        x11s <- c()
    }
    if(length(pdfs)>0){
        if(length(x11s)>0){
            family <- intersect(pdfs, x11s)
        }else{
            family <- pdfs
        }
    }else{
        if(length(x11s)>0){
            family <- x11s
        }else{
            family <- c('Helvetica', 'serif', 'mono')
        }
    }
    if('Helvetica' %in% family){
        selected <- 'Helvetica'
    }else{
        selected <- family[1]
    }
    tagList(
        selectInput(NS(id, paste0(fontsizePrefix, 'fml')),
                    "Font family:",
                    choices = family, selected = selected),
        numericInput(
            NS(id, paste0(fontsizePrefix, "fsz")), "Font size:",
            value = 24, min=3, max = 72, step = .5)
    )
}

graphicsControlUI <- function(id, GeneExpraspSelect="Square"){
    tagList(
        actionButton(
            NS(id, "graphicTog"),
            "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.graphicTog % 2 == 1",
            ns = NS(id),
            column(
                6,
                sliderInput(
                    NS(id, "GeneExprsiz"),
                    "Point size:",
                    min = 0, max = 4, value = 1.25, step = 0.25),
                radioButtons(
                    NS(id, "GeneExprpsz"),
                    "Plot size:",
                    choices = c("Small", "Medium", "Large"),
                    selected = "Medium", inline = TRUE),
                fontUI(id, "GeneExpr")
            ),
            column(
                6, radioButtons(
                    NS(id, "GeneExprasp"), "Aspect ratio:",
                    choices = c("Square", "Fixed", "Free"),
                    selected = GeneExpraspSelect, inline = TRUE),
                checkboxInput(
                    NS(id, "GeneExprtxt"), "Show axis text", value = FALSE)
            )
        )
    )
}

resizablePlotContainer <- function(leftUI, rightUI){
    div(class = "resizable-container",
        # Left panel
        div(class = "panel panel-left", leftUI),
        div(class = "divider"),
        # Right panel
        div(class = "panel panel-right", rightUI)
    )
}

NS0 <- function(namespace, id, postfix){
    NS(namespace, id=paste0(id, postfix))
}
geneAccPlotControlUI <- function(
        id, postfix=1,
        colorNames=availableThemes("sequence")){
    tagList(
        actionButton(
            NS0(id, "GeneExprtog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0("input.GeneExprtog", postfix, " % 2 == 1"),
            ns=NS(id),
            radioButtons(
                NS0(id, "GeneExprcol", postfix), "Colour:",
                inline = TRUE,
                choices = colorNames,
                selected = colorNames[1]),
            sliderInput(#region selector
                NS(id, 'regionselector'), label = NULL,
                min=0, max = 100,
                step = 1,
                value = c(0, 100),
                ticks = FALSE,
                width = "100%"),
            actionButton(
                NS(id, 'regionsubmit'),
                label = "change region",
                width = "100%"
            )
        )
    )
}
manuXYlimOriUI <- function(id, postfix){
    div(style='display:none',
        numericInput(
            NS0(id, "manuXlimOriMin", postfix),
            label = '',
            value = NULL
        ),
        numericInput(
            NS0(id, "manuXlimOriMax", postfix),
            label = '',
            value = NULL
        ),
        numericInput(
            NS0(id, "manuYlimOriMin", postfix),
            label = '',
            value = NULL
        ),
        numericInput(
            NS0(id, "manuYlimOriMax", postfix),
            label = '',
            value = NULL
        )
    )
}

geneExprPlotControlUI <- function(
        id, postfix=1,
        colorNames=availableThemes("sequence"),
        linkXYlim = FALSE){
    tagList(
        actionButton(
            NS0(id, "GeneExprtog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0("input.GeneExprtog", postfix, " % 2 == 1"),
            ns=NS(id),
            radioButtons(
                NS0(id, "GeneExprtype", postfix), "Plot type",
                choices = c("Dotplot", "Ridgeplot"),
                selected = "Dotplot"),
            div(id = paste0(NS0(id, "Cell3Div", postfix), 'Menucontainer'),
                conditionalPanel(
                    condition = paste0(
                        "input.GeneExprtype", postfix, " == 'Dotplot'"),
                    ns=NS(id),
                    radioButtons(
                        NS0(id, "GeneExprcol", postfix), "Colour:",
                        inline = TRUE,
                        choices = colorNames,
                        selected = colorNames[1]),
                    radioButtons(
                        NS0(id, "GeneExprord", postfix), "Plot order:",
                        choices = c("Max-1st", "Min-1st",
                                    "Original", "Random"),
                        selected = "Max-1st", inline = TRUE),
                    checkboxInput(
                        NS0(id, "GeneExprhid", postfix),
                        "Hide filtered cells", value = FALSE),
                    checkboxInput(
                        NS0(id, "usingPan", postfix),
                        "Using wheel to zoom in/out", value = FALSE),
                    actionButton(
                        NS0(id, "GeneExprrgb", postfix),
                        "Manually set max color value",
                        inline = TRUE),
                    conditionalPanel(
                        condition =
                            paste0("input.GeneExprrgb", postfix, " % 2 ==1"),
                        ns=NS(id),
                        numericInput(
                            NS0(id, "GeneExprrg", postfix), "Max value:",
                            value = 100)),
                    checkboxInput(
                        NS0(id, 'GeneExprSegmentation', postfix),
                        "Show cell segmentation", value = FALSE),
                    conditionalPanel(
                        condition = paste0(
                            "input.GeneExprSegmentation", postfix, " == true"),
                        ns=NS(id),
                        sliderInput(
                            NS0(id, 'GeneExprSegAlpha', postfix),
                            "Cell segmentation alpha", value=1, 
                            min = 0, max=1, step=0.01),
                        checkboxInput(
                            NS0(id, 'GeneExprSegBorderColor', postfix),
                            "Show cell segmentation border",
                            value = FALSE
                        ),
                        conditionalPanel(
                            condition = paste0(
                                "input.GeneExprSegBorderColor", postfix, " == true"),
                            ns=NS(id),
                            colourInput(
                                NS0(id, 'GeneExprSegColor', postfix),
                                "Cell segmentation border color",
                                value = '#EEEEEE'
                            )
                        )
                    ),
                    checkboxInput(
                        NS0(id, 'GeneExprBgImg', postfix),
                        "Show spatial image", value = FALSE)
                ),
            
                actionButton(
                    NS0(id, "manuXYlimTog", postfix),
                    "Manually set x/y axis", inline = TRUE),
                conditionalPanel(
                    condition = paste0(
                        "input.manuXYlimTog", postfix, " % 2 ==1"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, "manuXlim", postfix), "Xlim range:",
                        min = -10, max = 100,
                        value = .globals$defaultLimValue,
                        step = 0.1),
                    conditionalPanel(
                        condition = paste0(
                            "input.GeneExprtype", postfix, " == 'Dotplot'"),
                        ns=NS(id),
                        sliderInput(
                            NS0(id, "manuYlim", postfix), "Ylim range:",
                            min = -10, max = 100,
                            value = .globals$defaultLimValue,
                            step = 0.1)
                    ),
                    manuXYlimOriUI(id, postfix),
                    if(linkXYlim){
                        conditionalPanel(
                            condition = paste0("input.manuXYlimTog",
                                               ifelse(postfix==2, 1, 2),
                                               " % 2 ==1"),
                            ns=NS(id),
                            checkboxInput(
                                NS0(id, 'XYlimLinker', postfix),
                                "Link X/Y Axis",
                                value = TRUE)
                        )
                    }
                )
            )
        )
    )
}
cellInfoPlotControlUI <- function(
        id, postfix=1,
        colorNames=availableThemes("sequence"),
        linkXYlim = FALSE){
    tagList(
        actionButton(
            NS0(id, "CellInfotog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0(
                "input.CellInfotog", postfix, " % 2 == 1"), ns=NS(id),
            div(style="display:inline-block",
                numericInput(inputId = NS0(id, "subsetCellPct", postfix),
                             label = 'percent:',
                             value = 100,
                             min = 1,
                             max = 100,
                             step = 0.1,
                             width = 100) %>%
                shinyhelper::helper(
                    type = "inline",
                    size = "m",
                    fade = TRUE,
                    title = "Subset cell by percentage:",
                    content = c(
                        paste(
                            "Set the percentage ",
                            "to sebset cells"),
                        "- Default 100 will show all cells.",
                        "- 50 will show half of the cells.",
                        "- 30 will show 30% of the cells."
                    )
                )),
            textOutput(NS0(id, "subsetCellNum", postfix), inline=TRUE),
            
            div(id = paste0(NS0(id, "Cell3Div", postfix), 'Menucontainer'),
                radioButtons(
                    NS0(id, "CellInfocol", postfix), "Colour (Continuous data):",
                    inline = TRUE,
                    choices = colorNames,
                    selected = colorNames[1]),
                radioButtons(
                    NS0(id, "CellInfoord", postfix), "Plot order:",
                    choices = c("Max-1st", "Min-1st",
                                "Original", "Random"),
                    selected = "Original", inline = TRUE),
                checkboxInput(
                    NS0(id, "CellInfolab", postfix),
                    "Show cell info labels", value = TRUE),
                selectInput(
                    NS0(id, 'CellInfoname', postfix),
                    "Cell info labels",
                    choices = NULL
                ),
                checkboxInput(
                    NS0(id, "CellInfohid", postfix),
                    "Hide filtered cells", value = FALSE),
                checkboxInput(
                    NS0(id, "usingPan", postfix),
                    "Using wheel to zoom in/out", value = FALSE),
                checkboxInput(
                    NS0(id, "CellInfoslingshot", postfix),
                    "Show lineages", value = TRUE),
                checkboxInput(
                    NS0(id, "CellInfoedge", postfix),
                    "Show cell-cell links", value = TRUE),
                checkboxInput(
                    NS0(id, 'CellInfoSegmentation', postfix),
                    "Show cell segmentation", value = FALSE),
                conditionalPanel(
                    condition = paste0(
                        "input.CellInfoSegmentation", postfix, " == true"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, 'CellInfoSegAlpha', postfix),
                        "Cell segmentation alpha", value=1, 
                        min = 0, max=1, step=0.01),
                    checkboxInput(
                        NS0(id, 'CellInfoSegBorderColor', postfix),
                        "Show cell segmentation border",
                        value = FALSE
                    ),
                    conditionalPanel(
                        condition = paste0(
                            "input.CellInfoSegBorderColor", postfix, " == true"),
                        ns=NS(id),
                        colourInput(
                            NS0(id, 'CellInfoSegColor', postfix),
                            "Cell segmentation border color",
                            value = '#EEEEEE'
                        )
                    )
                ),
                checkboxInput(
                    NS0(id, 'CellInfoBgImg', postfix),
                    "Show spatial image", value = FALSE),
                actionButton(
                    NS0(id, "manuXYlimTog", postfix),
                    "Manually set x/y axis", inline = TRUE),
                conditionalPanel(
                    condition = paste0(
                        "input.manuXYlimTog", postfix, " % 2 ==1"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, "manuXlim", postfix), "Xlim range:",
                        min = -10, max = 100,
                        value = .globals$defaultLimValue,
                        step = 0.1),
                    sliderInput(
                        NS0(id, "manuYlim", postfix), "Ylim range:",
                        min = -10, max = 100,
                        value = .globals$defaultLimValue,
                        step = 0.1),
                    manuXYlimOriUI(id, postfix),
                    if(linkXYlim){
                        conditionalPanel(
                            condition = paste0("input.manuXYlimTog",
                                               ifelse(postfix==2, 1, 2),
                                               " % 2 ==1"),
                            ns=NS(id),
                            checkboxInput(
                                NS0(id, 'XYlimLinker', postfix),
                                "Link X/Y Axis",
                                value = TRUE)
                        )
                    }
                )
            )
        ),
        div(style = "visibility:hidden;",
            id = paste0(NS0(id, "CellInfodup", postfix), 'container'),
            div(style="display:inline-block",
            textInput(NS0(id, "CellInfodname", postfix), "",
                      placeholder = "new name", width = "100px")),
            actionButton(NS0(id, "CellInfodup", postfix),
                         "Dup"),
            actionButton(NS0(id, "CellInforename", postfix),
                         "Ren"),
            actionButton(NS0(id, "CellInfodel", postfix),
                         "Del")
        )
    )
}
geneCoExprPlotControlUI <- function(id, postfix=1, plotly=FALSE){
    choices <- .globals$coExpColor
    if(plotly){
        choices <- c("Default", availableThemes("sequence"))
    }
    tagList(
        actionButton(NS0(id, "CoExprtog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0(
                "input.CoExprtog", postfix, " % 2 == 1"), ns=NS(id),
            radioButtons(
                NS0(id, "CoExprtype", postfix), "Plot type",
                choices = c("Dotplot", "Ridgeplot"),
                selected = "Dotplot"),
            conditionalPanel(
                condition = paste0(
                    "input.CoExprtype", postfix,
                    " == 'Dotplot'"),
                ns=NS(id),
                radioButtons(
                    NS0(id, "CoExprcol", postfix), "Colour:",
                    inline = TRUE,
                    choices = choices,
                    selected = choices[1]),
                radioButtons(
                    NS0(id, "CoExprord", postfix), "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Max-1st", inline = TRUE),
                checkboxInput(
                    NS0(id, "CoExprhid", postfix),
                    "Hide filtered cells", value = FALSE),
                checkboxInput(
                    NS0(id, 'CoExprSegmentation', postfix),
                    "Show cell segmentation", value = FALSE),
                conditionalPanel(
                    condition = paste0(
                        "input.CoExprSegmentation", postfix, " == true"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, 'CoExprSegAlpha', postfix),
                        "Cell segmentation alpha", value=1, 
                        min = 0, max=1, step=0.01),
                    checkboxInput(
                        NS0(id, 'CoExprSegBorderColor', postfix),
                        "Show cell segmentation border",
                        value = FALSE
                    ),
                    conditionalPanel(
                        condition = paste0(
                            "input.CoExprSegBorderColor", postfix, " == true"),
                        ns=NS(id),
                        colourInput(
                            NS0(id, 'CoExprSegColor', postfix),
                            "Cell segmentation border color",
                            value = '#EEEEEE'
                        )
                    )
                ),
                checkboxInput(
                    NS0(id, 'CoExprBgImg', postfix),
                    "Show spatial image", value = FALSE),
                actionButton(
                    NS0(id, "manuXYlimTog", postfix),
                    "Manually set x/y axis", inline = TRUE),
                conditionalPanel(
                    condition = paste0(
                        "input.manuXYlimTog", postfix, " % 2 ==1"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, "manuXlim", postfix), "Xlim range:",
                        min = -10, max = 100,
                        value = .globals$defaultLimValue,
                        step = 0.1),
                    sliderInput(
                        NS0(id, "manuYlim", postfix), "Ylim range:",
                        min = -10, max = 100,
                        value = .globals$defaultLimValue,
                        step = 0.1),
                    manuXYlimOriUI(id, postfix)
                ),
                checkboxInput(
                   NS0(id, "usingPan", postfix),
                   "Using wheel to zoom in/out", value = FALSE)
            ),
            conditionalPanel(
                condition = paste0(
                    "input.CoExprtype", postfix,
                    " == 'Ridgeplot'"),
                ns=NS(id),
                radioButtons(
                    NS0(id, "CoExprStreamtype", postfix), "Stream type:",
                    choices = c('mirror', 'ridge', 'proportional'),
                    selected = "proportional", inline = TRUE),
                checkboxInput(NS0(id, 'useNorm', postfix),
                              "Normalized by total",
                              value=FALSE),
                actionButton(
                    NS0(id, "manuXYlimTog", postfix),
                    "Manually set x axis", inline = TRUE),
                conditionalPanel(
                    condition = paste0(
                        "input.manuXYlimTog",
                        postfix, " % 2 ==1"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, "manuXlim", postfix), "Xlim range:",
                        min = -10, max = 100,
                        value = .globals$defaultLimValue,
                        step = 0.1),
                    manuXYlimOriUI(id, postfix)
                )
            )
        )
    )
}

boxPlotControlUI <- function(
        id, withPoints=TRUE, withColor=FALSE,
        withFontSize=TRUE,
        colorNames=availableThemes("sequence")){
    tagList(
        actionButton(
            NS(id, "plottog"), "Toggle graphics controls"),
        conditionalPanel(
            condition = "input.plottog % 2 == 1",
            ns=NS(id),
            if(withPoints) {
                sliderInput(
                    NS(id, "plotsiz"), "Data point size:",
                    min = 0, max = 4, value = 1.25, step = 0.25)
            }else{
                span()
            },
            if(withColor){
                radioButtons(
                    NS(id, "plotcols"), "Colour scheme:",
                    inline = TRUE,
                    choices = colorNames,
                    selected = colorNames[2])
            }else{
                span()
            },
            radioButtons(
                NS(id, "plotpsz"), "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE),
            if(withFontSize){
                fontUI(id)
            }else{
                span()
            }
        )
    )
}

dimensionReductionUI <- function(id, ABcolumn, Z=FALSE){
    idx <- "GeneExprdrX"
    idy <- "GeneExprdrY"
    idz <- "GeneExprdrZ"
    title <- "Dimension Reduction"
    if(!missing(ABcolumn)){
        idx <- paste0(idx, ABcolumn)
        idy <- paste0(idy, ABcolumn)
        idz <- paste0(idz, ABcolumn)
        title <- paste(title, ABcolumn)
        tagList(
            actionButton(NS0(id, "reductionTogT", ABcolumn),
                         title),
            conditionalPanel(
                condition = paste0("input.reductionTogT", ABcolumn,
                                   " % 2 == ",
                                   ifelse(ABcolumn==.globals$subsetgroup[1],
                                          0, 1)),
                ns = NS(id),
                fluidRow(
                    column(
                        12,
                        selectInput(
                            NS(id, idx),
                            "X-axis:",
                            choices = NULL),
                        selectInput(
                            NS(id, idy),
                            "Y-axis:",
                            choices = NULL),
                        if(isTRUE(Z)){
                            selectInput(
                                NS(id, idz),
                                "Z-axis:",
                                choices = NULL)
                        }else{
                            div(
                                style = "visibility:hidden;",
                                selectInput(
                                    NS(id, idz),
                                    "Z-axis:",
                                    choices = NULL)
                            )
                        }
                        )
                )
            )
        )
    }else{
        tagList(
            h4(title),
            fluidRow(
                column(
                    12,
                    selectInput(
                        NS(id, idx),
                        "X-axis:",
                        choices = NULL),
                    selectInput(
                        NS(id, idy),
                        "Y-axis:",
                        choices = NULL),
                    if(isTRUE(Z)){
                    selectInput(
                        NS(id, idz),
                        "Z-axis:",
                        choices = NULL)
                    }else{
                        div(
                            style = "visibility:hidden;",
                            selectInput(
                                NS(id, idz),
                                "Z-axis:",
                                choices = NULL)
                        )
                    }
                    )
            )
        )
    }
}
#' @importFrom magrittr %>%
subsetCellByInfoUI <- function(id, mini=FALSE, multiple=TRUE, ABcolumn){
    if(mini){
        tagList(
            uiOutput(NS(id, "subsetCellSel.ui")) %>%
                helper1(category="subsetCellInfo"),
            if(multiple) actionButton(
                NS(id, 'subsetCell.multi'),
                label="multiple") else tags$span(),
            uiOutput(NS(id, "subsetCell.ui"))
        )
    }else{
        if(missing(ABcolumn)){
            tagList(
                actionButton(NS(id, "subsetTogT"), "Toggle to subset cells"),
                conditionalPanel(
                    condition = "input.subsetTogT % 2 == 0",
                    ns = NS(id),
                    fluidRow(
                        column(9,
                               uiOutput(NS(id, "subsetCellSel.ui")) %>%
                                   helper1(category="subsetCellInfo")),
                        column(3,
                               if(multiple) actionButton(
                                   NS(id, 'subsetCell.multi'),
                                   label="multiple",
                                   class = "align-action-button")
                               else tags$span())),
                    uiOutput(NS(id, "subsetCell.ui"))
                )
            )
        }else{
            tagList(
                actionButton(NS0(id, "subsetTogT", ABcolumn),
                             paste("Toggle to subset cells setting",
                                   ABcolumn)),
                conditionalPanel(
                    condition = paste0("input.subsetTogT", ABcolumn,
                                       " % 2 == ",
                                       ifelse(ABcolumn==.globals$subsetgroup[1],
                                              0, 1)),
                    ns = NS(id),
                    fluidRow(
                        column(
                            7,
                            uiOutput(NS0(id, "subsetCellSel.ui", ABcolumn))
                        ),
                        column(
                            3,
                            if(multiple) actionButton(
                                NS0(id, 'subsetCell.multi', ABcolumn),
                                label="multiple", class = "align-action-button")
                            else tags$span()
                        )
                    ),
                    
                    uiOutput(NS0(id, "subsetCell.ui", ABcolumn))
                )
            )
        }
    }
}
#' @importFrom magrittr %>%
subsetCellByFilterUI <- function(
        id,
        label="Cell Info/Gene name to subset:",
        title=NULL,
        content=NULL){
    tagList(
        selectInput(
            NS(id, "filterCell"),
            label=label,
            choices = NULL) %>%
            helper1(category="subsetCellInfo", title=title, content=content),
        uiOutput(NS(id, "filterCell.ui"))
    )
}

geneExprDotPlotUI <- function(id, postfix=1, editor=FALSE){
    tagList(
        fluidRow(column(12, uiOutput(NS0(id, "GeneExproup.ui", postfix)))),
        div(style="display:inline-block",
            selectInput(
                NS0(id, "GeneExproup.fmt", postfix),
                "Format:", width = "75px",
                choices = .globals$figFormats,
                selected = .globals$figFormats[1])),
        div(style="display:none",
            checkboxInput(NS0(id, 'GeneExproupDimT', postfix),
                     label = NULL, width = 0, value=TRUE)),
        div(style="display:inline-block",
            numericInput(
                NS0(id, "GeneExproup.h", postfix),
                "height:", width = "60px",
                min = 2, max = 20, value = .globals$figHeight, step = 0.5)),
        div(style="display:inline-block",
        conditionalPanel(
            condition = paste0('input.GeneExproupDimT', postfix, " == true"),
            ns = NS(id),
            div(style="display:inline-block",
                numericInput(
                    NS0(id, "GeneExproup.w", postfix),
                    "width:", width = "60px",
                    min = 2, max = 20, value = .globals$figWidth, step = 0.5))
        )),
        downloadButton(NS0(id, "GeneExproup.dwn", postfix), "download"),
        div(style="display:inline-block",
            conditionalPanel(
                condition = paste0('input.GeneExproupDimT', postfix, " == false"),
                ns = NS(id),
                div(style="display:inline-block",
                    selectInput(NS0(id, 'GeneExproupSelMethod', postfix),
                                label = 'Lasso:', width='75px',
                                choices = c('new', 'add', 'del'),
                                selected = 'new')),
                div(style="display:inline-block",
                    actionButton(NS0(id, 'GeneExproupSelIDs', postfix),
                                 label = 'Set lasso selection',
                                 disabled = FALSE))
            )),
        if(editor){
            tagList(
                uiOutput(NS0(id, 'GeneExproup.info', postfix))
            )
        }else{
            div(style = "visibility:hidden;",
                textInput(NS0(id, 'GeneExpext.info', postfix),
                          label='', value = '')
            )
        }
    )
}

#' @importFrom magrittr %>%
cellInfoUI <- function(id, postfix=1){
    tagList(
        selectInput(
            NS0(id, "CellInfo", postfix), "Cell information:",
            choices = NULL) %>%
            helper1(category="cellInfo")
    )
}
#' @importFrom DT DTOutput
cellInfoTblUI <- function(id, postfix=1){
    tagList(
        actionButton(
            NS0(id, "CellInfoTableTog", postfix),
            "Toggle to show cell numbers / statistics"),
        conditionalPanel(
            condition = paste0("input.CellInfoTableTog", postfix, " % 2 == 1"),
            ns=NS(id),
            h4("Cell numbers / statistics"),
            radioButtons(
                NS0(id, "GeneExprsplt", postfix),
                "Split continuous cell info into:",
                choices = c("Quartile", "Decile"),
                selected = "Decile", inline = TRUE),
            DTOutput(NS0(id, "GeneExpr.dt", postfix))
        )
    )
}

#' @importFrom magrittr %>%
geneExprUI <- function(id, postfix=1){
    tagList(
        selectInput(
            NS0(id, "GeneName", postfix),
            'Gene name:', choices=NULL) %>%
            helper1(category="geneName")
    )
}

geneAccUI <- function(id, postfix=1){
    tagList(
        textInput(
            NS0(id, "coord", postfix),
            "Coordinates:", value=NULL),
        div(
            class = "acccontroler",
            actionButton(#zoom in
                NS(id, 'zoomin'), label = '', title="Zoom In",
                icon = icon('plus'),
                class = "submodule-dot-btn submodule-icon",
                style = "background: #ED594A;"),
            actionButton(#zoom out
                NS(id, 'zoomout'), label = '', title="Zoom Out",
                icon = icon('minus'),
                class = "submodule-dot-btn submodule-icon",
                style = "background: #FDD800;"),
            actionButton(#move left
                NS(id, 'moveleft'), label = '', title="Move Left",
                icon = icon('angle-left'),
                class = "submodule-dot-btn submodule-icon",
                style = "background: #006EF4;"),
            actionButton(#move right
                NS(id, 'moveright'), label = '', title="Move Right",
                icon = icon('angle-right'),
                class = "submodule-dot-btn submodule-icon",
                style = "background: #5AC05A;")
        )
    )
}

#' @importFrom magrittr %>%
xaxisCellInfoUI <- function(id){
    tagList(
        selectInput(
            NS(id, "CellInfoX"), "Cell information (X-axis):",
            choices = NULL) %>%
            helper1(category="cellInfoX")
    )
}

#' @importFrom magrittr %>%
yaxisCellInfoUI <- function(id){
    tagList(
        selectInput(
            NS(id, "CellInfoY"), "Cell Info / Gene name (Y-axis):",
            choices=NULL) %>%
            helper1(category="cellInfoY")
    )
}
# subMOduleUIs
subModuleContainerUI <- function(id, mainSelectUI, menuUI, contentUI){
    tagList(
        div(
            class="submodule-container",
            div(
                class="submodule-row",
                div(
                    class="submodule-column submodule-left",
                    actionButton(
                        NS(id, 'close'), label = '',
                        icon = icon('close'),
                        class = "submodule-dot-btn submodule-icon",
                        style = "background: #ED594A;"),
                    actionButton(
                        NS(id, 'movedown'), label = '',
                        icon = icon('angle-down'),
                        class = "submodule-dot-btn submodule-icon",
                        style = "background: #FDD800;"),
                    actionButton(
                        NS(id, 'moveup'), label = '',
                        icon = icon('angle-up'),
                        class = "submodule-dot-btn submodule-icon",
                        style = "background: #006EF4;"),
                    actionButton(
                        NS(id, 'resize'), label = '',
                        icon = icon('arrows-left-right'),
                        class = "submodule-dot-btn submodule-icon",
                        style = "background: #5AC05A;")
                ),
                div(
                    class="submodule-column submodule-middle",
                    mainSelectUI
                ),
                div(
                    class="submodule-column submodule-right",
                    menuUI
                )
            ),
            div(
                class="submodule-content",
                div(
                    contentUI
                )
            )
        )
    )
}

subsetGrpRadioButton <- function(id, label, selected, inline=TRUE){
    if(is.logical(selected)){
        if(selected){
            selected <- .globals$subsetgroup[1]
        }
    }
    if(selected %in% .globals$subsetgroup){
        radioButtons(
            inputId = id,
            label = label,
            choices = .globals$subsetgroup,
            selected = selected,
            inline = inline
        )
    }
}

contextMenuCellInfoUI <- function(
        id, postfix=1,
        colorNames=availableThemes("sequence"),
        group=.globals$subsetgroup[1],
        coorgrp=.globals$subsetgroup[1]){
    tagList(
        actionButton(
            NS0(id, "CellInfotog", postfix), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.CellInfotog", postfix, " % 2 == 1"),
                ns=NS(id),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfoCoor', postfix),
                    label = "Reduction group:",
                    selected = coorgrp,
                    inline=TRUE) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "Set the reduction group:",
                        content = c(
                            "Group A or GroupB in above",
                            "- A: using dimension reduction A",
                            "- B: using dimension reduction B"
                        )
                    ),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfosubgrp', postfix),
                    label = "Subset setting group:",
                    selected = group,
                    inline=TRUE) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "Set the subset group:",
                        content = c(
                            "Group A or GroupB in above",
                            "- A: using subset cells setting A",
                            "- B: using subset cells setting B"
                        )
                    ),
                div(style="display:inline-block",
                numericInput(inputId = NS0(id, "subsetCellPct", postfix),
                             label = 'percent:',
                             value = 100,
                             min = 1,
                             max = 100,
                             step = 0.1,
                             width = 100) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "Subset cell by percentage:",
                        content = c(
                            paste(
                                "Set the percentage ",
                                "to sebset cells"),
                            "- Default 100 will show all cells.",
                            "- 50 will show half of the cells.",
                            "- 30 will show 30% of the cells."
                        )
                    )),
                textOutput(NS0(id, "subsetCellNum", postfix), inline=TRUE),
                radioButtons(
                    NS0(id, "CellInfocol", postfix),
                    "Colour (Continuous data):",
                    inline = TRUE,
                    choices = colorNames,
                    selected = colorNames[1]),
                radioButtons(
                    NS0(id, "CellInfoord", postfix), "Plot order:",
                    choices = c("Max-1st", "Min-1st",
                                "Original", "Random"),
                    selected = "Original", inline = TRUE),
                checkboxInput(
                    NS0(id, "CellInfolab", postfix),
                    "Show cell info labels", value = TRUE),
                checkboxInput(
                    NS0(id, "CellInfoslingshot", postfix),
                    "Show lineages", value = TRUE),
                div(style="display:none",
                    checkboxInput(
                        NS0(id, 'CellInfoSegmentation', postfix),
                        "Show cell segmentation", value = FALSE),
                    conditionalPanel(
                        condition = paste0(
                            "input.CellInfoSegmentation", postfix, " == true"),
                        ns=NS(id),
                        sliderInput(
                            NS0(id, 'CellInfoSegAlpha', postfix),
                            "Cell segmentation alpha", value=1, 
                            min = 0, max=1, step=0.01),
                        checkboxInput(
                            NS0(id, 'CellInfoSegBorderColor', postfix),
                            "Show cell segmentation border",
                            value = FALSE
                        ),
                        conditionalPanel(
                            condition = paste0(
                                "input.CellInfoSegBorderColor", postfix,
                                " == true"),
                            ns=NS(id),
                            colourInput(
                                NS0(id, 'CellInfoSegColor', postfix),
                                "Cell segmentation border color",
                                value = '#EEEEEE'
                            )
                        )
                    )
                ),
                checkboxInput(
                    NS0(id, 'CellInfoBgImg', postfix),
                    "Show spatial image", value = FALSE),
                checkboxInput(
                    NS0(id, "CellInfoedge", postfix),
                    "Show cell-cell links", value = TRUE),
                checkboxInput(
                    NS0(id, "CellInfohid", postfix),
                    "Hide filtered cells", value = FALSE),
                div(style="display:none",
                        checkboxInput(
                        NS0(id, "usingPan", postfix),
                        "Using wheel to zoom in/out", value = FALSE))
            )
        )
    )
}
contextMenuGeneExprUI <- function(
        id, postfix=1,
        colorNames=availableThemes("sequence"),
        group=.globals$subsetgroup[1],
        coorgrp=.globals$subsetgroup[1]){
    tagList(
        actionButton(
            NS0(id, "GeneExprtog", postfix), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.GeneExprtog", postfix, " % 2 == 1"),
                ns=NS(id),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfoCoor', postfix),
                    label = "Reduction group:",
                    selected = coorgrp,
                    inline=TRUE),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfosubgrp', postfix),
                    label = "Subset setting group:",
                    selected = group,
                    inline=TRUE),
                radioButtons(
                    NS0(id, "GeneExprtype", postfix), "Plot type",
                    choices = c("Dotplot", "Ridgeplot"),
                    selected = "Dotplot"),
                conditionalPanel(
                    condition = paste0(
                        "input.GeneExprtype",
                        postfix, " == 'Dotplot'"),
                    ns=NS(id),
                    radioButtons(
                        NS0(id, "GeneExprcol", postfix), "Colour:",
                        inline = TRUE,
                        choices = colorNames,
                        selected = colorNames[1]),
                    radioButtons(
                        NS0(id, "GeneExprord", postfix), "Plot order:",
                        choices = c("Max-1st", "Min-1st",
                                    "Original", "Random"),
                        selected = "Max-1st", inline = TRUE),
                    checkboxInput(
                        NS0(id, "GeneExprhid", postfix),
                        "Hide filtered cells", value = FALSE),
                    div(style="display:none",
                        checkboxInput(
                            NS0(id, "usingPan", postfix),
                            "Using wheel to zoom in/out", value = FALSE)),
                    actionButton(
                        NS0(id, "GeneExprrgb", postfix),
                        "Manually set max color value",
                        inline = TRUE),
                    conditionalPanel(
                        condition = paste0(
                            "input.GeneExprrgb",
                            postfix, " % 2 ==1"),
                        ns=NS(id),
                        numericInput(
                            NS0(id, "GeneExprrg", postfix), "Max value:",
                            value = 100))
                ),
                conditionalPanel(
                    condition = paste0(
                        "input.GeneExprtype", postfix,
                        " == 'Ridgeplot'"),
                    ns=NS(id),
                    actionButton(
                        NS0(id, "manuXYlimTog", postfix),
                        "Manually set x axis", inline = TRUE),
                    conditionalPanel(
                        condition = paste0(
                            "input.manuXYlimTog",
                            postfix, " % 2 ==1"),
                        ns=NS(id),
                        sliderInput(
                            NS0(id, "manuXlim", postfix), "Xlim range:",
                            min = -10, max = 100,
                            value = .globals$defaultLimValue,
                            step = 0.1),
                        manuXYlimOriUI(id, postfix)
                    )
                )
            )
        )
    )
}
contextMenuCoExprUI <- function(
        id, postfix=1,
        colorNames=availableThemes("sequence"),
        plotly = FALSE,
        group=.globals$subsetgroup[1],
        coorgrp=.globals$subsetgroup[1]){
    choices <- .globals$coExpColor
    if(plotly){
        choices <- c("Default", availableThemes("sequence"))
    }
    tagList(
        actionButton(
            NS0(id, "CoExprtog", postfix), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.CoExprtog", postfix, " % 2 == 1"),
                ns=NS(id),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfoCoor', postfix),
                    label = "Reduction group:",
                    selected = coorgrp,
                    inline=TRUE),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfosubgrp', postfix),
                    label = "Subset setting group:",
                    selected = group,
                    inline=TRUE),
                radioButtons(
                    NS0(id, "CoExprcol", postfix), "Colour:",
                    choices = choices,
                    selected = choices[1]),
                radioButtons(
                    NS0(id, "CoExprord", postfix), "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Max-1st", inline = TRUE),
                checkboxInput(
                    NS0(id, "CoExprhid", postfix),
                    "Hide filtered cells", value = FALSE),
                div(style="display:none",
                    checkboxInput(
                        NS0(id, "usingPan", postfix),
                        "Using wheel to zoom in/out", value = FALSE))
            )
        )
    )
}
contextMenuPropUI <- function(id, postfix=1, group = FALSE){
    tagList(
        actionButton(
            NS(id, "Proptog"), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.Proptog", " % 2 == 1"), ns=NS(id),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfosubgrp', postfix),
                    label = "Subset setting group:",
                    selected = group,
                    inline=TRUE),
                radioButtons(
                    NS(id, "plottyp"),
                    "Plot value:",
                    choices = c("Proportion", "CellNumbers"),
                    selected = "Proportion", inline = TRUE),
                checkboxInput(
                    NS(id, "plotflp"),
                    "Flip X/Y", value = FALSE),
                checkboxInput(
                    NS(id, "plotord"),
                    "Reorder the contents", value = FALSE
                ),
                conditionalPanel(
                    condition = "input.plotord % 2 == 1",
                    ns=NS(id),
                    uiOutput(outputId = NS(id, "plotXord")),
                    uiOutput(outputId = NS(id, "plotYord"))
                )
            )
        )
    )
}
contextMenuViolinUI <- function(id, postfix=1, group = FALSE){
    tagList(
        actionButton(
            NS(id, "Propviolin"), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.Propviolin", " % 2 == 1"), ns=NS(id),
                subsetGrpRadioButton(
                    id = NS0(id, 'CellInfosubgrp', postfix),
                    label = "Subset setting group:",
                    selected = group,
                    inline=TRUE),
                radioButtons(
                    NS(id, "plottyp"), "Plot type:",
                    choices = c("violin", "boxplot"),
                    selected = "violin", inline = TRUE),
                checkboxInput(
                    NS(id, "plotpts"),
                    "Show data points",
                    value = FALSE),
                checkboxInput(
                    NS(id, 'addnoise'),
                    "Add noise", value = TRUE
                ),
                checkboxInput(
                    NS(id, "plotord"),
                    "Reorder the contents", value = FALSE
                ),
                conditionalPanel(
                    condition = "input.plotord % 2 == 1",
                    ns=NS(id),
                    uiOutput(outputId = NS(id, "plotXord"))
                )
            )
        )
    )
}

darkPanel <- function(){
    tabPanel(
        'darkTheme',
        value = 'darkThemeOpt',
        HTML("Dark Theme Options"),
        h4("Background and Text setting for the dark theme"),
        "In this tab, users can adjust the theme for dark mode",
        br(),
        br(),
        fluidRow(
            column(
                5,
                style = "border-right: 2px solid black",
                # ── Tabbed controls ──────────────────────────────
                tabsetPanel(
                    id = "theme_tabs",
                    tabPanel("Background",
                             br(),
                             colourInput("darktheme_bg_color", "Plot background", value = "#222"),
                             colourInput("darktheme_panel_bg", "Panel background", value = "gray20"),
                             colourInput("darktheme_grid_color", "Grid lines", value = "gray30"),
                             selectInput("darktheme_panel_border", "Panel border",
                                         choices = c("None" = "blank", "Rect" = "rect"))
                    ),
                    
                    tabPanel("Text",
                             br(),
                             colourInput("darktheme_axis_text", "Axis text", value = "#ffffff"),
                             colourInput("darktheme_axis_title", "Axis title", value = "#ffffff")
                    ),
                    
                    tabPanel("Panels",
                             br(),
                             selectInput("darktheme_grid_type", "Grid lines",
                                         choices = c("Both" = "both",
                                                     "Major only" = "major",
                                                     "None" = "none")),
                             sliderInput("darktheme_grid_size", "Grid line width",
                                         min = 0.1, max = 1.5, value = 0.3, step = 0.1)
                    ),
                    
                    tabPanel("Legend",
                             br(),
                             colourInput("darktheme_legend_text", "Legend text", value = "#ffffff"),
                             colourInput("darktheme_legend_title", "Legend title", value = "#ffffff"),
                             colourInput("darktheme_legend_bg", "Legend background", value = "#222"),
                             colourInput("darktheme_strip_bg", "Strip background", value = "gray20"),
                             colourInput("darktheme_strip_text", "Strip text", value = "#ffffff")
                    )
                ),
                hr(),
                actionButton("applyDarkTheme", "Apply theme", width = "80%", class = "btn-primary")
            ),
            column(7, 
                   plotOutput("darkThemePreview", height = "400px"))
        ),
        br()
    )
}