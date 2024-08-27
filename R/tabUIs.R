tabsubTitleUI <- function(id, title, description){
    tagList(
        htmlOutput(NS(id, paste0(title, "SubTitle"))),
        description,
        br(),br()
    )
}

#' @importFrom grDevices pdfFonts X11Fonts
fontUI <- function(id, fontsizePrefix='plot'){
    pdfs <- names(pdfFonts())
    x11s <- names(X11Fonts())
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
NS0 <- function(namespace, id, postfix){
    NS(namespace, id=paste0(id, postfix))
}
geneAccPlotControlUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList)){
    tagList(
        actionButton(
            NS0(id, "GeneExprtog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0("input.GeneExprtog", postfix, " % 2 == 1"),
            ns=NS(id),
            radioButtons(
                NS0(id, "GeneExprcol", postfix), "Colour:",
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
geneExprPlotControlUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList)){
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
            conditionalPanel(
                condition = paste0(
                    "input.GeneExprtype", postfix, " == 'Dotplot'"),
                ns=NS(id),
                radioButtons(
                    NS0(id, "GeneExprcol", postfix), "Colour:",
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
                        value = 100))
            ),
            conditionalPanel(
                condition = paste0(
                    "input.GeneExprtype", postfix, " == 'Ridgeplot'"),
                ns=NS(id),
                actionButton(
                    NS0(id, "GeneExprxlimb", postfix),
                    "Manually set x axis", inline = TRUE),
                conditionalPanel(
                    condition = paste0(
                        "input.GeneExprxlimb", postfix, " % 2 ==1"),
                    ns=NS(id),
                    sliderInput(
                        NS0(id, "GeneExprxlim", postfix), "Xlim range:",
                        min = -10, max = 100, value = c(-1.5, 10),
                        step = 0.1))
            )
        )
    )
}
cellInfoPlotControlUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList)){
    tagList(
        actionButton(
            NS0(id, "CellInfotog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0(
                "input.CellInfotog", postfix, " % 2 == 1"), ns=NS(id),
            radioButtons(
                NS0(id, "CellInfocol", postfix), "Colour (Continuous data):",
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
                NS0(id, "CellInfoslingshot", postfix),
                "Show lineages", value = TRUE)
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
        choices <- c("Default", names(.globals$cList))
    }
    tagList(
        actionButton(NS0(id, "CoExprtog", postfix), "Toggle plot controls"),
        conditionalPanel(
            condition = paste0(
                "input.CoExprtog", postfix, " % 2 == 1"), ns=NS(id),
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
                "Hide filtered cells", value = FALSE)
        )
    )
}

boxPlotControlUI <- function(
        id, withPoints=TRUE, withColor=FALSE,
        withFontSize=TRUE,
        colorNames=names(.globals$cList)){
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

dimensionReductionUI <- function(id){
    tagList(
        h4("Dimension Reduction"),
        fluidRow(
            column(
                12,
                selectInput(
                    NS(id, "GeneExprdrX"),
                    "X-axis:",
                    choices = NULL),
                selectInput(
                    NS(id, "GeneExprdrY"),
                    "Y-axis:",
                    choices = NULL))
        )
    )
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
        div(style="display:inline-block",
            numericInput(
                NS0(id, "GeneExproup.h", postfix),
                "height:", width = "60px",
                min = 2, max = 20, value = .globals$figHeight, step = 0.5)),
        div(style="display:inline-block",
            numericInput(
                NS0(id, "GeneExproup.w", postfix),
                "width:", width = "60px",
                min = 2, max = 20, value = .globals$figWidth, step = 0.5)),
        downloadButton(NS0(id, "GeneExproup.dwn", postfix), "download"),
        if(editor){
            tagList(
                uiOutput(NS0(id, 'GeneExproup.info', postfix))
            )
        }else{
            div()
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
            "Gene name:", choices=NULL) %>%
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
        colorNames=names(.globals$cList),
        group=FALSE){
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
                    id = NS0(id, 'CellInfosubgrp', postfix),
                    label = "Subset setting group:",
                    selected = group,
                    inline=TRUE),
                radioButtons(
                    NS0(id, "CellInfocol", postfix),
                    "Colour (Continuous data):",
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
                checkboxInput(
                    NS0(id, "CellInfohid", postfix),
                    "Hide filtered cells", value = FALSE)
            )
        )
    )
}
contextMenuGeneExprUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList),
        group = FALSE){
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
                        NS0(id, "GeneExprxlimb", postfix),
                        "Manually set x axis", inline = TRUE),
                    conditionalPanel(
                        condition = paste0(
                            "input.GeneExprxlimb",
                            postfix, " % 2 ==1"),
                        ns=NS(id),
                        sliderInput(
                            NS0(id, "GeneExprxlim", postfix), "Xlim range:",
                            min = -10, max = 100, value = c(-1.5, 10),
                            step = 0.1))
                )
            )
        )
    )
}
contextMenuCoExprUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList),
        plotly = FALSE,
        group = FALSE){
    choices <- .globals$coExpColor
    if(plotly){
        choices <- c("Default", names(.globals$cList))
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
                    "Hide filtered cells", value = FALSE)
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
