tabsubTitleUI <- function(id, title, description){
    tagList(
        htmlOutput(NS(id, paste0(title, "SubTitle"))),
        description,
        br(),br()
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
                radioButtons(
                    NS(id, "GeneExprfsz"),
                    "Font size:",
                    choices = c("Small", "Medium", "Large"),
                    selected = "Medium", inline = TRUE)
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
            checkboxInput(
                NS0(id, "CellInfoslingshot", postfix),
                "Show lineages", value = TRUE)
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
                selected = "Max-1st", inline = TRUE)
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
                radioButtons(
                    NS(id, "plotfsz"), "Font size:",
                    choices = c("Small", "Medium", "Large"),
                    selected = "Medium", inline = TRUE)
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
subsetCellByInfoUI <- function(id, mini=FALSE, multiple=TRUE){
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
        tagList(
            actionButton(NS(id, "subsetTogT"), "Toggle to subset cells"),
            conditionalPanel(
                condition = "input.subsetTogT % 2 == 0",
                ns = NS(id),
                uiOutput(NS(id, "subsetCellSel.ui")) %>%
                    helper1(category="subsetCellInfo"),
                if(multiple) actionButton(
                    NS(id, 'subsetCell.multi'),
                    label="multiple") else tags$span(),
                uiOutput(NS(id, "subsetCell.ui"))
            )
        )
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

geneExprDotPlotUI <- function(id, postfix=1){
    tagList(
        fluidRow(column(12, uiOutput(NS0(id, "GeneExproup.ui", postfix)))),
        span("Download PDF/PNG "),
        div(style="display:inline-block",
            numericInput(
                NS0(id, "GeneExproup.h", postfix),
                "height:", width = "50px",
                min = 4, max = 20, value = 6, step = 0.5)),
        div(style="display:inline-block",
            numericInput(
                NS0(id, "GeneExproup.w", postfix),
                "width:", width = "50px",
                min = 4, max = 20, value = 8, step = 0.5)),
        downloadButton(NS0(id, "GeneExproup.pdf", postfix), "PDF"),
        downloadButton(NS0(id, "GeneExproup.png", postfix), "PNG")
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

contextMenuCellInfoUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList)){
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
                    "Show lineages", value = TRUE)
            )
        )
    )
}
contextMenuGeneExprUI <- function(
        id, postfix=1,
        colorNames=names(.globals$cList)){
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
        plotly = FALSE){
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
                radioButtons(
                    NS0(id, "CoExprcol", postfix), "Colour:",
                    choices = choices,
                    selected = choices[1]),
                radioButtons(
                    NS0(id, "CoExprord", postfix), "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Max-1st", inline = TRUE)
            )
        )
    )
}
contextMenuPropUI <- function(id){
    tagList(
        actionButton(
            NS(id, "Proptog"), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.Proptog", " % 2 == 1"), ns=NS(id),
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
contextMenuViolinUI <- function(id){
    tagList(
        actionButton(
            NS(id, "Propviolin"), "",
            icon = icon("bars"),
            class = "submodule-icon"),
        div(
            class="submodule-contextmenu",
            conditionalPanel(
                condition = paste0("input.Propviolin", " % 2 == 1"), ns=NS(id),
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
