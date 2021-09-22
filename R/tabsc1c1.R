tab1c1 <- tabPanel( 
  HTML("Violinplot / Boxplot"),  
  h4("Cell information / gene expression violin plot / box plot"), 
  "In this tab, users can visualise the gene expression or continuous cell information ",  
  "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("sc1c1inp1", "Cell information (X-axis):", 
                  choices = NULL) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group cells by",  
               content = c("Select categorical cell information to group cells by",  
                           "- Single cells are grouped by this categorical covariate",  
                           "- Plotted as the X-axis of the violin plot / box plot")),
      selectInput("sc1c1inp1a", "Cell information to subset:", 
                  choices = NULL) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to subset by:", 
               content = c("Select categorical cell information to sebset cells by", 
                           "- cells are shown in different subsets")), 
      uiOutput("sc1c1inp1b.ui"),
      selectInput("sc1c1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell Info / Gene to plot", 
               content = c("Select cell info / gene to plot on Y-axis", 
                           "- Can be continuous cell information (e.g. nUMIs / scores)", 
                           "- Can also be gene expression")), 
      uiOutput("sc1c1inp1c.ui"),
      radioButtons("sc1c1typ", "Plot type:", 
                   choices = c("violin", "boxplot"), 
                   selected = "violin", inline = TRUE), 
      checkboxInput("sc1c1pts", "Show data points", value = FALSE), 
      actionButton("sc1c1tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.sc1c1tog % 2 == 1", 
        sliderInput("sc1c1siz", "Data point size:",  
                    min = 0, max = 4, value = 1.25, step = 0.25),  
        radioButtons("sc1c1psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium", inline = TRUE), 
        radioButtons("sc1c1fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("sc1c1oup.ui"),  
           downloadButton("sc1c1oup.pdf", "Download PDF"),  
           downloadButton("sc1c1oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("sc1c1oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("sc1c1oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
)