tab1a3 <- tabPanel( 
  HTML("GeneExpr vs GeneExpr"), 
  h4("Gene expression vs gene expression on dimension reduction"), 
  "In this tab, users can visualise two gene expressions side-by-side ", 
  "on low-dimensional representions.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, h4("Dimension Reduction"), 
      fluidRow( 
        column( 
          12, selectInput("sc1a3drX", "X-axis:", choices = NULL), 
          selectInput("sc1a3drY", "Y-axis:", choices = NULL))
      ) 
    ), # End of column (6 space) 
    column( 
      3, actionButton("sc1a3togL", "Toggle to subset cells"), 
      conditionalPanel( 
        condition = "input.sc1a3togL % 2 == 0", 
        selectInput("sc1a3sub1", "Cell information to subset:", 
                    choices = NULL), 
        uiOutput("sc1a3sub1.ui") 
      ) 
    ), # End of column (6 space) 
    column( 
      6, actionButton("sc1a3tog0", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.sc1a3tog0 % 2 == 1", 
        fluidRow( 
          column( 
            6, sliderInput("sc1a3siz", "Point size:", 
                           min = 0, max = 4, value = 1.25, step = 0.25), 
            radioButtons("sc1a3psz", "Plot size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE), 
            radioButtons("sc1a3fsz", "Font size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE) 
          ), 
          column( 
            6, radioButtons("sc1a3asp", "Aspect ratio:", 
                            choices = c("Square", "Fixed", "Free"), 
                            selected = "Square", inline = TRUE), 
            checkboxInput("sc1a3txt", "Show axis text", value = FALSE) 
          ) 
        ) 
      ) 
    )  # End of column (6 space) 
  ),   # End of fluidRow (4 space) 
  fluidRow( 
    column( 
      6, style="border-right: 2px solid black", h4("Gene expression 1"), 
      fluidRow( 
        column( 
          6, selectInput("sc1a3inp1", "Gene name:", choices=NULL) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Gene expression to colour cells by", 
                   content = c("Select gene to colour cells by gene expression", 
                               paste0("- Gene expression are coloured in a ", 
                                      "White-Red colour scheme which can be ", 
                                      "changed in the plot controls"))) 
        ), 
        column( 
          6, actionButton("sc1a3tog1", "Toggle plot controls"), 
          conditionalPanel( 
            condition = "input.sc1a3tog1 % 2 == 1", 
            radioButtons("sc1a3col1", "Colour:", 
                         choices = c("White-Red", "Blue-Yellow-Red", 
                                     "Yellow-Green-Purple"), 
                         selected = "White-Red"), 
            radioButtons("sc1a3ord1", "Plot order:", 
                         choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                         selected = "Max-1st", inline = TRUE) 
          ) 
        ) 
      ), 
      fluidRow(column(12, uiOutput("sc1a3oup1.ui"))), 
      downloadButton("sc1a3oup1.pdf", "Download PDF"), 
      downloadButton("sc1a3oup1.png", "Download PNG"), br(), 
      div(style="display:inline-block", 
          numericInput("sc1a3oup1.h", "PDF / PNG height:", width = "138px", 
                       min = 4, max = 20, value = 6, step = 0.5)), 
      div(style="display:inline-block", 
          numericInput("sc1a3oup1.w", "PDF / PNG width:", width = "138px", 
                       min = 4, max = 20, value = 8, step = 0.5)) 
    ), # End of column (6 space) 
    column( 
      6, h4("Gene expression 2"), 
      fluidRow( 
        column( 
          6, selectInput("sc1a3inp2", "Gene name:", choices=NULL) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Gene expression to colour cells by", 
                   content = c("Select gene to colour cells by gene expression", 
                               paste0("- Gene expression are coloured in a ", 
                                      "White-Red colour scheme which can be ", 
                                      "changed in the plot controls"))) 
        ), 
        column( 
          6, actionButton("sc1a3tog2", "Toggle plot controls"), 
          conditionalPanel( 
            condition = "input.sc1a3tog2 % 2 == 1", 
            radioButtons("sc1a3col2", "Colour:", 
                         choices = c("White-Red", "Blue-Yellow-Red", 
                                     "Yellow-Green-Purple"), 
                         selected = "White-Red"), 
            radioButtons("sc1a3ord2", "Plot order:", 
                         choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                         selected = "Max-1st", inline = TRUE) 
          ) 
        ) 
      ), 
      fluidRow(column(12, uiOutput("sc1a3oup2.ui"))), 
      downloadButton("sc1a3oup2.pdf", "Download PDF"), 
      downloadButton("sc1a3oup2.png", "Download PNG"), br(), 
      div(style="display:inline-block", 
          numericInput("sc1a3oup2.h", "PDF / PNG height:", width = "138px", 
                       min = 4, max = 20, value = 6, step = 0.5)), 
      div(style="display:inline-block", 
          numericInput("sc1a3oup2.w", "PDF / PNG width:", width = "138px", 
                       min = 4, max = 20, value = 8, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
)