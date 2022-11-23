#' @importFrom htmltools htmlDependency
visitorDependencies <- function(){
  htmlDependency(name = "scRNAseqApp-assets", version = "0.0.1",
                 package = "scRNAseqApp",
                 src = "assets",
                 script = c("js/script.js"),
                 stylesheet = c("css/style.css")
  )
}
# summary box for home page
summaryBox <- function(title, value,
                       width = 4,
                       icon = "fas fa-chart-bar",
                       style = "info",
                       border = "left") {
  div(
    class = c(paste0("col-md-", width), "about-left-border"),
    div(
      class = paste0("card border-",
                     border, "-",
                     style,
                     " shadow h-100 py-2"),
      div(
        class = "card-body",
        div(
          class = "row no-gutters align-items-center",
          div(
            class = "col mr-2",
            div(
              class = paste0("text-xs font-weight-bold text-",
                             style, " text-uppercase mb-1"),
              toupper(title)
            ),
            div(
              class = "h5 mb-0 font-weight-bold text-gray-800",
              value
            )
          ),
          div(
            class = "col-auto about-large-icon about-right",
            icon(class=paste0("about-", style), icon)
          )
        )
      )
    )
  )
}

plotLoader <- function(ui, bufferStr='loading...'){
  id <- removeQuote(gsub("^.*?id=(.*?)\\s+.*$", "\\1", ui))
  tagList(
    div(
      class = 'ploader-container',
      div(
        class = 'ploader',
        id=paste0(id, '-loader'),
        bufferStr
      ),
      ui
    )
  )
}

#' Function to extract legend
#' @noRd
#' @importFrom ggplot2 ggplot_gtable ggplot_build
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# update search results
updateSearch <- function(key_words, output, symbolDict, id){
  key_words = gsub("[^a-zA-Z0-9._'\"*-]+", "", key_words)
  if(length(key_words)==1 &&
     nchar(key_words)>1 &&
     isGene(key_words, symbolDict)){## check if it is a gene
    search_res <- checkGene(key_words, id=id)
    output$search_res <-
      renderUI(search_res$UI)
    for(i in seq_along(search_res$PLOT)){
      local({
        output[[names(search_res$PLOT)[i]]] <- search_res$PLOT[[i]]
      })
    }
  }else{
    res_data <- lapply(getAppConf(), function(.ele){
      x <- paste(unlist(.ele), collapse = " ")
      m <- vapply(key_words, grepl, logical(1L), x = x, ignore.case=TRUE)
      m <- sum(m)
      return(c(m, .ele$id, .ele$title))
    })
    ## update search_res
    res_data <- do.call(rbind, res_data)
    res_data <- res_data[res_data[, 1]>0, , drop=FALSE]
    res_data <- res_data[order(res_data[, 1], decreasing = TRUE),
                         -1, drop=FALSE]
    if(nrow(res_data)>0){
      output$search_res <- renderUI(
        tags$ul(class='about-ul',
                apply(res_data, 1, function(.ele){
                  return(tags$li(
                    tags$a(href=paste0('?data=', .ele[1]),
                           .ele[2])))
                })
        )
      )
    }else{
      output$search_res <- renderUI(tags$div(
        "Sorry, I can do nothing with this. Try different one.
        If you see this sentence is fading out,
        I'm working hard on your request.
        Have a coffee."))
    }
  }
}

#' check if a symbol is a gene
#' @noRd
#' @param symbol the character to be checked
#' @param dict the gene symbol dictionary available in the data folder
isGene <- function(symbol, dict, maxEvent=3){
  if(isQuote(symbol)){
    symbol <- removeQuote(symbol)
    return(symbol %in% dict)
  }
  if(isAsterisk(symbol)){
    symbol <- isAsterisk(symbol, transform = TRUE)
    maxEvent <- 50
  }
  g <- sum(grepl(symbol, dict))
  g > 0 && g < maxEvent
}
#' check if a symbol is quoted
#' @noRd
#' @param symbol the character to be checked
isQuote <- function(symbol){
  grepl("'|\"", symbol)
}
removeQuote <- function(symbol){
  gsub("'|\"", "", symbol)
}
#' check if a symbol contain '*'
#' @noRd
#' @param symbol the character to be checked
#' @param transform change the '*' to '.*'
isAsterisk <- function(symbol, transform=FALSE){
  isT <- grepl("*", symbol, fixed = TRUE)
  if(transform){
    if(isT){
      symbol <- gsub("*", ".*", symbol, fixed = TRUE)
    }
    return(symbol)
  }
  return(isT)
}
#' get cell type column name
#' @noRd
#' @param config config data table
#' @param celltypePattern the pattern of cell type column name
getCelltypeCol <- function(config, celltypePattern='celltype'){
  groupName <- config[config$grp]$ID
  ad <- adist(celltypePattern, groupName, ignore.case = TRUE)[1, ]
  groupName[which.min(ad)][1]
}

#' waffle plot
#' @noRd
#' @param expres expression table returned by `read_exprs`
#' @param id module id
#' @param plotname the name of plotUI
#' @param numGene number of gene
#' @param groupCol group column name, used to compare two groups in the plot
#' @importFrom utils adist
wafflePlot <- function(expres, id, plotname, numGene,
                       groupCol="treatment"){
  groupValue <- expres[[groupCol]]
  if(all(as.character(groupValue)==
         as.character(expres$grpBy))){
    groupValue <- 1
  }
  list(
    UI = plotLoader(
        plotOutput(NS(id, plotname),
                   width = '100%',
                   height =
                     paste0(min(numGene*max(1,
                                            length(unique(groupValue))),
                                max(length(unique(expres$grpBy)),
                                    6)),
                            '00px'))),
    PLOT = renderPlot(scWafflePlot(expres, groupCol))
  )
}
#' get search result by gene name
#' @noRd
#' @param gene character(1L), gene name
#' @param datafolder the data folder
#' @param id namespace
#' @return Html tags for search results
checkGene <- function(gene, id){
  appconfs <- getAppConf()
  exprs <- lapply(appconfs, function(.ele){
    geneIds <- readData("sc1gene", .ele$id)
    if(isQuote(gene)){
      genenames <- geneIds[names(geneIds) %in% removeQuote(gene)]
    }else{
      gene <- isAsterisk(gene, transform = TRUE)
      genenames <- geneIds[grepl(gene, names(geneIds), ignore.case = TRUE)]
    }
    if(length(genenames)>0){
      config <- readData("sc1conf", .ele$id)
      groupName <- getCelltypeCol(config)
      ggData <-
        read_exprs(.ele$id,
                   genenames,
                   readData("sc1meta", .ele$id),
                   config, groupName, valueOnly=FALSE)
      ggData[ggData$val < 0]$val <- 0
      #waffle plot
      plotname = paste0('search-plot', .ele$id)
      groupCol <-
        ifelse(!is.null(.ele$groupCol),
               .ele$groupCol,
               getCelltypeCol(config,
                              celltypePattern =
                                .globals$groupColPattern))
      wp <- wafflePlot(ggData, id,
                       plotname,
                       length(genenames),
                       groupCol = groupCol)
      list(
        UI = tags$li(
          tags$a(href=paste0('?data=', .ele$id, '&gene=',
                             paste(names(genenames), collapse=";")),
                 .ele$title),
          wp$UI
        ),
        PLOT = wp$PLOT,
        NAME = plotname
      )
    }else{
      NULL
    }
  })
  exprs <- exprs[lengths(exprs)>0]
  if(length(exprs)==0){
    return(list(UI=tagList(), PLOT=NULL))
  }else{
    plots <- lapply(exprs, function(.ele){
      .ele$PLOT
    })
    names(plots) <- vapply(exprs, function(.ele) .ele$NAME, character(1L))
    exprs <- lapply(exprs, function(.ele){
      .ele$UI
    })
    return(
      list(
        UI=tags$ul(exprs),
        PLOT=plots
        )
      )
  }
}

# vistor plots
updateVisitor <- function(input, output, session){
  conterFilename <- "www/counter.tsv"
  ## update visitor stats
  update_visitor <- function(){
    req(input$remote_addr)
    counter <- read.delim(conterFilename, header = TRUE)
    ips <- counter$ip
    counter <- as.Date(counter$date)
    visitors <- paste(format(counter, "%d/%m/%y %H"), ips)
    current <- Sys.time()
    ip <- isolate(input$remote_addr)
    agent <- isolate(input$remote_agent)
    if(!paste(format(current, "%d/%m/%y %H"), ip) %in% visitors){
      write(paste(as.character(current), ip, agent, sep="\t"),
            conterFilename, append = TRUE)
    }
  }
  observeEvent(input$remote_addr, update_visitor())
  output$total_visitor <- renderPlot({
    counter <- read.delim(conterFilename, header = TRUE)
    counter <- as.Date(counter$date)
    counter <- counter[as.numeric(difftime(as.Date(Sys.time()), counter, units = 'days'))<730]
    counter <- table(format(counter, "%y-%m"))
    counter <- as.data.frame(counter)
    ggplot(counter, aes_string(x="Var1", y="Freq")) +
      geom_bar(stat = "identity", fill="darkorchid4") +
      theme_minimal() + xlab("") + ylab("visitor counts") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
}
