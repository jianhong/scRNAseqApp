adminMsg <- function(msg, type, duration=5, close=TRUE){
  showNotification(toString(msg)[1], duration = duration,
                   closeButton = close,
                   type = type)
}

conditionHandler <- function(cond){
  type <- vapply(c("warning", "message", "error"),
                 function(x) is(cond, x),
                 logical(1L))
  type <- c("warning", "message", "error")[type]
  if(length(type)==0) type <- 'warning'
  if(type=="message"){
    progress <- get("progress_value", envir = .globals)
    if(progress < .9){
      progress <- progress + .05
    }
    assign("progress_value", progress, envir = .globals)
    get("progress", envir = .globals)$set(message=cond$message,
                                          value=progress)
  }else{
    adminMsg(cond$message, type = type[1], duration = 5, close = TRUE)
  }
}

#' @noRd
#' @param startMsg,endMsg messages for start and end of the progress
#' @param expr expression for the process
adminProcess <- function(expr, startMsg, endMsg){
  # Create a Progress object
  assign("progress", shiny::Progress$new(), envir = .globals)
  assign("progress_value", 0, envir = .globals)
  on.exit(get("progress", envir = .globals)$close())
  get("progress", envir = .globals)$set(message=startMsg,
                                        value=0)
  withCallingHandlers(
    withRestarts({expr}, muffleStop=function() NULL),
    message = conditionHandler,
    warning = conditionHandler,
    error = function(e){
      conditionHandler(e)
      invokeRestart("muffleStop")
    }
  )
  get("progress", envir = .globals)$close()
  on.exit()
  if(!missing(endMsg)){
    adminMsg(endMsg, "message")
  }
}

askNamespace <- function(...){
  pkgs <- list(...)
  lapply(pkgs, function(pkg){
    if(!requireNamespace(pkg)){
      adminMsg(paste("The", pkg, "package is required for this function!"),
               type = "error")
      return()
    }
  })
}

redirectOutput <- function(input, output, session, open=TRUE,
                           tmpf = tempfile()){
  if(open){
    sink(tmpf)
    autoInvalidate <- reactiveTimer(1000)
    observe({
      autoInvalidate()
      output$consoleOutput <- renderText(readLines(tmpf), sep = "\n")
    })

  }else{
    sink()
    autoInvalidate <- reactiveTimer(1000000)
  }
}

#' @noRd
#' @importFrom xml2 read_xml as_list
#' @param id id to be convert
#' @param type target id type
#' @param url the service url
idConverter <-
  function(id, type=c("doi", "pmid"),
           url='https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/'){
    type <- match.arg(type)
    res <- read_xml(paste0(url, "?format=xml&ids=", id))
    res <- as_list(res)
    if(!is.null(res$pmcids$record)){
      return(attr(res$pmcids$record, type))
    }
    return(NULL)
  }
updateRefIDs <- function(element, input, output, session){
  o_ele <- ifelse(element=="doi", "pmid", "doi")
  updateID <- FALSE
  updateID <-is.null(input[[o_ele]])
  if(!updateID){
    updateID <- gsub("\\s+", '', input[[o_ele]])==""
  }

  if(updateID){
    res <- idConverter(input[[element]], type=o_ele)
    if(!is.null(res)){
      updateTextInput(session,
                      o_ele,
                      value = res)
    }else{
      adminMsg(paste('Please update the correct', toupper(o_ele)), 'warning')
    }
  }
}

#' @importFrom methods is
updateRefById <- function(id, element, FUN, input, output, session){
  if(!is.null(input[[element]])){
    if(input[[element]]!=""){
      tryCatch(
        {
          suppressMessages(bibentry <- FUN(input[[element]]))
          if(is(bibentry, "bibentry")){
            updateTextAreaInput(session,
                                id,
                                value = format(bibentry, style = 'html'))
          }
          updateRefIDs(element, input, output, session)
          return(bibentry)
        },
        error = function(e){
          adminMsg(e, 'error')
        },
        warning = function(w){
          adminMsg(w, 'warning')
        }
      )

    }
  }
}
getGrpIDs <- function(config){
  x <- config$ID[config$grp]
  names(x) <- x
  x
}
filterGrpIDs <- function(grp_ids, meta){
  keep <- vapply(grp_ids, FUN=function(.ele){
    all(is.na(suppressWarnings(
      as.numeric(as.character(unique(meta[[.ele]]))))))
  }, FUN.VALUE = logical(1L))
  grp_ids[keep]
}

updateAppConf <- function(input, global){
  markers <- global()$markers
  if(is.character(markers)){
    markers <- markers[!is.na(markers)]
    markers <- markers[markers!=""]
    markers <- t(t(markers))
    rownames(markers) <- markers
    markers <- list(markers=as.data.frame(markers))
  }
  if(input$species2!="" && input$species2!="NA"){
    species <- input$species2
  }else{
    species <- input$species
  }
  appconf <- APPconf(title=input$title,
                     id=input$dir,
                     species=species,
                     ref=list(
                       bib=input$reference,
                       doi=input$doi,
                       pmid=input$pmid,
                       entry=global()$ref
                     ),
                     type=input$datatype,
                     markers = markers,
                     keywords = input$keywords)
  if(!is.null(input$dir)){
    if(input$dir!=""){
      saveAppConf(appconf)
    }
  }
}
updateDef <- function(input){
  sc1def <- list(
    meta1 = input$meta1,
    meta2 = input$meta2,
    gene1 = input$gene1,
    gene2 = input$gene2,
    genes = input$multigene,
    dimred = c(input$dimred1, input$dimred2),
    grp1 = input$grp1,
    grp2 = input$grp2
  )
  if(!is.null(input$dir)){
    if(input$dir!=""){
      saveData(sc1def, input$dir, "sc1def")
    }
  }
}
formatfID_CL <- function(x, rev=FALSE){
  if(rev){
    x[!is.na(x)] <- gsub(";\\s+", "|", x[!is.na(x)])
    return(x)
  }
  x[!is.na(x)] <- gsub("\\|", "; ", x[!is.na(x)])
  x
}
#' read expression matrix from sc1gexpr.h5
#' @noRd
#' @param h5filename Parent foldername of h5 file
#' @param rown,coln rownames and colnames for the expression file.
#' rownames = names(readRDS('sc1gene.rds'));
#' colnames = readRDS('sc1meta.rds')$sampleID
#' @importFrom hdf5r readDataSet
readDataMatrix <- function(h5filename, rown, coln){
  h5file <- H5File$new(file.path(.globals$datafolder,
                                 h5filename,
                                 .globals$filenames$sc1gexpr),
                       mode = "r")
  on.exit(h5file$close_all())
  h5data <- h5file[["grp"]][["data"]]
  expr <- readDataSet(h5data)
  h5file$close_all()
  on.exit()
  rownames(expr) <- rown
  colnames(expr) <- coln
  return(expr)
}

getReductionMethod <- function(reduction_method, forMonocole=TRUE){
  if("umap" %in% reduction_method){
    reduction_method <- "UMAP"
  }else{
    if("tsne" %in% reduction_method){
      reduction_method <- "tSNE"
    }else{
      if("pca" %in% reduction_method){
        reduction_method <- "PCA"
      }else{
        reduction_method <- "UMAP"
      }
    }
  }
  if(!forMonocole) reduction_method <- tolower(reduction_method)
  reduction_method
}
