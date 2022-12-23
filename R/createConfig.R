#' Create a shinycell config data.table
#' This function was simplified from shinycell for package submission
#' @noRd
#' @param obj input single-cell object for Seurat (v3+)
#' @param meta.to.include columns to include from the single-cell metadata.
#'   Default is \code{NA}, which is to use all columns. Users can specify
#'   the columns to include, which must match one of the following:
#'   \itemize{
#'     \item{Seurat objects}: column names in \code{seu@meta.data}
#'       i.e. \code{colnames(seu@meta.data)}
#'   }
#' @param legendCols maximum number of columns allowed when displaying the
#'   legends of categorical metadata
#' @param maxLevels maximum number of levels allowed for categorical metadata.
#'   Metadata with nlevels > maxLevels will be discarded automatically
#'
#' @return config data.table
#' @importFrom SeuratObject Reductions
#' @importFrom data.table data.table
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
createConfig <- function(obj, meta.to.include = NA, legendCols = 4,
                         maxLevels = 50){
  # Extract corresponding metadata
  drExist = TRUE
  if(inherits(obj, "Seurat")){
    # Seurat Object
    objMeta <- obj[[]]
    if(length(Reductions(obj)) == 0){drExist <- FALSE}
  }  else {
    stop("Only Seurat object is accepted!")
  }
  if(!drExist){
    stop("Can not detect any dimension reduction data \n",
         "       e.g. umap / tsne. Has any analysis been performed?")
  }

  # Checks and get list of metadata to include
  if(is.na(meta.to.include[1])){meta.to.include <- colnames(objMeta)}
  if(length(meta.to.include) < 2){stop("At least 2 metadata is required!")}

  # Start making config data.table
  scConf <- data.table()
  for(iMeta in meta.to.include){
    tmpConf <- data.table(ID = iMeta, UI = iMeta, fID = NA, fUI = NA,
                          fCL = NA, fRow = NA, default = 0, grp = FALSE)

    # Convert to factors if metadata contains characters
    if(is.character(objMeta[[iMeta]])){
      objMeta[[iMeta]] <- factor(objMeta[[iMeta]])
    }

    # Additional preprocessing for categorical metadata
    nLevels <- nlevels(objMeta[[iMeta]])
    if(nLevels <= maxLevels){
      if(nLevels >= 2){
        tmpConf$fID <- paste0(levels(objMeta[[iMeta]]), collapse = "|")
        tmpConf$fUI <- tmpConf$fID
        tmpConf$fCL <-
          paste0(colorRampPalette(brewer.pal(12, "Paired"))(nLevels),
                 collapse = "|")
        tmpConf$fRow <- ceiling(nLevels / legendCols)
        tmpConf$grp <- TRUE
      } else if(nLevels == 1){
        tmpConf$fID <- levels(objMeta[[iMeta]])
        tmpConf$fUI <- tmpConf$fID
        tmpConf$fCL <- "black"
        tmpConf$fRow <- 1
      }
      scConf <- rbindlist(list(scConf, tmpConf))
    }
  }

  # Set defaults
  def1 <- grep("ident|library", scConf$ID, ignore.case = TRUE)[1]
  def2 <- grep("clust", scConf$ID, ignore.case = TRUE)
  def2 <- setdiff(def2, def1)[1]
  if(is.na(def1)){def1 <- setdiff(c(1,2), def2)[1]}
  if(is.na(def2)){def2 <- setdiff(c(1,2), def1)[1]}
  scConf[def1]$default <- 1
  scConf[def2]$default <- 2

  # STOP if there is no single multi-level covariate
  if(nrow(scConf[scConf$grp == TRUE]) == 0){
    stop("Can not detect any multi-group cell metadata.")
  }

  return(scConf)
}

