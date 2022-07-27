library(magrittr)
library(shinyhelper)
sortLevels <- function(lev, controls=c("control", "wildtype", "wt", "cont", "contr", "vehicle", "sham")){
  l0 <- lev[tolower(lev) %in% controls]
  l1 <- lev[!tolower(lev) %in% controls]
  ## find the same part of the levels
  l1.sub <- strsplit(l1, split="")
  ml <- max(lengths(l1.sub))
  l1.sub <- lapply(l1.sub, function(.ele) c(.ele, rep("", ml))[seq.int(ml)])
  l1.sub <- do.call(rbind, l1.sub)
  l1.sub <- apply(l1.sub, 2, unique, simplify = FALSE)
  ml <- lengths(l1.sub)
  ml <- min(which(ml!=1))
  if(ml>0){
    l1.sub <- paste(unlist(l1.sub)[seq.int(ml-1)], collapse="")
    l1.sub0 <- sub(l1.sub, "", l1)
  }else{
    l1.sub0 <- l1
  }
  ## postfix
  l1.sub <- strsplit(l1.sub0, split="")
  l1.sub <- Reduce(intersect, l1.sub)
  l1.sub <- paste(l1.sub, collapse = "")
  l1.sub <- sub(l1.sub, "", l1.sub0)
  l1.sub <- suppressWarnings(as.numeric(l1.sub))
  if(all(!is.na(l1.sub))){
    l1 <- l1[order(l1.sub)]
  }
  c(l0, l1)
}
source("R/tabsc1a1.R")
source("R/tabsc1a2.R")
source("R/tabsc1a3.R")
source("R/tabsc1b2.R")
source("R/tabsc1c1.R")
source("R/tabsc1c2.R")
source("R/tabsc1d1.R")
source("R/tabsc1e1.R")
source("R/tabsc1f1.R")
