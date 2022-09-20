datafolder <- "data"
defaultDataset <- "GSM5023610_glial_app"

datasets <- c("GSM5023610_5dpi_snSeq_glial_cells_KlattShaw_Saraswathy_Zhou_etal_2021"="GSM5023610_glial_app",
              "Leslie_scRNAseq_brain"="Leslie_scRNAseq")

refs_authors <- c(
  "GSM5023610"="Shaw, Dana Klatt and Saraswathy, Vishnu Muraleedharan and Zhou, Lili and McAdow, Anthony R and Burris, Brooke and Butka, Emily and Morris, Samantha A and Dietmann, Sabine and Mokalled, Mayssa H "
)
refs_titles <- c(
  "GSM5023610"="Localized EMT reprograms glial progenitors to promote spinal cord repair "
)
refs_journals <- c("GSM5023610"="Dev Cell ")
refs_years <- c("GSM5023610"="(2021) ")
refs_pmids <- c("GSM5023610"="33609461")
data_types <- c("GSM5023610"="scRNAseq")

## datasets is a named vector with data folder names
## refs_xxxx is names vector of reference informations.
#datasets <- c("GSE150871_spinal_cord_YiLi_etal_2020"="GSE150871_microglia")
#refs_authors <- c("GSE150871"="Li Y, He X, Kawaguchi R, Zhang Y, Wang Q, Monavarfeshani A, Yang Z, Chen B, Shi Z, Meng H, Zhou S, Zhu J, Jacobi A, Swarup V, Popovich PG, Geschwind DH, He Z.")
#refs_titles <- c("GSE150871"="Microglia-organized scar-free spinal cord repair in neonatal mice")
#refs_journals <- c("GSE150871"="Nature")
#refs_years <- c("GSE150871"="(2020) ")
#refs_pmids <- c("GSE150871"="33029008")

## the datalist file is described in data.R.
# source(file.path(datafolder, "datalist.R"))
# if(file.exists(file.path(datafolder, "token.R"))){
#   source(file.path(datafolder, "token.R"))
# }
#' @include token.R
if(!defaultDataset %in% datasets){
  defaultDataset <- datasets[1]
}
