library(Seurat)
library(future)
plan("multisession", workers = 4)
library(ggplot2)
library(scRNAseqApp)
library(spacexr)
library(SPOTlight)
library(scran)

options(future.globals.maxSize = 10 * 1024^3)
# curl -O https://cf.10xgenomics.com/samples/xenium/1.0.2/Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP/Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP_outs.zip
# unzip Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP_outs.zip
path <- '~/Downloads/Xenium_V1'
xenium.obj <- LoadXenium(path, fov = "fov", segmentations = "cell")
xenium.obj <- subset(xenium.obj, subset = nCount_Xenium > 0)
xenium.obj <- SCTransform(xenium.obj, assay = "Xenium")
xenium.obj <- RunPCA(xenium.obj, npcs = 30, features = rownames(xenium.obj))
xenium.obj <- RunUMAP(xenium.obj, dims = 1:30)
xenium.obj <- FindNeighbors(xenium.obj, reduction = "pca", dims = 1:30)
xenium.obj <- FindClusters(xenium.obj, resolution = 0.3)
# curl -O https://www.dropbox.com/s/cuowvm4vrf65pvq/allen_cortex.rds?dl=1
allen.cortex.ref <- readRDS("~/Downloads/Xenium_V1/allen_cortex.rds")
allen.cortex.ref <- UpdateSeuratObject(allen.cortex.ref)

query.counts <- GetAssayData(xenium.obj, assay = "Xenium", layer = "counts")
coords <- GetTissueCoordinates(xenium.obj$fov, which = "centroids")
rownames(coords) <- coords$cell
coords$cell <- NULL
query <- SpatialRNA(coords, query.counts, colSums(query.counts))
Idents(allen.cortex.ref) <- "subclass"
# remove CR cells because there aren't enough of them for annotation
allen.cortex.ref <- subset(allen.cortex.ref, subset = subclass != "CR")
counts <- GetAssayData(allen.cortex.ref, assay = "RNA", slot = "counts")
cluster <- as.factor(allen.cortex.ref$subclass)
names(cluster) <- colnames(allen.cortex.ref)
nUMI <- allen.cortex.ref$nCount_RNA
names(nUMI) <- colnames(allen.cortex.ref)
nUMI <- colSums(counts)
levels(cluster) <- gsub("/", "-", levels(cluster))
reference <- Reference(counts, cluster, nUMI)
# run RCTD with many cores
RCTD <- create.RCTD(query, reference, max_cores = 8)
RCTD <- run.RCTD(RCTD, doublet_mode = "doublet")
annotations.df <- RCTD@results$results_df
annotations <- annotations.df$first_type
names(annotations) <- rownames(annotations.df)
xenium.obj$predicted.celltype <- annotations

# SPOTLight
genes <- !grepl(pattern = "^Rp[l|s]|Mt", x = rownames(allen.cortex.ref))
Idents(allen.cortex.ref) <- 'subclass'
markers <- FindAllMarkers(allen.cortex.ref)
res <- SPOTlight(
    x = allen.cortex.ref,
    y = xenium.obj,
    groups = as.character(allen.cortex.ref$subclass),
    mgs = mgs_df,
    hvg = hvg,
    weight_id = "mean.AUC",
    group_id = "cluster",
    gene_id = "gene")

appconf <- createAppConfig(
            title="xenium_small",
            destinationFolder = "xenium_small",
            species = "Homo sapiens",
            doi="10.1038/nbt.3192",
            datatype = "spatial",
            markers =rownames(xenium.obj)[1:5])
unlink('~/Downloads/Xenium_V1/xenium_small', recursive = TRUE)
createDataSet(appconf, seu = xenium.obj,
              datafolder = path, boundaries = 'segmentations')
