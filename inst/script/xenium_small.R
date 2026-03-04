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
Idents(allen.cortex.ref) <- 'subclass'
allen.cortex.ref <- NormalizeData(allen.cortex.ref, normalization.method = "LogNormalize", scale.factor = 10000)
allen.cortex.ref <- FindVariableFeatures(allen.cortex.ref, selection.method = "vst", nfeatures = 2000)
genes <- rownames(allen.cortex.ref)[!grepl(pattern = "^Rp[l|s]|Mt", x = rownames(allen.cortex.ref))]
allen.cortex.ref <- ScaleData(allen.cortex.ref, features = genes)
markers <- FindAllMarkers(allen.cortex.ref, logfc.threshold = 1, only.pos = TRUE, min.pct=0.3, return.thresh=0.05)
markers <- markers[markers$p_val_adj<0.05&abs(markers$avg_log2FC)>1, ]
dim(markers)
table(markers$cluster)
hvg <- VariableFeatures(allen.cortex.ref)

DefaultAssay(xenium.obj) <- 'Xenium'
xenium.obj <- NormalizeData(xenium.obj, normalization.method = "LogNormalize", scale.factor = 10000)

res <- SPOTlight(
    x = GetAssayData(allen.cortex.ref, assay = 'RNA', layer = 'data'),
    y = GetAssayData(xenium.obj, assay = 'Xenium', layer = 'data'),
    groups = as.character(allen.cortex.ref$subclass),
    mgs = markers,
    hvg = hvg,
    weight_id = "avg_log2FC",
    group_id = "cluster",
    gene_id = "gene")
head(res)
mat <- res$mat
colnames(mat) <- paste0('SPOTLight_', colnames(mat))
identical(rownames(res$mat), colnames(xenium.obj))
xenium.obj <- AddMetaData(xenium.obj, mat)

head(xenium.obj)

saveRDS(xenium.obj, '~/Downloads/Xenium_V1/tmp.rds')
write.csv(markers, '~/Downloads/Xenium_V1/markers.csv')
markers <- markers[markers$p_val_adj<0.005&abs(markers$avg_log2FC)>2, ]
markers <- split(markers$gene, markers$cluster)
markers <- lapply(markers, head, n=100)

qc <- function(mat) {
    # Indices of mitochondrial genes
    mito_idx <- grep("^MT-", rownames(mat))
    
    # Calculate QC metrics
    res <- mat |>
        scuttle::perCellQCMetrics(subsets = list("Mito" = mito_idx)) |>
        scuttle::perCellQCFilters(sub.fields = "subsets_Mito_percent")
    
    # Remove low quality cells
    mat[, !res$discard]
}
imputed_gex <- GetAssayData(xenium.obj, assay = "Xenium", layer = "counts") |>
    qc() |>
    scuttle::normalizeCounts() |>
    Seqtometry::impute()
future::plan("sequential")
# Allot 1 GiB of memory for global variables
options(future.globals.maxSize = 1024 ^ 3)
scores <- markers |>
    Seqtometry::score(imputed_gex, signatures = _)
colnames(scores) <- paste0('SeqtometryScore_', colnames(scores))
colnames(scores) <- make.names(colnames(scores), allow_ = TRUE)
xenium.obj <- AddMetaData(xenium.obj, metadata = scores[, -1])
markers <- lapply(markers, head, n=3)
markers <- unlist(markers)
markers <- unname(markers)
markers <- unique(markers)
DefaultAssay(xenium.obj) <- 'SCT'
appconf <- createAppConfig(
            title="xenium_small",
            destinationFolder = "xenium_small",
            species = "Homo sapiens",
            doi="10.1038/nbt.3192",
            datatype = "spatial",
            markers =markers[markers %in% rownames(xenium.obj)])
unlink('~/Downloads/Xenium_V1/xenium_small', recursive = TRUE)
createDataSet(appconf, seu = xenium.obj,
              datafolder = path, boundaries = 'segmentations')

