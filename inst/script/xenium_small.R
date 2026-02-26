library(Seurat)
library(future)
plan("multisession", workers = 4)
library(ggplot2)
library(scRNAseqApp)

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

pkgload::load_all()

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
