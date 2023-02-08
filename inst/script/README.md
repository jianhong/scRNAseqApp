# scripts to generate the data in extdata folder

## gene to symbol list: gn2sym.rds

The `gn2sym.rds` is a list with 2 elements: unique and multiple.
The unique element is a character vector of gene symbols with gene name as its
name.
The multiple is a list of character vectors with gene name as the name of the
list and gene symbols as elements for each gene name.
All the gene name and gene symbol were converted to lower case.
The script `gn2sym.R` is used to generate the `gn2sym` list.
The list were generated from packages:
"org.Ce.eg.db", "org.Cf.eg.db", "org.Dm.eg.db", "org.Dr.eg.db",
"org.Gg.eg.db", "org.Hs.eg.db", "org.Mm.eg.db", "org.Mmu.eg.db",
"org.Pt.eg.db", "org.Rn.eg.db", "org.Ss.eg.db", and "org.Xl.eg.db".
The data were created by `gn2sym.R` at the data Dec 23, 2022.

## cell cycle gene list: cc.genes.list.rds

The `cc.genes.list.rds` is a list of genes involved in cell cycles.
The first level of the list is the organisms which include 
Bos taurus, Caenorhabditis elegans, Drosophila melanogaster, Danio rerio,
Gallus gallus, Mus musculus, Macaca mulatta, Pan troglodytes,
Rattus norvegicus, Saccharomyces cerevisiae, Sus scrofa, and Homo sapiens.
The second level of the list is a list with names 'ensembl' and 'symbol'.
The elements in 'ensembl' is the ensembl ids and that in 'symbol' is the
gene symbols.
The third level of the list is list with names 's.genes' and 'g2m.genes'
with the gene ids related to 'S' and 'g2m' phase.
All the ids were converted from cell cycle genes (2019 update) of
Seurat package by biomart server of ensembl.org.
The data were created by `cell_cycle_gene_list.R` at the date Dec 23, 2022.

## sample data: pbmc_small

The `pbmc_samll` is the toy data to demo the APP. It was created by the script
`pbmc_small.R`.