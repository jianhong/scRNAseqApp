# scRNAseqApp

**scRNAseqApp** is a Bioconductor Shiny application for interactive exploration and visualization of single‑cell RNA‑seq (scRNA‑seq), scATAC-seq, multiomics or spatial data. It enables researchers to publish, explore, and share single‑cell datasets through a web‑based interface with rich plotting and filtering functionality.

---

## Key Features

* Interactive visualization of single‑cell datasets
* Support for multiple datasets within a single app
* Common single‑cell plots:

  * UMAP / t‑SNE
  * Violin plots
  * Dot plots
  * Heatmaps
* Metadata‑based filtering and grouping
* Downloadable figures and tables
* User authentication and management (optional)
* Suitable for local use or deployment on Shiny Server

---

## Installation

Install from Bioconductor:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("scRNAseqApp")
```

---

## Quick Start

### 1. Initialize an App Directory

Create and initialize an application folder:

```r
publish_folder <- tempdir()
scInit(app_path = publish_folder)
```

This sets up the required directory structure, database, and configuration files.
A typical app directory looks like:

```
MyApp/
├── data/                  # Stored datasets
├── db/
│   ├── database.sqlite    # User/app database
│   ├── counter.tsv        # Usage tracking
|── www/
│   └── ...                # Static app assets
└── app.R
```

Note: When deploying, ensure the `db/` directory and database files are writable by the Shiny server user.

---

### 2. Launch the App

```r
scRNAseqApp(app_path = publish_folder)
```

The app will open in your default web browser.

---

## Adding Data

scRNAseqApp works primarily with **Seurat objects**.

---

### Option A: Add Data Programmatically (Recommended for Deployment)

```r
appconf <- createAppConfig(
    title = "My scRNA‑seq App",
    destinationFolder = "MyApp",
    species = "Homo sapiens",
    doi = "10.XXXX/your_doi",
    datatype = "scRNAseq",
    abstract = "Description of the dataset"
)

createDataSet(
    appconf,
    seurat_object,
    datafolder = file.path(publish_folder, "data")
)
```

This method is reproducible and recommended for shared or public apps.

---

### Option B: Upload Through the App (Admin Mode)

1. Open the app
2. Switch user to **administrator**
3. Navigate to the **Upload Data** tab
4. Upload a Seurat object

This method will use local computational resources, not suggested for large dataset.

---

## Deployment on Shiny Server

1. Install **scRNAseqApp** on the server.
2. Initialize the app directory using `scInit()`.
3. Set the correct permissions for:

   * `db/database.sqlite`
   * `db/counter.tsv`
   * Other files under `db/` if user management is enabled.
4. Add data by copying the `destinationFolder` (e.g., `MyApp` in the example above) into the `data/` folder on the server.  
   If the newly copied data are not available to public users, first try reloading the app,  
   then check the accessibility and permissions of the folder.
5. Removing the destination folder will result in deletion of the dataset.

The app can then be served like any standard Shiny application

---

## Notes

* Designed for **exploration and sharing**, not for heavy upstream computation
* Preprocessing (normalization, clustering, dimensionality reduction) should be done **before** loading data into the app
* Best used with well‑annotated metadata

---

For full details, see the official Bioconductor vignette:

```
vignette("scRNAseqApp")
```

---

### Video Tutorials
Comprehensive video tutorials demonstrating the usage of **scRNAseqApp** are available on the following YouTube playlist:  
https://youtube.com/playlist?list=PL0uThf-sipbnepUDzVmAHpNWUy65F4HOm


## Contributing

Contributions, suggestions, and bug reports are welcome!
Please open an issue or submit a pull request on [GitHub](https://github.com/jianhong/scRNAseqApp/issues).
