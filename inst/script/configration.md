# Configuring scRNAseqApp

This document describes how to configure the application using the configuration loading mechanism implemented in `loadConfigFile()`.

The function supports two configuration formats:

1. **DCF configuration file**: the preferred configuration method.
2. **RDS configuration file**: a legacy or fallback configuration method.

The configuration values are loaded into the `.globals` object and can override the application's default settings.

---

## 1. Possible Configuration Parameters

The following parameters can be configured in the application configuration file:

* **`welcomepage`**: The filename of the Markdown file displayed as the welcome or splash page.

* **`policy`**: The filename of the Markdown file containing the application's privacy policy.

* **`default_policy`**: A short default privacy policy statement displayed at the bottom of the home page.

* **`theme`**: The visual theme of the application.

* **`email`**: The email address of the application maintainer or administrator.

* **`figWidth`**: The default width of downloaded figures.

* **`figHeight`**: The default height of downloaded figures.

* **`figFormats`**: The default file formats available for figure downloads.

* **`maxHeatmapGene`**: The maximum number of genes that can be displayed in a heatmap.

* **`maxNumGene`**: The maximum number of genes that can be displayed in a dropdown selection menu.

* **`limitNumGene`**: The maximum number of genes allowed when performing a regular expression-based gene search. The default value is **3**.
  ⚠️ **Warning:** Do not set this value to a large number, as regular expression searches involving many genes may significantly affect application performance.

---

## 2. By DCF Configuration File

### DCF Configuration File Loading

The application first looks for a configuration file defined by:

```r
configFile <- file.path(app_path, scRNAseqApp:::.globals$filenames$config)
```

Therefore, the location and name of the configuration file are determined by:

* `app_path`: the root directory of the application.
* `.globals$filenames$config`: define the configuration filename.
The default configuration filename could not be changed.

For example, if:

```r
app_path <- "/path/to/application"
```

the application will look for:

```text
/path/to/application/config.dcf
```

If this file exists, it is loaded using:

```r
configs <- read.dcf(configFile)
```

---

### Using a DCF Configuration File

A **DCF file** is a **Debian Control File** format.
The DCF configuration file is the preferred method for defining application settings.

DCF files use a simple `key: value` format.

For example:

```text
figFormats: PDF, PNG, JPEG
maxHeatmapGene: 50
theme: bootswatch='lumen'
```

Only configuration names that are already defined in `.globals` will be loaded.

The function determines the valid configuration parameters using:

```r
cn <- intersect(colnames(configs), names(.globals))
```

This means that:

> **A configuration parameter in the DCF file must have the same name as an existing variable in `.globals`.**

Unknown parameters will be ignored.

---

### Setting Standard Configuration Parameters

For parameters that are not lists, values can be provided as comma-separated values.

For example:

```text
maxHeatmapGene: 50
```

The default maxHeatmapGene is 100. 
The configuration value will replace the default maxHeatmapGene to 

```r
.globals$maxHeatmapGene <- 50
```

And then the maximal gene number for heatmap plot will be limited to 50.

---

### Setting Multiple Values

Multiple values can be separated using commas.

For example:

```text
figFormats: PDF, PNG, JPEG
```

The default figFormats are 'PDF', 'PNG', 'TIFF', 'JPEG', 'BMP', 'CSV'. 
The configuration value will replace the default figFormats to 

```r
.globals$figFormats <- c('PDF', 'PNG', 'JPEG')
```

And then only PDF, PNG and JPEG can be downloaded from the APP.

---

### Configuring the Application Theme

The `theme` parameter is handled differently from other configuration options.

When the configuration parameter is named:

```text
theme
```

the value is used to create a Bootstrap theme using `bs_theme()`.

The code is:

```r
theme <- eval(
    parse(
        text = paste0(
            "bs_theme(",
            configs[1, i],
            ")"
        )
    )
)

.globals$theme <- theme
```

Therefore, the value in the DCF file should contain valid arguments for the `bs_theme()` function.

#### Example

```text
theme: bootswatch=flatly
```

This will be evaluated as:

```r
bs_theme(
    bootswatch = "flatly"
)
```

Another example:

```text
theme: version = 5, bg = "#FFFFFF", fg = "#333333"
```

This allows the application's Bootstrap appearance to be customized directly through the configuration file.

> ⚠️ The theme configuration must contain valid R arguments for `bs_theme()`. Invalid syntax may cause an error when the application starts.

---

## 3. Fallback RDS Configuration

If the DCF configuration file does not exist, the application will look for:

```text
config.rds
```

inside the application's database folder:

```r
configFile <- file.path(
    app_path,
    scRNAseqApp:::.globals$dbFolder,
    "config.rds"
)
```

For example, if:

```r
app_path <- "/path/to/application"
```

the application will look for:

```text
/path/to/application/db/config.rds
```

The configuration is loaded using:

```r
configs <- readRDS(configFile)
```

---

### Updating Non-List Parameters from RDS

For standard parameters, the value stored in the RDS file directly replaces the corresponding value in `.globals`.

```r
.globals[[i]] <- configs[[i]]
```

Only configuration names shared by both objects are loaded:

```r
cn <- intersect(
    names(configs),
    names(.globals)
)
```

Unknown configuration parameters are ignored.

---

### Updating Nested List Parameters from RDS

Unlike the DCF configuration method, the RDS configuration method supports nested lists.

This allows selected configuration values to be overridden while preserving unspecified default values.

This operation is not encouraged.

---

## 4. Configuration Loading Priority

The configuration loading process follows this order:

```text
┌─────────────────────────────┐
│ Default .globals settings   │
└──────────────┬──────────────┘
               │
               ▼
      Is the DCF file present?
               │
        ┌──────┴──────┐
       Yes            No
        │              │
        ▼              ▼
   Load DCF       Look for config.rds
        │              │
        ▼              ▼
 Override        Override settings
 non-list        including nested
 parameters      list parameters
```

In summary:

| Configuration file | Priority  | Supported parameters                |
| ------------------ | --------- | ----------------------------------- |
| DCF file           | Preferred | Non-list parameters and theme       |
| `config.rds`       | Fallback  | Standard and nested list parameters |

If the DCF configuration file exists, the RDS configuration file will not be loaded.

---

## 5. Recommended Configuration Workflow

For most applications, we recommend using the DCF configuration file for simple, user-editable settings.

### Recommended uses for DCF

* Application theme
* Display options
* Feature flags
* Species or dataset selections
* Other simple scalar or vector parameters

### Recommended uses for RDS

* Nested configuration structures
* Complex lists
* Internal application settings
* Programmatically generated configuration objects

---

## 6. Example Configuration

A typical DCF configuration file might look like:

```text
commentsIntervals: 3
figFormats: PDF, PNG, JPEG
maxHeatmapGene: 50
theme: bootswatch='lumen'
```

When the application starts, the configuration loader will:

1. Load the default values from `.globals`.
2. Check whether the DCF configuration file exists.
3. Read the DCF configuration.
4. Identify parameters that match names in `.globals`.
5. Convert values to the appropriate data type.
6. Apply the configuration values.
7. Return the updated `.globals` object.

---

## Summary

The configuration system provides a flexible mechanism for customizing application behavior without modifying the application source code.

> **Use the DCF configuration file for simple user-facing settings, and use `config.rds` as a fallback for complex or nested configuration structures.**

The system also ensures that only recognized configuration parameters are loaded, helping preserve the structure and default settings defined in `.globals`.
