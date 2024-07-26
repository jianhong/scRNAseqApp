#' @noRd
#' @importFrom shinyhelper helper
helper1 <- function(...,
                    category,
                    title = NULL,
                    content = NULL) {
    if (missing(category))
        category <- "geneName"
    if (is.null(title)) {
        titles <- c(
            "geneName" = "Gene information to colour cells by",
            "cellInfo" = "Cell information to colour cells by",
            "cellInfoX" = "Cell information to group cells by",
            "cellInfoY" = "Cell Info / Gene to plot",
            "subsetCellInfo" = "Cell information to subset by"
        )
        title <- titles[category]
    }
    if (is.null(content)) {
        contents <-
            list(
                "geneName" = c(
                    paste(
                        "Select gene to colour cells by gene",
                        "expression/accessibility"),
                    paste(
                        "- Gene expression/accessibility are coloured in a",
                        "White-Red colour scheme which can be",
                        "changed in the plot controls"
                    ),
                    paste(
                        "- Gene name support autocomplete.",
                        "Try to input gene name in the input box."
                    )
                ),
                "cellInfo" = c(
                    "Select cell information to colour cells",
                    "- Categorical covariates have a fixed colour palette",
                    paste(
                        "- Continuous covariates are coloured in a",
                        "Blue-Yellow-Red colour scheme, which can be",
                        "changed in the plot controls"
                    )
                ),
                "cellInfoX" = c(
                    "Select categorical cell information to group cells by",
                    "- Single cells are grouped by this categorical covariate",
                    paste(
                        "- Plotted as the X-axis of the violin plot /",
                        "box plot / bar plot")
                ),
                "cellInfoY" = c(
                    "Select cell info to plot for group"
                ),
                "subsetCellInfo" = c(
                    "Select categorical cell information to subset cells by",
                    "- cells are shown in different subsets"
                )
            )
        
        content <- contents[[category]]
    }
    shinyhelper::helper(
        ...,
        type = "inline",
        size = "m",
        fade = TRUE,
        title = title,
        content = content
    )
}
