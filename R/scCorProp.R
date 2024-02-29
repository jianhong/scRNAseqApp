# Plot heatmap for proportion
#' @importFrom ggplot2 ggplot aes .data scale_color_gradientn ylab xlab geom_tile geom_text
#' scale_fill_manual theme
#' @importFrom data.table data.table melt
#' @importFrom stats prcomp
scCorProp <- function(
        proportions,
        orderX,
        method = c("pearson", "kendall", "spearman"),
        type=c('heatmap', 'PCA'),
        labelsFontsize='Medium',
        inpcols='White-Red'
        ) {
    type <- match.arg(type)
    method <- match.arg(method)
    ggOut <- switch(type,
                    PCA={
                        pca <- prcomp(proportions)
                        pca <- data.table(pca$rotation,
                                          keep.rownames = TRUE)
                        ggplot(pca, aes(.data[['PC1']],
                                        .data[['PC2']],
                                        label = .data[['rn']])) +
                            geom_point() +
                            geom_text(hjust=0, vjust=1) +
                            sctheme(
                                base_size = .globals$sList[labelsFontsize])
                    },
                    heatmap={
                        cor <- cor(proportions, method = method)
                        cor <- melt(data.table(cor, keep.rownames = TRUE),
                                    id.vars = c("rn"))
                        colnames(cor) <- c('Var1', 'Var2', 'value')
                        if(!is.null(orderX)){
                            cor$Var1 <- factor(as.character(cor$Var1),
                                               levels = orderX)
                            cor$Var2 <- factor(as.character(cor$Var2),
                                               levels = orderX)
                        }else{
                            cor$Var1 <- factor(as.character(cor$Var1),
                                               levels = levels(cor$Var2))
                        }
                        ggplot(cor, aes(.data[["Var1"]], .data[["Var2"]],
                                        fill = .data[["value"]],
                                        label = round(.data[["value"]], 3))) +
                            geom_tile() +
                            geom_text() +
                            xlab("") + ylab("") +
                            sctheme(
                                base_size = .globals$sList[labelsFontsize],
                                Xang = 45,
                                XjusH = 1) +
                            scale_fill_gradientn(
                                "",
                                limits = c(ifelse(any(cor$value<0), -1, 0), 1),
                                colours = .globals$cList[[inpcols]]) +
                            theme(legend.position = "right")
                    })
    return(ggOut)
}
