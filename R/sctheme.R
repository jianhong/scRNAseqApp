# Plot theme
#' @importFrom ggplot2 theme element_text element_rect element_line
#' element_blank
sctheme <-
    function(
        base_size = 24,
        family = "Helvetica",
        XYval = TRUE,
        Xang = 0,
        XjusH = 0.5) {
        oupTheme <- theme(
            text = element_text(size = base_size, family = family),
            panel.background = element_rect(fill = "white", colour = NA),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black", size = base_size / 20),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(size = base_size, family = family),
            axis.text.x = element_text(angle = Xang, hjust = XjusH),
            legend.position = "bottom",
            legend.key = element_rect(colour = NA, fill = NA)
        )
        if (!XYval) {
            oupTheme <- oupTheme + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            )
        }
        return(oupTheme)
    }
