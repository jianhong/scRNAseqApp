#' Available themes of color sets
#' @description
#' The color themes inspired by modern web design palettes
#' @param output The theme names or 'name' to print all available themes.
#' @export
#' @importFrom RColorBrewer brewer.pal.info
#' @return A palette.
#' @examples
#' availableThemes()
#' 
availableThemes <- function(output='name'){
    availableColors <- list(
        "White-Red" = c(
            "grey85", "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84",
            "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"
        ),
        "Blue-Yellow-Red" = c(
            "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8",
            "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027"
        )[c(1, seq.int(9), 9)],
        "Yellow-Green-Purple" = c(
            "#FDE725", "#AADC32", "#5DC863", "#27AD81", "#21908C", 
            "#2C728E", "#3B528B", "#472D7B", "#440154"
        ),
        "Blue-DarkOrange" = c(
            "#1E8E99", "#51C3CC", "#99F9FF", "#B2FCFF", "#CCFEFF",
            "#E5FFFF", "#FFE5CC", "#FFCA99", "#FFAD65", "#FF8E32",
            "#CC5800", "#993F00"
        ),
        "Green-White-Magenta" = c(
            "#005000", "#008600", "#00BB00", "#00F100", "#50FF50",
            "#86FF86", "#BBFFBB", "#FFFFFF", "#FFF1FF", "#FFBBFF",
            "#FF86FF", "#FF50FF", "#F100F1", "#BB00BB", "#860086", "#500050"
        ),
        "Viridis" = c(
            '#440154FF', '#472D7BFF', '#3B528BFF', '#2C728EFF', '#21908CFF',
            '#27AD81FF', '#5DC863FF', '#AADC32FF', '#FDE725FF'
        ),
        # 1. Modern Vibrant (Material-Inspired)
        modern_vibrant = c(
            "#6200ea", "#03dac6", "#ff0266", "#3700b3", "#018786",
            "#bb86fc", "#03a9f4", "#ff9800", "#4caf50", "#f44336",
            "#7c4dff", "#00bcd4", "#ff5722", "#8bc34a", "#e91e63",
            "#2196f3", "#cddc39", "#009688", "#ffc107", "#9c27b0"
        ),
        
        # 2. Minimal Neutral (Tailwind Slate)
        minimal_neutral = c(
            "#0f172a", "#1e293b", "#334155", "#475569", "#64748b",
            "#94a3b8", "#cbd5e1", "#e2e8f0", "#f1f5f9", "#f8fafc",
            "#1e293b", "#374151", "#4b5563", "#6b7280", "#9ca3af",
            "#d1d5db", "#e5e7eb", "#f3f4f6", "#f9fafb", "#ffffff"
        ),
        
        # 3. Soft Pastel (Flat UI Inspired)
        soft_pastel = c(
            "#6c5ce7", "#00cec9", "#ffeaa7", "#fab1a0", "#55efc4",
            "#dfe6e9", "#ff7675", "#74b9ff", "#a29bfe", "#fd79a8",
            "#81ecec", "#fdcb6e", "#e17055", "#00b894", "#0984e3",
            "#b2bec3", "#636e72", "#fd5555", "#f78fb3", "#9b59b6"
        ),
        
        # 4. Warm Earthy (Color Hunt Style)
        warm_earthy = c(
            "#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51",
            "#1d3557", "#457b9d", "#a8dadc", "#f1faee", "#e63946",
            "#6d6875", "#b5838d", "#e5989b", "#ffb4a2", "#ffcdb2",
            "#cb997e", "#ddbea9", "#ffe8d6", "#b7b7a4", "#a5a58d"
        ),
        
        # 5. Bright Playful (Dribbble/Startup Aesthetic)
        bright_playful = c(
            "#ff6f61", "#6b5b95", "#88b04b", "#f7cac9", "#92a8d1",
            "#f7786b", "#34ace0", "#ffb142", "#33d9b2", "#706fd3",
            "#f19066", "#227093", "#218c74", "#f5cd79", "#ff793f",
            "#3dc1d3", "#cf6a87", "#574b90", "#786fa6", "#f8a5c2"
        ),
        
        # 6. Cool Oceanic
        cool_oceanic = c(
            "#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087",
            "#f95d6a", "#ff7c43", "#ffa600", "#00aaff", "#00d2d3",
            "#00b894", "#55efc4", "#74b9ff", "#0984e3", "#6c5ce7",
            "#00cec9", "#01a3a4", "#2e86de", "#8395a7", "#c8d6e5"
        ),
        
        # 7. Sunset Glow
        sunset_glow = c(
            "#ff9f1c", "#ff7f50", "#ff6f61", "#f25f5c", "#d1495b",
            "#ef476f", "#f7b267", "#f7c59f", "#f6bd60", "#e36414",
            "#9a031e", "#fb5607", "#ffb703", "#ffcb77", "#fcd5ce",
            "#f8ad9d", "#e5989b", "#b5838d", "#6d6875", "#2b2d42"
        ),
        
        # 8. Neon Cyberpunk
        neon_cyberpunk = c(
            "#ff00ff", "#00ffff", "#ff1493", "#00ff9f", "#00bfff",
            "#39ff14", "#ff4500", "#ffea00", "#8a2be2", "#ff6ec7",
            "#ff4b5c", "#7d5fff", "#3ae374", "#32ff7e", "#7efff5",
            "#18dcff", "#0abde3", "#5f27cd", "#341f97", "#222f3e"
        ),
        
        # 9. Nature Forest
        nature_forest = c(
            "#004b23", "#006400", "#228b22", "#2e8b57", "#556b2f",
            "#6b8e23", "#8f9779", "#a3b18a", "#cfe1b9", "#e9f5db",
            "#1b4332", "#2d6a4f", "#40916c", "#52b788", "#74c69d",
            "#95d5b2", "#b7e4c7", "#d8f3dc", "#ccd5ae", "#e9f5db"
        ),
        
        # 10. Monochrome Blues
        monochrome_blues = c(
            "#001f3f", "#003366", "#004080", "#00509e", "#0066cc",
            "#0077e6", "#1a8cff", "#3399ff", "#4da6ff", "#66b3ff",
            "#80bfff", "#99ccff", "#b3d9ff", "#cce6ff", "#e6f2ff",
            "#99b3cc", "#668fb3", "#336699", "#264d73", "#19334d"
        )
    )
    
    if(output %in% c("name", "namestring")){
        N <- c(names(availableColors), rownames(brewer.pal.info))
        if(output == "namestring"){
            return(paste(N, collapse = ", "))
        }else{
            return(N)
        }
    }
    
    if(output == "sequence"){
        return(c(names(availableColors)[seq.int(6)],
                 rownames(brewer.pal.info[brewer.pal.info$category=="seq", ])))
    }
    
    if(output %in% rownames(brewer.pal.info)){
        return(brewer.pal(brewer.pal.info[output, "maxcolors"], output))
    }
    
    if(output %in% names(availableColors)){
        return(availableColors[[output]])
    }
    
    return(brewer.pal(12, "Paired"))
}

#' @importFrom grDevices colorRampPalette
scColorRampPalette <- function(nLevels, theme){
    colorRampPalette(availableThemes(theme))(nLevels)
}
