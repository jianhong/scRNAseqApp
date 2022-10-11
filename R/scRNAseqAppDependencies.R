scRNAseqAppDependencies <- function() {
  htmlDependency(name = "scRNAseqApp-assets", version = "0.0.1",
                 package = "scRNAseqApp",
                 src = "assets",
                 script = "js/login.js",
                 stylesheet = c()
  )
}
