# copied internal functions from https://github.com/datastorm-open/shinymanager
shinymanager_where <- function(where) {
  tags$div(
    style = "display: none;",
    selectInput(inputId = "shinymanager_where", label = NULL,
                choices = where, selected = where, multiple = FALSE)
  )
}
shinymanager_language <- function(lan) {
  tags$div(
    style = "display: none;",
    selectInput(inputId = "shinymanager_language", label = NULL,
                choices = lan, selected = lan, multiple = FALSE)
  )
}
