#' Create a scRNAseqApp project
#' @description To run scRNAseqApp, you need to first create a directory
#' which contains the required files.
#' @param app_path path, a directory where do you want to create the app
#' @param root character(1), the user name for administrator
#' @param password character(1), the password for administrator
#' @param datafolder the folder where saved the dataset for the app
#' @param overwrite logical(1), overwrite the `app_path` if there is a project.
#' @return no returns. This function will copy files to `app_path`
#' @export
#' @importFrom shinymanager create_db
#' @importFrom scrypt hashPassword
#' @examples
#' if(interactive()){
#'   scInit()
#' }

scInit <- function(app_path=getwd(),
                   root='admin',
                   password='scRNAseqApp',
                   datafolder='data',
                   overwrite = FALSE){
  stopifnot(is.logical(overwrite) && length(overwrite) == 1)
  .globals$datafolder <- datafolder
  if(!dir.exists(app_path)){
    dir.create(app_path, recursive = TRUE)
  }
  message("Now copy files")
  for(f in c("www", "doc.txt", "data")){
    to <- ifelse(f=="data", datafolder, f)
    file.copy(system.file("extdata", f, package = "scRNAseqApp"),
              to=app_path, recursive=f!="doc.txt",
              overwrite = overwrite)
  }
  # define credentials
  credentials <- data.frame(
    user = root,
    password = hashPassword(password),
    admin = TRUE,
    is_hashed_password = TRUE,
    privilege = 'all',
    stringsAsFactors = FALSE)
  # Init the database
  create_db(
    credentials_data = credentials,
    sqlite_path = file.path(app_path, "database.sqlite")
  )
  return(invisible())
}
