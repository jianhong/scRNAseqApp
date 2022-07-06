my_password <- list()
pwdfilename <- "passwd"
for(d in datasets){
  if(file.exists(file.path(datafolder, d, pwdfilename))){
    my_password[[d]] <- readRDS(file.path(datafolder, d, pwdfilename))
  }
}
checkLockedDataset <- function(datasetname){
  datasetname %in% names(my_password)
}
checkToken <- function(tokenList, token, dataset){
  if(token %in% names(tokenList)){
    return(tokenList[[token]]==dataset)
  }else{
    return(FALSE)
  }
}
checkUserNameAndPassword <- function(username, password, datasetname){
  if(checkLockedDataset(datasetname)){
    pwd <- my_password[[datasetname]]
    return(username==pwd$username && password==pwd$password)
  }else{
    return(FALSE)
  }
}
createPassword <- function(username, password, datasetname,
                           pwdfilename="passwd",
                           datafolder="data"){
  x <- list(username=username, password=password)
  saveRDS(x, file = file.path(datafolder, datasetname, pwdfilename))
}
printPassword <- function(){
  out <- ""
  for(i in seq_along(my_password)){
    out <- c(out, paste("data", names(my_password)[i],
                        "uid", my_password[[i]]$username,
                        "pwd", my_password[[i]]$password))
  }
  paste(out, collapse=",")
}
