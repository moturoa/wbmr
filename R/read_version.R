#' Read the application version
#' @export
read_version <- function(){
  fn <- "VERSION"
  if(!file.exists(fn)){
    ""
  } else {
    readLines(fn)[1]
  }
  
}


#' Read application info
#' @export
read_application_info <- function(){
  
  info <- shintoshiny::read_application_info()
  info$this_version$appversion <- read_version()
  
info
}
