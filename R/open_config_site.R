#' Open config_site for current gemeente
#' @export
#' @importFrom rstudioapi navigateToFile
open_config_site <- function(){
  
  requireNamespace("rstudioapi")  
  
  cur_gemeente <- get_gemeente()
  
  pth <- glue::glue("config_site/{cur_gemeente}/config_site.yml")
  if(file.exists(pth)){
    rstudioapi::navigateToFile(pth)  
  } else {
    message(paste("Could not find file:",pth))
  }
  
  
} 
 
