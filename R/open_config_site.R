#' Open config_site for current tenant
#' @export
#' @importFrom rstudioapi navigateToFile
open_config_site <- function(){
  
  requireNamespace("rstudioapi")  
  
  cur_tenant <- get_tenant()
  
  pth <- glue::glue("config_site/{cur_tenant}/config_site.yml")
  if(file.exists(pth)){
    rstudioapi::navigateToFile(pth)  
  } else {
    message(paste("Could not find file:",pth))
  }
  
  
} 
 
