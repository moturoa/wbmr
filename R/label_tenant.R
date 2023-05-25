
#' Label a tenant based on the code
#' @export
label_tenant <- function(tenant){
  
  key <- yaml::read_yaml("tenant_list.yml")
  key[[tenant]]
  
}
