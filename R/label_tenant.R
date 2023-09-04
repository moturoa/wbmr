
#' Label a tenant based on the code
#' @param tenant Tenant (e.g. 'DEMO')
#' @export
label_tenant <- function(tenant){
  
  key <- yaml::read_yaml("tenant_list.yml")
  unlist(unname(key[match(toupper(tenant),toupper(names(key)))]))
  
}
