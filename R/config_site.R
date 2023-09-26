
#' Search all config_site's for all tenants
#' @export
find_cc <- function(txt){
  
  tens <- get_tenant_choices()
  
  ccs <- lapply(tens, function(tenant){
    configurationObject$new(tenant = tenant)
  }) %>% setNames(tens)
  
  lapply(ccs, function(.cc){
    .cc$get(txt)
  })
  
}

