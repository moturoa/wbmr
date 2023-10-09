
#' Vectorized and safe version of deploy_now
#' @description Also checks the db config file and prompts for missing passwords
#' @param tenants vector of tenants to deploy
#' @param where Refers to config section (development/production/)
#' @param servers 
#' @export
deploy_all <- function(tenants, 
                       where = "development",
                       servers = c(development = "devapp.shintolabs.net", 
                                   production = "app.shintolabs.net",
                                   eindhoven_premium = "eindhoven.shintolabs.net")  
){
  
  
  if(!where %in% names(servers)){
    stop("'where' should refer to a server in servers argument")
  }
  
  server_where <- servers[match(where, names(servers))]
  
  prepare_db_config(tenants, where = where)
  
  tenant_list <- yaml::read_yaml("tenant_list.yml")
  
  for(tenant in tenants){
    cli::cli_h1("Deploying {tenant}")
    tm <- try({
      deploy_now(tenant, where = server_where)  
    })
    if(inherits(tm, "try-error")){
      cli::cli_alert_danger("Deploy for {tenant} not successful!")
    }
    
  }
  
}
