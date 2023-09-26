#' Make code friendly version of tenant name
#' @export
#' @rdname utils
normalize_tenant_name <- function(tenant){
  label <- label_tenant(tenant)
  o <- tolower(label)
  o <- gsub("-","_",o)
  o <- gsub(" ","_",o)
  o
}

#' Make WBM appn ame based on tenant name
#' @description Corresponds to the application name on rsconnect
#' @export
#' @rdname utils
make_app_name <- function(tenant){
  paste0(normalize_tenant_name(tenant), "_wbm")
}

#' Find database name for the current client
#' @export
#' @rdname utils
#' @importFrom config get
get_current_db_name <- function(){
  client <- get_tenant()
  
  stopifnot(file.exists("conf/config.yml"))
  config::get(client, file = "conf/config.yml")$dbname
}

#' Set current tenant in this_version.yml
#' @description Overwrites current this_version.yml!
#' @param tenant Tenant (e.g. 'DEMO')
#' @param 
#' @export
#' @rdname utils
set_tenant <- function(tenant, path = getwd()){
  cli::cli_alert_success(paste("Juno - nieuwe tenant:", tenant))
  
  if(!shintodb::has_config_entry(tenant, where = "development")){
    cli::cli_alert_warning("Tenant does not have a database config entry! Add with shintodb::add_config_entry(...)")
  }
  
  if(!tenant %in% get_tenant_choices()){
    cli::cli_alert_warning("Tenant does not have an entry in tenant_list.yml - add it there or check your spelling!")
  }
  
  yaml::write_yaml(list(tenant = tenant), file.path(path, "this_version.yml"))
}

#' Set random tenant in this_version.yml
#' @description Travel somewhere!
#' @export
#' @rdname utils
set_random_tenant <- function(){
  tenant <- sample(get_tenant_choices(),1)
  set_tenant(tenant)
}



#' Get current tenant from this_version.yml
#' @export
#' @rdname utils
get_tenant <- function(){
  yaml::read_yaml("this_version.yml")$tenant
}

#' Get available tenants for the WBM
#' @export
get_tenant_choices <- function(path = getwd()){
  tl <- yaml::read_yaml("tenant_list.yml")
  names(tl)
  
}


