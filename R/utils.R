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
#' @export
#' @rdname utils
set_tenant <- function(tenant){
  message(paste("Juno - new tenant:", tenant))
  yaml::write_yaml(list(tenant = tenant), "this_version.yml")
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
  fns <- list.dirs(file.path(path, "config_site"), recursive = FALSE)
  sort(basename(fns))
}


