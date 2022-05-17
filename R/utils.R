#' Make code friendly version of gemeente name
#' @export
#' @rdname utils
normalize_gemeente_name <- function(gemeente){
  o <- tolower(gemeente)
  o <- gsub("-","_",o)
  o <- gsub(" ","_",o)
  o
}

#' Make WBM appn ame based on gemeente name
#' @decription Corresponds to the application name on rsconnect
#' @export
#' @rdname utils
make_app_name <- function(gemeente){
  paste0(normalize_gemeente_name(gemeente), "_wbm")
}

#' Find database name for the current client
#' @export
#' @rdname utils
get_current_db_name <- function(){
  client <- get_gemeente()
  
  stopifnot(file.exists("conf/config.yml"))
  config::get(client, file = "conf/config.yml")$dbname
}

#' Set current gemeente in this_version.yml
#' @description Overwrites current this_version.yml!
#' @export
#' @rdname utils
set_gemeente <- function(gemeente){
  yaml::write_yaml(list(gemeente = gemeente), "this_version.yml")
}

#' Get current gemeente from this_version.yml
#' @export
#' @rdname utils
get_gemeente <- function(){
  yaml::read_yaml("this_version.yml")$gemeente
}

#' Get available gemeentes for the WBM
#' @export
get_gemeente_choices <- function(path = getwd()){
  fns <- list.dirs(file.path(path, "config_site"), recursive = FALSE)
  sort(basename(fns))
}


