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

#' Set current gemeente in this_version.yml
#' @description Overwrites current this_version.yml!
#' @export
#' @rdname utils
set_gemeente <- function(gemeente){
  yaml::write_yaml(list(gemeente = gemeente), "this_version.yml")
}

