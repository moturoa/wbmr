#' Make a 'deploy project' for the WBM
#' @export
#' @importFrom yaml read_yaml write_yaml
#' @importFrom shintoshiny make_deploy_project
deploy_project <- function(gemeente = NULL){

  # If 'gemeente' specified, ignore this_version,
  # and set this_version correctly in the deploy project
  if(is.null(gemeente)){
    gemeente <- yaml::read_yaml("this_version.yml")$gemeente
    if(is.null(gemeente)){
      stop("Specify 'gemeente' in this_version.yml")
    }
  } else {
    file.copy("this_version.yml", "backup_this_version.yml")
    on.exit({
      file.copy("backup_this_version.yml", "this_version.yml", overwrite = TRUE)
      file.remove("backup_this_version.yml")
    })
    set_gemeente(gemeente)
  }

  appname <- make_app_name(gemeente)

  dirs <- c("conf","modules","R","www","preload",
            "data_public/NL",
            file.path("data_public",gemeente),
            file.path("data",gemeente),
            file.path("config_site",gemeente))

  shintoshiny::make_deploy_project(appname, directories = dirs)

}

