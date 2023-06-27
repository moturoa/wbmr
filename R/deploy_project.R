#' Make a 'deploy project' for the WBM
#' @export
#' @importFrom yaml read_yaml write_yaml
#' @importFrom shintoshiny make_deploy_project
#' @rdname deploy
deploy_project <- function(tenant = NULL, test = FALSE){

  # If 'tenant' specified, ignore this_version,
  # and set this_version correctly in the deploy project
  if(is.null(tenant)){
    tenant <- get_tenant()
    if(is.null(tenant)){
      stop("Specify 'tenant' in this_version.yml")
    }
  } else {
    file.copy("this_version.yml", "backup_this_version.yml")
    on.exit({
      file.copy("backup_this_version.yml", "this_version.yml", overwrite = TRUE)
      file.remove("backup_this_version.yml")
    })
    set_tenant(tenant)
  }

  appname <- make_app_name(tenant)
  
  if(test)appname <- paste0(appname, "_test")

  dirs <- c("conf","modules","R","www","preload",
            "data_public/NL",
            file.path("data_public",tenant),
            file.path("data",tenant),
            file.path("config_site",tenant))

  extra <- c("config_site/help.yml", "data_public/osm_icon_key.csv")
  if(!all(file.exists(extra)))extra <- NULL
  
  shintoshiny::make_deploy_project(appname, directories = dirs,
                                   extra_files = extra)

}


#' @rdname deploy
#' @export
deploy_test_project <- function(...){
  deploy_project(..., test = TRUE)
}
