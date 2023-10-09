
#' Deploy Juno directly
#' @description Does not make a 'deploy project' but deploys Juno for a tenant immediately,
#' from a temporary directory
#' @param tenant Tenant name (e.g. 'DEMO')
#' @param appname Name of app on posit connect - if NULL (the default), read from tenant_list.yml
#' @param where The server to deploy to (devapp or app)
#' @param appid Optional: the app ID (looks like a uuid in the url at posit connect). Needed if you
#' are a collaborator on an app and want to update it (the original uploader does not need the ID)
#' @param appid_from_tenant_list If TRUE (the default) and appid is NULL (also default), reads appid from the tenant
#' list if the deployment is to the production environment
#' @param db_config_file Standard location of the DB config file
#' @param log_deployment If TRUE, attempts to log the deployment in DB 
#' @param launch_browser If TRUE, opens a browser with the app after deployment
#' @param deploy_location App will be deployed from a temp directory, or set a directory here if
#' you want to inspect the pre-deploy files.
#' @param delete_after_deploy If TRUE, deletes all files after deploying. 
#' @param ... Further arguments passed to [rsconnect::deployApp()]
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_info
#' @importFrom rsconnect accounts deployApp
#' @importFrom R.utils copyDirectory
#' @importFrom uuid UUIDgenerate
#' @importFrom yaml write_yaml
#' @importFrom shintodb decrypt_config_file has_config_entry
#' @importFrom shintoshiny log_rsconnect_deployments connect_db_rsconnect_deployments
#' @importFrom DBI dbDisconnect
#' @export
#' @importFrom shintodb has_config_entry
deploy_now <- function(tenant,
                       appname = NULL,
                       where = "development",
                       appid = NULL,
                       appid_from_tenant_list = TRUE,
                       db_config_file = "conf/config.yml",
                       log_deployment = TRUE,
                       launch_browser = FALSE,
                       deploy_location = NULL,
                       delete_after_deploy = TRUE,
                       ...
){
  
  
  posit_server <- get_server(where)
  
  cli::cli_alert_info(glue::glue("Deploying Juno for tenant {tenant} to {where} ({format(Sys.time())})"))
  
  # where to copy the files, locally
  if(is.null(deploy_location)){
    
    deploy_location <- file.path(tempdir(), "juno")
    unlink(deploy_location, recursive = TRUE, force = TRUE)
    dir.create(deploy_location)
    
  }
  
  # check if we have a DB connection
  have_db <- shintodb::has_config_entry(tenant, where)
  if(!have_db){
    cli::cli_alert_danger(glue::glue("Stop Juno deployment - no database connection found in config for {tenant} in section {where}"))
    return(invisible(NULL))
  }
  
  # read from tenant_list
  if(is.null(appname)){
    appname <- appname_tenant(tenant)
  }
  
  # production deployment
  if(where %in% c("production", "eindhoven_premium")){
    
    check_prod <- has_production_tenant(tenant)
    if(!check_prod){
      cli::cli_alert_warning(glue::glue("Tenant {tenant} not deployed to production/premium server - set 'production: yes' in tenant_list.yml"))
      return(invisible(NULL))
    }
    
  }
  
  
  # posit connect account / user
  acc <- rsconnect::accounts()
  
  posit_user <- acc$name[acc$server == posit_server]
  if(length(posit_user) == 0){
    stop(paste("Connect a user to", posit_server, "using the Rstudio Posit Connect button"))
  }
  
  cli::cli_alert_success(glue::glue("Posit connect user verified: {posit_user}"))
  
  # Files
  directories <- c(
    "conf",
    file.path("config_site", tenant),
    "modules",
    "preload",
    "R",
    "www"
  )
  
  lapply(directories, function(p){
    if(dir.exists(p)){
      R.utils::copyDirectory(p, to = file.path(deploy_location, p), overwrite = TRUE)  
    }
  })
  
  root_files <- c("global_definition.R",
                  "global.R",
                  "NEWS.md",
                  "server.R",
                  "tenant_list.yml",
                  "this_version.yml",
                  "ui.R",
                  "VERSION")
  
  file.copy(root_files, deploy_location)
  
  # extra files
  file.copy("config_site/help.yml", file.path(deploy_location, "config_site/help.yml"))
  
  dir.create(file.path(deploy_location, "data_public"), showWarnings = FALSE)
  file.copy("data_public/osm_icon_key.csv", file.path(deploy_location, "data_public/osm_icon_key.csv"))
  
  cli::cli_alert_success("Copying files to deployment directory - done")

  # manifest
  manif <- list(
    timestamp = format(Sys.time()),
    uuid = uuid::UUIDgenerate(),
    git = shintoshiny::read_git_version()
  )
  yaml::write_yaml(manif, file.path(deploy_location, "shintoconnect_manifest.yml"))
  
  
  # set tenant
  wbmr::set_tenant(tenant, deploy_location)
  
  #if(shintodb::config_is_encrypted(db_config_file)){
  # unencrypted wordt toch geskipt
  if(file.exists(db_config_file)){
    
    out_conf <- file.path(deploy_location, db_config_file)
    shintodb::decrypt_config_file(out_conf, out_conf)
    
    if(!delete_after_deploy){
      on.exit(
        shintodb::encrypt_config_file(out_conf, out_conf)
      )
    }
  }
  
  # find the app id in the config
  if(is.null(appid) & appid_from_tenant_list){
    if(where == "development"){
      appid <- value_tenant(tenant, "devappid")[[1]]
    } else if(where == "production"){
      appid <- appid_tenant(tenant)[[1]]
    } else {
      appid <- NULL
    }
    
  }
  
  if(!is.null(appid)){
    resp <- rsconnect::deployApp(
      appDir = deploy_location,
      appId = appid,
      account = posit_user,
      server = posit_server,
      launch.browser = launch_browser,
      ...
    )  
  } else {
    resp <- rsconnect::deployApp(
      appDir = deploy_location,
      appName = appname,
      appTitle = appname,
      account = posit_user,
      server = posit_server,
      launch.browser = launch_browser,
      forceUpdate = TRUE,
      ...
    )
  }
  # Deploy de app
  
  
  if(isTRUE(resp) && log_deployment){
    
    if(!shintodb::has_config_entry("rsconnect_deployments", where = "default")){
      
      cli::cli_alert_warning("No DB connection config found for 'rsconnect_deployments', deployment will not be logged.")
      
    } else {
      
      # Schrijf deployment info naar rsconnect_deployments database
      con <- shintoshiny::connect_db_rsconnect_deployments(db_config_file)
      
      on.exit({
        DBI::dbDisconnect(con)
      })
      
      shintoshiny::log_rsconnect_deployments(con,
                                             appname = appname,
                                             environment = where,
                                             userid = posit_user,
                                             shintoconnect_manifest = manif,
                                             version =  wbmr::read_version()
                                             )
      
    }
    
  }
  
  if(delete_after_deploy){
    unlink(deploy_location, recursive = TRUE, force = TRUE)  
    cli::cli_alert_info("Deploy files have been deleted")
  }
  
  cli::cli_alert_success(paste("Juno deployment -", tenant,"- successful"))
  
}

