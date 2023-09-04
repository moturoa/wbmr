
#' Read NEWS
#' @export
#' @rdname news
#' @importFrom dplyr filter
read_news <- function(tenant = NULL){
  data <- tools:::.build_news_db_from_package_NEWS_md("NEWS.md")
  
  if(!is.null(tenant)){
    data <- dplyr::filter(data, Category %in% c("", tenant))
  }
  
  data
}


