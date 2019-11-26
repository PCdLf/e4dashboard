#' hide_tab
#' @description hides tabs in the ui
#' @export

hide_tab <- function(value){
  shinyjs::hide(selector = glue("li > a[data-value='{value}']")) 
}

#' show_tab
#' @description show tabs in the ui
#' @export
show_tab <- function(value){
  shinyjs::show(selector = glue("li > a[data-value='{value}']")) 
}
