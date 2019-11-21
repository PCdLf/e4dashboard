
hide_tab <- function(value){
  shinyjs::hide(selector = glue("li > a[data-value='{value}']")) 
}

show_tab <- function(value){
  shinyjs::show(selector = glue("li > a[data-value='{value}']")) 
}
