#' E4 dashboard
#' @export
e4dashboard <- function(){

  shiny::shinyApp(e4ui(), e4server)

}
