
hide_tab <- function(value){
  shinyjs::hide(selector = glue("li > a[data-value='{value}']")) 
}

show_tab <- function(value){
  shinyjs::show(selector = glue("li > a[data-value='{value}']")) 
}



disable_link <- function(name){
  addCssClass(selector = glue("a[data-value='{name}']"), 
              class = "inactivelink")
}

enable_link <- function(name){
  removeCssClass(selector = glue("a[data-value='{name}']"), 
                 class = "inactivelink")
}

side_by_side <- function(...){
  
  mc <- list(...)
  lapply(mc, function(x){
    
    tags$div(style=paste("display: inline-block;",
                         "vertical-align: top;"), 
             x)  
    
  })
  
}


logo_image_with_link <- function(img_path, url){
  tags$a(tags$img(src = img_path, class = "grayscale", width = 130, style = "padding: 10px"), 
         href = url, target = "_blank")
}



