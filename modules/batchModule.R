

batchModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      shinydashboard::box(
        width = 6,
        title = "Batch Analysis",
        
        
       actionButton(ns("btn_select_folder"), "Select input folder", 
                    icon = icon("folder-open"), class = "btn-light"),
       
       tags$br(),
       uiOutput(ns("ui_n_files_found")),
       shinyjs::hidden(
         actionButton(ns("btn_run_batch"), "Run batch analysis", icon = icon("check"), class = "btn-success")
       )
       
       
        
        
      ),
      
      verbatimTextOutput(ns("txt_out"))
    )
    
  )
  
}

choose_directory <- function(caption = 'Select data directory') {
  if (.Platform$OS.type == "windows") {
    choose.dir(caption = caption) 
  } else {
    tk_choose.dir(caption = caption)
  }
}

batchModule <- function(input, output, session){
  
  
  folder_in <- reactiveVal()
  
  observeEvent(input$btn_select_folder, {
    folder_in(
      choose_directory()
    )
    
    shinyjs::show("btn_run_batch")
  })
  
  n_zip_files <- reactive({
    req(folder_in())
    length(list.files(folder_in(), pattern = "[.]zip$", recursive = TRUE, full.names = TRUE))
  })
  
  output$ui_n_files_found <- renderUI({
    
    req(n_zip_files())
    
    paste(n_zip_files(), "ZIP files found in the selected folder, continue?")
  })
  

  observeEvent(input$btn_run_batch, {
    
    toastr_info("Batch analysis started, this can take a while!")
    e4tools::batch_analysis(folder_in())
    toastr_info("Batch analysis completed!")
    
  })
  
}


