

batchModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      shinydashboard::box(
        width = 6,
        title = "Batch Analysis",
        
       fluidRow(
         column(6,
                actionButton(ns("btn_select_folder_input"), "Select input folder", 
                             icon = icon("folder-open"), class = "btn-light"),
                uiOutput(ns("ui_folder_in")),
                
                tags$br(),
                uiOutput(ns("ui_n_files_found"))
         ),
         column(6,
                actionButton(ns("btn_select_folder_output"), "Select output folder", 
                             icon = icon("folder-open"), class = "btn-light"),
                uiOutput(ns("ui_folder_out"))
         )
       ),  
       
       
       tags$br(),
       tags$hr(),
       
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
  folder_out <- reactiveVal()
  
  observeEvent(input$btn_select_folder_input, {
    folder_in(
      choose_directory()
    )
    
  })
  
  observeEvent(input$btn_select_folder_output, {
    folder_out(
      choose_directory()
    )
    
  })
  
  output$ui_folder_in <- renderUI({
    paste("Selected:", folder_in())
  })
  
  output$ui_folder_out <- renderUI({
    paste("Selected:", folder_out())
  })
  
  observe({
    
    have_in <- !is.null(folder_in())
    have_out <- !is.null(folder_out())
    
    if(have_in & have_out){
      shinyjs::show("btn_run_batch")  
    }
    
  })
  
  
  zip_files <- reactive({
    req(folder_in())
    list.files(folder_in(), pattern = "[.]zip$", recursive = TRUE, full.names = TRUE)
  })
  
  n_zip_files <- reactive({
    length(zip_files())
  })
  
  output$ui_n_files_found <- renderUI({
    
    req(n_zip_files())
    paste(n_zip_files(), "ZIP files found in the selected folder.")
  })
  

  observeEvent(input$btn_run_batch, {
    
    toastr_info("Batch analysis started, this can take a while!")
    
    zips <- zip_files() 
    path_out <- folder_out()
    
    withProgress(message = "Running batch analysis...", value = 0, {
        
      for(i in seq_along(zips)){
        
        out <- read_and_process_e4(zips[i])
        
        fn_root <- basename(tools::file_path_sans_ext(zips[i]))
        out_file <- file.path(path_out, paste0(fn_root, ".rds"))
        
        tm <- try(saveRDS(out, out_file))
        
        if(inherits(tm, "try-error")){
          err <-  attr(tm, "condition")$message
          toastr_error(paste("Some problem with batch analysis, error message:", err))
          break
        }
        
        incProgress(1/length(zips), detail = basename(out_file))
        
      }
      
    })
    
    toastr_info("Batch analysis completed!")
    
  })
  
}


