

batchModuleUI <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      shinydashboard::box(
        width = 10,
        title = tagList(icon("running"), "Batch Analysis"),
        
        
       fluidRow(
         
         column(12,
                tags$p("Select the input and output folder for a batch analysis of multiple ZIP files.")
         ),
         
         column(6, style = "padding: 16px;",
                actionButton(ns("btn_select_folder_input"), "Select input folder", 
                             icon = icon("folder-open"), class = "btn-light"),
                uiOutput(ns("ui_folder_in")),
                
                tags$br(),
                uiOutput(ns("ui_n_files_found"))
         ),
         column(6,style = "padding: 16px; border:",
                actionButton(ns("btn_select_folder_output"), "Select output folder", 
                             icon = icon("folder-open"), class = "btn-light"),
                
                shinyjs::hidden(
                  tags$div(id = ns("div_same_as_input"),
                     tags$br(),
                     actionButton(ns("btn_output_same_input"), "Same as input folder", 
                                  icon = icon("arrow-alt-circle-left"), class = "btn-secondary")
                  )
                ),
                
                uiOutput(ns("ui_folder_out"))
         )
       ),  
       
       
       tags$br(),
       tags$hr(),
       
       shinyjs::hidden(
         actionButton(ns("btn_run_batch"), "Run batch analysis", icon = icon("check"), class = "btn-success")
       )
        
      ),
      
      tags$div(style = "width: 100%;",
               tags$div(style = "float: right;",
                        helpButtonUI(ns("help"))
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
  
  callModule(helpButton, "help", helptext = .help$batch)
  
  folder_in <- reactiveVal()
  folder_out <- reactiveVal()
  
  observeEvent(input$btn_select_folder_input, {
    chc <- choose_directory()
    if(!is.na(chc)){
      folder_in(chc)  
    }
    
    
  })
  
  observe({
    if(!is.null(folder_in())){
      shinyjs::show("div_same_as_input")
    }
  })
  
  observeEvent(input$btn_select_folder_output, {
    
    chc <- choose_directory()
    
    if(!is.na(chc)){
      folder_out(chc)  
    }
    
  })
  
  observeEvent(input$btn_output_same_input, {
    
    folder_out(folder_in())
    
  })
  
  
  output$ui_folder_in <- renderUI({
    tags$p(folder_in(),
           style = "font-style: italic; font-size : 0.9em;")
  })
  
  output$ui_folder_out <- renderUI({
    tags$p(folder_out(),
           style = "font-style: italic; font-size : 0.9em;")
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
    tagList(
      tags$p(paste(n_zip_files(), "ZIP files found in the selected folder.")),
      tags$p("Select output folder to continue.")
    )
    
  })
  

  observeEvent(input$btn_run_batch, {
    
    toastr_info("Batch analysis started, this can take a while!")
    
    shinyjs::disable("btn_run_batch")
    
    zips <- zip_files() 
    path_out <- folder_out()
    
    withProgress(message = "Running batch analysis...", value = 0, {
        
      for(i in seq_along(zips)){
        
        out <- read_and_process_e4(zips[i])
        
        fn_root <- basename(tools::file_path_sans_ext(zips[i]))
        out_file <- file.path(path_out, paste0(fn_root, ".rds"))
        
        tm <- try(saveRDS(out, out_file))
        
        if(inherits(tm, "try-error")){
          err <- attr(tm, "condition")$message
          toastr_error(paste("Some problem with batch analysis, error message:", err))
          break
        }
        
        incProgress(1/length(zips), detail = basename(out_file))
        
      }
      
    })
    
    toastr_info("Batch analysis completed!")
    shinyjs::enable("btn_run_batch")
  })
  
}


