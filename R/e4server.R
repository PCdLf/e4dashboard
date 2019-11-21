e4server <- function(input, output, session) {

#----- Setup -----
  rv <- reactiveValues(
    data = readRDS("rvdata.rds"),
    datafolder = "C:\\repos\\BVI\\Sensor_data\\Peter",
    calendar = readRDS("rvcalendar.rds")
  )
  
  hide_tab("plottab")
  
#----- Choose directory, read files ----- 
  observeEvent(input$btn_choose_dir, {
    
    # Werkt alleen op Windows!
    rv$datafolder <- choose.dir(default = "c:/repos/BVI/Sensor_data", #getwd(), 
                         caption = "Choose a ZIP file with E4 data.")
    
    # See which, if any, zip files are in the chosen folder
    zips <- dir(rv$datafolder, pattern = "[.]zip$", full.names = TRUE)
    
    # The names of the vector are used as the displayed options in pickerInput.
    # The actual value of the vector (here: the full path name) will be assigned
    # to the input slot (here: input$pick_zips)
    names(zips) <- basename(zips)
    updatePickerInput(session, "pick_zips", choices = zips)
    
    # Display the picker, after the choices are updated.
    shinyjs::show("pick_zips")
    
  })
  
  
  observeEvent(input$pick_zips, {
    
    shinyjs::show("btn_read_data")
    shinyjs::show("btn_reset")
    
  })
  
  observeEvent(input$btn_read_data, {
    
    # Read selected ZIP files
    fns <- input$pick_zips
    
    # Read data into a list (Each element of the list contents from 1 zip file)
    data <- list()
    n <- length(fns) + 1
    withProgress(message = "Reading data...", value = 0, {
      
      for(i in seq_along(fns)){
        
        incProgress(1/n, detail = basename(fns[i]))
        data[[i]] <- read_e4(fns[i])
        
      }
      
      # If more than 1 zip file selected, row-bind them using our custom function
      incProgress(1/n, detail = "Row-binding")
      if(length(fns) > 1){
        rv$data <- rbind_e4(data)
      } else {
        rv$data <- data[[1]]
      }
      
    })
    
    findxl <- dir(rv$datafolder, pattern="[.]xls|xlsx", full.names = TRUE)
    if(length(findxl)){
      if(length(findxl) > 1){
        showModal(modalDialog(
          title = "Problem reading calendar data",
          "More than one XLS(X) file found in directory!",
          easyClose = TRUE
        ))
      }
      rv$calendar <- read_calendar(findxl)
    }


    # Message: data read!
    output$msg_data_read <- renderUI({
      tags$p("Data read successfully!", style = "color: blue;")
    })

    
  })
  
  
  observeEvent(input$btn_reset, {
    
    rv$data <- NULL
    rv$datafolder <- NULL
    rv$calendar <- NULL
    shinyjs::hide("pick_zips")
    shinyjs::hide("btn_read_data")
    shinyjs::hide("btn_reset")
    output$msg_data_read <- renderUI("")
      
  })
  
  
#----- Calendar data -----
  
  # Make sure to use DT:: to use the right renderDataTable (shiny has an old version)
  output$dt_calendar <- DT::renderDataTable({
    
    req(rv$calendar)
    
    rv$calendar %>%
      mutate(Date = format(Date, "%Y-%m-%d"),
             Start = format(Start, "%H:%M:%S"),
             End = format(End, "%H:%M:%S")
             ) %>% 
      datatable()
    
  })
  
  
#----- Visualization -----
  observeEvent(input$btn_make_plot, {
    
    req(rv$data)
    show_tab("plottab")
    updateTabsetPanel(session, "plottabbox", selected = "plottab")
    
    if(input$check_add_calendar_annotation){
      annotatedata <- rv$calendar
    } else {
      annotatedata <- NULL
    }
    
    plots <- e4_timeseries_plot(rv$data,
                                main_title = input$txt_plot_main_title,
                                calendar_data = annotatedata)
    
    output$dygraph_current_data1 <- renderDygraph(plots[[1]])
    output$dygraph_current_data2 <- renderDygraph(plots[[2]])
    output$dygraph_current_data3 <- renderDygraph(plots[[3]])
    output$dygraph_current_data4 <- renderDygraph(plots[[4]])
    
  })


}
