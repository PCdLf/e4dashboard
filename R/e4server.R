#' e4server
#' @description responsible for server transformations
#' @param input standard input, output, session follwing Shinyapp
#' @param output standard input, output, session follwing Shinyapp
#' @param session standard input, output, session follwing Shinyapp
#' @export
#' @importFrom tcltk tk_choose.files

e4server <- function(input, output, session) {

#----- Setup -----
  rv <- reactiveValues(
    data = NULL, #readRDS("rvdata.rds"),
    datafolder = NULL, #"C:\\repos\\BVI\\Sensor_data\\Peter",
    calendar = NULL, #readRDS("rvcalendar.rds"),
    timeseries = NULL #readRDS("rvtimeseries.rds") 
  )
  
  hide_tab("plottab")
  
#----- Choose directory, read files ----- 
  observeEvent(input$btn_choose_files, {
    
    # Werkt alleen op Windows!
    rv$datafolder <- tcltk::tk_choose.files(default = "c:/repos/BVI/Sensor_data", #!!!!!
                         caption = "Choose ZIP file(s) with E4 data.",
                         filters = matrix(c("ZIP files",".zip"),ncol=2)
                         )
    
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
    
    rv$timeseries <- list(
      EDA = as_timeseries(rv$data$EDA, name_col = "EDA"),
      HR = as_timeseries(rv$data$HR, name_col = "HR"),
      TEMP = as_timeseries(rv$data$TEMP, name_col = "Temperature"),
      MOVE = as_timeseries(rv$data$ACC, index = 5, name_col = "Movement")
    )
    
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
  
  observe({

    ts <- rv$timeseries
    req(ts)
    nice_range <- function(x){
      round(range(x, na.rm = TRUE), digits = 2)
    }

    updateNumericRangeInput(session, "slide_yaxis_eda", label = "EDA Y-axis range", 
                            value = nice_range(ts$EDA))
    updateNumericRangeInput(session, "slide_yaxis_hr", label = "HR Y-axis range", 
                            value = nice_range(ts$HR))
    updateNumericRangeInput(session, "slide_yaxis_temp", label = "TEMP Y-axis range",
                            value = nice_range(ts$TEMP))
    updateNumericRangeInput(session, "slide_yaxis_move", label = "MOVE Y-axis range",
                            value = nice_range(ts$MOVE))
    
    
  })

  yaxis_ranges <- reactive(
    list(
      EDA = input$slide_yaxis_eda,
      HR = input$slide_yaxis_hr,
      TEMP = input$slide_yaxis_temp,
      MOVE = input$slide_yaxis_move
    )
  )

  
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
    
    req(rv$timeseries)
    show_tab("plottab")
    updateTabsetPanel(session, "plottabbox", selected = "plottab")
    
    if(input$check_add_calendar_annotation){
      annotatedata <- rv$calendar
    } else {
      annotatedata <- NULL
    }
    
    plots <- e4_timeseries_plot(rv$timeseries,
                                main_title = input$txt_plot_main_title,
                                calendar_data = annotatedata,
                                average_lines =  input$check_average_lines,
                                yaxis_ranges =  yaxis_ranges()
                                )
    
    output$dygraph_current_data1 <- renderDygraph(plots[[1]])
    output$dygraph_current_data2 <- renderDygraph(plots[[2]])
    output$dygraph_current_data3 <- renderDygraph(plots[[3]])
    output$dygraph_current_data4 <- renderDygraph(plots[[4]])
    
  })

#----- Analysis -----
  
  observeEvent(input$btn_do_analysis, {
    
    
    result <- calculate_heartrate_params(rv$data$IBI, rv$data$EDA)
    output$dt_analysis_output <- renderPrint(t(as.data.frame(out)))
    
    
  })

}


#' e4dashboard
#' @description creates a function for the e4 dashboard.
#' @export

e4dashboard <- function(){
  
  shiny::shinyApp(e4ui(), e4server)
  
}