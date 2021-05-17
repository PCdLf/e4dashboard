
disable_link("tabCalendar")
disable_link("tabVisualization")
disable_link("tabAnalysis")
disable_link("tabReport")

options(shiny.maxRequestSize=30*1024^2) 

#----- Setup -----
rv <- reactiveValues(
  data = NULL, #readRDS("rvdata.rds"),
  data_agg = NULL,
  zip_files = NULL,
  calendar = NULL, #readRDS("rvcalendar.rds"),
  timeseries = NULL,
  last_analysis = NULL #readRDS("rvtimeseries.rds") 
)

hide_tab("plottab")
hide_tab("plotannotations")

#----- Choose directory, read files ----- 
observeEvent(input$select_zip_files, {
  
  rv$zip_files <- input$select_zip_files
  
  # Display the picker, after the choices are updated.
  shinyjs::show("btn_read_data")
  shinyjs::show("btn_reset")
})

output$msg_files_selected <- renderUI({
  
  req(rv$zip_files)
  n <- nrow(rv$zip_files)
  if(n > 0){
    tags$p(glue("You have selected {n} ZIP files."))
  }
  
})



observeEvent(input$btn_read_data, {
  
  # Read selected ZIP files
  fns <- rv$zip_files$datapath
  fn_names <- rv$zip_files$name
  
  # Read data into a list (Each element of the list contents from 1 zip file)
  data <- list()
  n <- length(fns) + 1
  withProgress(message = "Reading data...", value = 0, {
    
    for(i in seq_along(fns)){
      
      incProgress(1/n, detail = fn_names[i])
      data[[i]] <- e4tools::read_e4(fns[i])
      
    }
    
    # If more than 1 zip file selected, row-bind them using our custom function
    incProgress(1/n, detail = "Row-binding")
    if(length(fns) > 1){
      rv$data <- e4tools::rbind_e4(data)
    } else {
      rv$data <- data[[1]]
    }
    
    rv$data_agg <- e4tools::aggregate_e4_data(rv$data)
    
  })
  
  rv$timeseries <- list(
    EDA = e4tools::as_timeseries(rv$data_agg$EDA, name_col = "EDA"),
    HR = e4tools::as_timeseries(rv$data_agg$HR, name_col = "HR"),
    TEMP = e4tools::as_timeseries(rv$data_agg$TEMP, name_col = "Temperature"),
    MOVE = e4tools::as_timeseries(rv$data_agg$ACC, index = 5, name_col = "Movement")
  )
  
  
  # Message: data read!
  output$msg_data_read <- renderUI({
    tags$p("Data read successfully!", style = "color: blue;")
  })
  
  
  enable_link("tabCalendar")
  enable_link("tabVisualization")
  enable_link("tabAnalysis")
  
  # Fill analysis times
  tms <- range(rv$data[[1]]$DateTime)
  updateDateInput(session, "date_analysis_start",
                  value = min(as.Date(tms)),
                  min = min(as.Date(tms)),
                  max = max(as.Date(tms))
  )
  updateDateInput(session, "date_analysis_end",
                  value = max(as.Date(tms)),
                  min = min(as.Date(tms)),
                  max = max(as.Date(tms))
  )
  
  updateNumericInput(session, "time_hour_start",
                     value = hour(min(tms)), min = 0, max = 23)
  updateNumericInput(session, "time_hour_end",
                     value = hour(max(tms)), min = 0, max = 23)
  
  updateNumericInput(session, "time_minute_start",
                     value = minute(min(tms)), min = 0, max = 59)
  updateNumericInput(session, "time_minute_end",
                     value = minute(max(tms)), min = 0, max = 59)
  
  updateNumericInput(session, "time_second_start",
                     value = second(min(tms)), min = 0, max = 59)
  updateNumericInput(session, "time_second_end",
                     value = second(max(tms)), min = 0, max = 59)
  
})


observeEvent(input$btn_reset, {
  
  rv$data <- NULL
  rv$datafolder <- NULL
  rv$calendar <- NULL
  rv$last_analysis <- NULL
  rv$timeseries <- NULL
  
  shinyjs::hide("pick_zips")
  shinyjs::hide("btn_read_data")
  shinyjs::hide("btn_reset")
  
  disable_link("tabCalendar")
  disable_link("tabVisualization")
  disable_link("tabAnalysis")
  disable_link("tabReport")
  
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


observeEvent(input$select_calendar_file, {
  
  dfr <- input$select_calendar_file
  
  rv$calendar <- read_calendar(dfr$datapath)
  
  shinyjs::hide("calendar_in_block")
  shinyjs::show("calendar_block")
})

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
  
  
  if(isTRUE(nrow(rv$calendar))){
    show_tab("plotannotations")
  }
  
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

current_visible_annotations <- reactive({
  
  # !!!! WATCH THE TIMEZONE!
  # Problem: cannot read timezone info from rv$calendar$Start, should
  # be saved there (as tzone attribute) when reading calendar
  ran <- suppressWarnings({
    ymd_hms(input$dygraph_current_data4_date_window, tz = "CET")
  })
  
  filter(rv$calendar,
         !((Start > ran[[2]] && End > ran[[2]]) |
             (Start < ran[[1]] && End < ran[[1]]))
  )
  
})


observeEvent(input$btn_panel_float, {
  
  shinyjs::show("thispanel")
  
})

output$dt_panel_annotations <- DT::renderDT({
  
  current_visible_annotations() %>%
    mutate(Date = format(Date, "%Y-%m-%d"),
           Start = format(Start, "%H:%M:%S"),
           End = format(End, "%H:%M:%S")
    ) %>%
    datatable(width = 500)
  
})

output$dt_annotations_visible <- DT::renderDT({
  
  current_visible_annotations() %>%
    mutate(Date = format(Date, "%Y-%m-%d"),
           Start = format(Start, "%H:%M:%S"),
           End = format(End, "%H:%M:%S")
    ) %>%
    datatable(width = 500)
  
})

#----- Analysis -----


#range(as.Date(rv$data[[1]]$DateTime))

observeEvent(input$btn_do_analysis, {
  
  
  start <- ISOdatetime(
    year = year(input$date_analysis_start),
    month = month(input$date_analysis_start),
    day = day(input$date_analysis_start),
    hour = input$time_hour_start,
    min = input$time_minute_start,
    sec = input$time_second_start,
    tz = "UTC"
  )
  
  end <- ISOdatetime(
    year = year(input$date_analysis_end),
    month = month(input$date_analysis_end),
    day = day(input$date_analysis_end),
    hour = input$time_hour_end,
    min = input$time_minute_end,
    sec = input$time_second_end,
    tz = "UTC"
  )
  
  data <- rv$data
  
  data$IBI$datetime <- force_tz(rv$data$IBI$DateTime, "UTC")
  data$EDA$datetime <- force_tz(rv$data$EDA$DateTime, "UTC")
  data$ACC$datetime <- force_tz(rv$data$ACC$DateTime, "UTC")
  data$TEMP$datetime <- force_tz(rv$data$TEMP$DateTime, "UTC")
  data$HR$datetime <- force_tz(rv$data$HR$DateTime, "UTC")
  
  data$IBI <- filter(data$IBI, 
                     datetime >= start,
                     datetime <= end)
  data$EDA <- filter(data$EDA, 
                     datetime >= start,
                     datetime <= end)
  data$ACC <- filter(data$ACC, 
                     datetime >= start,
                     datetime <= end)
  data$TEMP <- filter(data$TEMP, 
                      datetime >= start,
                      datetime <= end)
  data$HR <- filter(data$HR, 
                    datetime >= start,
                    datetime <= end)
  
  rv$last_analysis <- calculate_heartrate_params(data$IBI, data$EDA)
  
  output$dt_analysis_output <- DT::renderDT({
    
    datatable(t(as.data.frame(rv$last_analysis)))
    
  })
  
  enable_link("tabReport")
  
  
})


#----- Report -----

# See https://shiny.rstudio.com/articles/generating-reports.html
output$btn_download_report <- downloadHandler(
  
  filename = "e4_analysis.html",
  content = function(file){
    
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
    
    rmarkdown::render(tempReport, output_file = file)
    
  }
  
)
