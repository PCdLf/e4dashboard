


e4_timeseries_plot <- function(data, 
                               main_title = "",
                               calendar_data = NULL,
                               average_lines = "",
                               yaxis_ranges = NULL,
                               colours = c("#1874CD", "#FF7F00", "#458B00", "#68228B")){
  
  
  my_dygraph <- function(ts, 
                         main_title = FALSE, 
                         ylab_title = FALSE, 
                         draw_x_axis = FALSE, 
                         color = "black",
                         average_line = FALSE,
                         events = calendar_data,
                         events_label = FALSE,
                         yaxis_range = NULL){
    
    begin_time <- min(index(ts)) - 30*60
    
    out <- dygraph(ts, main = main_title, group = "plot2", ylab = ylab_title, height = 150, width = 900) %>%   
      dyHighlight(highlightCircleSize = 5) %>%
      dyOptions(drawPoints = FALSE, 
                drawXAxis = draw_x_axis,
                colors = color)
      # dyAxis(name = "x", valueRange = c(begin_time, NULL))
    
    if(average_line){
      
      out <- out %>%
        dyLimit(mean(ts, na.rm = TRUE), "Mean",
                strokePattern = "solid", color = "blue")
      
    }
    
    if(!is.null(yaxis_range)){
      out <- out %>%
        dyAxis("y", valueRange = yaxis_range)
    }
    
    
    if(!is.null(events)){
      
      # 'Start' events
      for(i in 1:nrow(events)){
        
        txt <- ifelse(events_label, events$Tekst[i], "")
        
        out <- out %>%
          dyEvent(events$Start[i], txt, labelLoc="bottom")
        
      }
      
      
      # Shading events
      eventsub <- filter(events, !is.na(Start) & !is.na(End))
      if(nrow(eventsub)){
        for(i in 1:nrow(eventsub)){
          
          out <- out %>%
            dyShading(from = eventsub$Start[i],
                      to = eventsub$End[i],
                      color = "rgba(220,220,220,0.4)")
          
        }
      }
      
    }
    
  return(out)
  }
  
  list(
    my_dygraph(data$EDA, 
               main_title = main_title, 
               ylab_title = "EDA", 
               events_label = TRUE,
               average_line = "EDA" %in% average_lines,
               yaxis_range = yaxis_ranges$EDA,
               color = colours[1]),
    my_dygraph(data$HR, 
               ylab_title = "HR", 
               average_line = "HR" %in% average_lines,
               yaxis_range = yaxis_ranges$HR,
               color = colours[2]),
    my_dygraph(data$TEMP, 
               ylab_title = "Temperature", 
               average_line = "TEMP" %in% average_lines,
               yaxis_range = yaxis_ranges$TEMP,
               color = colours[3]),
    my_dygraph(data$MOVE, 
               draw_x_axis = TRUE, 
               ylab_title = "Movement", 
               average_line = "MOVE" %in% average_lines,
               yaxis_range = yaxis_ranges$MOVE,
               color = colours[4]) %>%
    dyRangeSelector()
  )
}


read_calendar <- function(fn){
  
  rectify_datetime <- function(date, time){
    ISOdatetime(year(date), month(date), day(date), 
                hour(time), minute(time), second(time))  
  }
  
  read_excel(fn) %>%
    mutate(Start = rectify_datetime(Date, Start),
           End = rectify_datetime(Date, End))
  
}


