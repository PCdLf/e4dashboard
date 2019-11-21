


e4_timeseries_plot <- function(data, colours = c("#1874CD", "#FF7F00", "#458B00", "#68228B")){
  
  my_dygraph <- function(ts, main_title = FALSE, ylab_title = FALSE, draw_x_axis = FALSE, color = "black"){
    dygraph(ts, main = main_title, group = "plot2", ylab = ylab_title, height = 150, width = 900) %>%   
      dyHighlight(highlightCircleSize = 5) %>%
      dyOptions(drawPoints = FALSE, 
                drawXAxis = draw_x_axis,
                colors = color)
  }
  
  list(
    my_dygraph(as_timeseries(data$EDA, name_col = "EDA"), main_title = "Test", ylab_title = "EDA", color = colours[1]),
    my_dygraph(as_timeseries(data$HR, name_col = "HR"), ylab_title = "HR", color = colours[2]),
    my_dygraph(as_timeseries(data$TEMP, name_col = "Temperature"), ylab_title = "Temperature", color = colours[3]),
    my_dygraph(as_timeseries(data$ACC, index = 5, name_col = "Movement"), draw_x_axis = TRUE, ylab_title = "Movement", color = colours[4]) %>%
    dyRangeSelector()
  )
}


