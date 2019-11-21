e4server <- function(input, output) {

  mydata <- read_e4("C:/repos/BVI/Sensor_data/Peter/1_A0020A.zip")

  plots <- e4_timeseries_plot(mydata)

  output$dygraph_current_data1 <- renderDygraph(plots[[1]])
  output$dygraph_current_data2 <- renderDygraph(plots[[2]])
  output$dygraph_current_data3 <- renderDygraph(plots[[3]])
  output$dygraph_current_data4 <- renderDygraph(plots[[4]])

}
