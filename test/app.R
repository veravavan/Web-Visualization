library(shiny)

#source('test.R')

ui <- fluidPage(
  titlePanel("Data Viewer"),
  sidebarPanel(
    checkboxGroupInput("loc", "Location", levels(data_melt$location),
                       selected = 'p1'),
    selectInput("var", "Variable", levels(data_melt$variable)),
    checkboxGroupInput('dis', 'Display:', c('dygraph', 'map'))
  ),
  mainPanel(
    plotOutput('plot', click = "plot_click"),
    verbatimTextOutput("plot_clicked_points"),
    conditionalPanel(
      condition="input.dis == 'dygraph'",
      dygraphOutput("plotdy")
    ),
    conditionalPanel(
      condition="input.dis == 'map'",
      leafletOutput("plotle")   
    )
  )
)

server <- function(input, output) {
  data_to_plot <- reactive({
    data_to_plot <- data_melt[location %in% input$loc & variable == input$var]
  })
  output$plot <- renderPlot({
    ggplot(data_to_plot(), aes(x = time, y = value, col=location)) + geom_line()
  })
  output$plot_clicked_points <- renderPrint({
    nearPoints(data_to_plot(), input$plot_click)
  })
  output$plotdy <- renderDygraph({
    dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% dyRangeSelector()
  })
  output$plotle <- renderLeaflet({
    leaflet() %>% setView(lng = 15, lat = 50, zoom = 7) %>% addTiles() %>%
      addAwesomeMarkers(lng=c(15.75, 12.74, 18.04), lat=c(50.75,49.75, 59.99),
                        label=c('p1', 'p2', 'p3'), 
                        icon = awesomeIcons('star', markerColor='orange'))
  })
}

shinyApp(ui = ui, server = server)
