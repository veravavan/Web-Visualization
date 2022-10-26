library(shiny)

ui <- fluidPage(
  titlePanel("Data Viewer"),
  textInput("tree", "Enter your favourite tree:"),
  textOutput("tree_text")
)

server <- function(input, output) {
  output$tree_text <- renderText({
    return(paste0("Your favourite tree is ", input$tree))
  })
}

shinyApp(ui = ui, server = server)