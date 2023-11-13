library(shiny)
library(basicInference)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("basicInference package"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: List for the type of random variables ----
      selectInput(inputId="rv1", label="Type of variable 1:", c("Normal", "Student's t", "Exponential")),
      selectInput(inputId="rv2", label="Type of variable 2:", c("Normal", "Student's t", "Exponential")),
      numericInput(inputId="rv1_mean", label="Mean of variable 1", value=0, min=0),
      numericInput(inputId="rv2_mean", label="Mean of variable 2", value=0, min=0)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Literal result of the test ----
      textOutput("text"),
      # Output: Scatter plot ----
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  generate_data <- eventReactive({
    input$rv1 
    input$rv2
    input$rv1_mean
    input$rv2_mean}, {  
    if (input$rv1=="Normal")
    {
      s1 <- rnorm(500, input$rv1_mean)
    }
    if (input$rv1=="Student's t")
    {
      s1 <- rt(500, 5, input$rv1_mean)
    }
    if (input$rv1=="Exponential")
    {
      s1 <- rexp(500, 1/input$rv1_mean)
    }
    if (input$rv2=="Normal")
    {
      s2 <- rnorm(500, input$rv2_mean)
    }
    if (input$rv2=="Student's t")
    {
      s2 <- rt(500, 5, input$rv2_mean)
    }
    if (input$rv2=="Exponential")
    {
      s2 <- rexp(500, 1/input$rv2_mean)
    }
    res <- t_test(s1, s2)
    return(res)
  })
  output$text <- renderText({
    as.character(generate_data()[5])})
  output$distPlot <- renderPlot({
    plot(generate_data())
  })
}

shinyApp(ui=ui, server=server)