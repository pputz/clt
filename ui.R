
library(shiny)

numericInputRow <- function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = value, min = 0, class="input-small"))
}

shinyUI(fluidPage(

  # Application title
  titlePanel("Simulation of Central Limit Theorem"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      br(), br(), br(), br(), br(),
      HTML("<h4> Probabilities for <br/> Each Side of a Die<h4/>"),
      h5("Enter any non-negative number"),
      numericInputRow("on", label = "Side 1", value = 1),
      numericInputRow("tw", label = "Side 2", value = 1),
      numericInputRow("th", label = "Side 3", value = 1),
      numericInputRow("fo", label = "Side 4", value = 1),
      numericInputRow("fi", label = "Side 5", value = 1),
      numericInputRow("si", label = "Side 6", value = 1),
      
      br(), br(), br(), br(), br(), br(), br(),
      hr(),
      br(), br(), br(), br(), br(), br(), br(),
      
      h4("Sampling Parameters"),
      sliderInput("n", label = "Sample Size",
                  min = 1, max = 100, value = 4, step = 1),
      
      selectInput("rep", label = "Repetitions",
                  choices = c(10, 100, 1000, 10000), selected = 1000)
    ),
    

    # Show a plot of the generated distribution

    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation",
                 
                 h4("Probability Distribution Function of a (Loaded) Die"),
                 plotOutput("pdf"),
                 verbatimTextOutput("text1"),
                 
                 
                 h4("Distribution of Sample Means"),
                 plotOutput("sampleMeans"),
                 verbatimTextOutput("text2")
                ),
        tabPanel("Documentation", 
                 verbatimTextOutput("documentation")
                )
      )
    )
  )
))
