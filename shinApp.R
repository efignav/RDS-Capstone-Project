library(shiny)
rm(list=ls())

left_hand_panel = fluidPage(
  HTML("<center><h2>Next Word Prediction</h2></center>"),
  HTML("<hr>"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("Input"), value = "ARE u TAlKING to"),
      helpText("Type in a 1, 2 or 3-word phrase. Predicted next word will be displayed on the right."),
      submitButton("Predict"),
      HTML("<br><br>")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(HTML("<h3>Output</h3>"), 
                 br(),
                 h4('Cleaned words'),
                 verbatimTextOutput('preprocessed'),
                 h4('Predictions'),
                 verbatimTextOutput('prediction')
        ),
        tabPanel(HTML("<h3>About</h3>"), 
                 br(),
                 HTML("<center><h5>Application developed in R for the Data Science Capstone Project</h5></center>"),
                 HTML("<center><h5>by Enrique Figueroa</h5></center>"),
                 HTML("<center><h5>December 2021</h5></center>")
        )
      )
    )
  )
)

source('prediction.R')

right_hand_panel = function(input, output){
  output$inputValue = renderPrint({input$text})
  
  output$prediction = renderPrint({
    get_next_word(input$text, unigrams, bigrams, trigrams, maxResults = 3)
  })
  
  output$preprocessed = renderPrint({ preprocess_input(input$text) })
}

shinyApp(ui=left_hand_panel, server=right_hand_panel)
