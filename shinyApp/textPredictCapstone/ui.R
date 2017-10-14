#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Prediction Engine"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(htmlOutput("sideText")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Word Prediction",
      textAreaInput("textAreaIn", "Insert your text here:", "...", width = "500px"),
      verbatimTextOutput("value"),
      actionButton("submit", "Predict next word"),
      uiOutput("nextWord")
      ),
      tabPanel("Want to know more?",
              uiOutput("additionalText")
               )
      )
    )
  )
))
