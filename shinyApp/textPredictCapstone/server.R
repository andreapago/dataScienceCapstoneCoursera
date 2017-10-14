#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

local<-FALSE

if(local){
  dir<-"~/Documents/coursera/Course10-Capstone/Rproject/"
  dataDir<-"~/Documents/coursera/Course10-Capstone/Rproject/grams/grams005/"
} else{
  dir<-"data/"
  dataDir<-dir
}


source(paste0(dir,"testScriptForModelCleaned.R"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$sideText<-renderText("App loading, please wait...")
  
  withProgress(message = 'Loading gram dictionary', value = 1, min=1, max=5, {
  #prepare data load them at the beginning
  incProgress(1/5, detail = paste("Dictionary part", 1))
    
  sixGrams<-readRDS(paste0(dataDir,"sixGram.RDS"))
  incProgress(2/5, detail = paste("Dictionary part", 2))
  fiveGrams<-readRDS(paste0(dataDir,"fiveGram.RDS"))
  incProgress(3/5, detail = paste("Dictionary part", 3))
  fourGrams<-readRDS(paste0(dataDir,"fourGram.RDS"))
  incProgress(4/5, detail = paste("Dictionary part", 4))
  threeGrams<-readRDS(paste0(dataDir,"threeGram.RDS"))
  incProgress(5/5, detail = paste("Dictionary part", 5))
  twoGrams<-readRDS(paste0(dataDir,"twoGram.RDS"))
  })
   
  
  
  output$additionalText<-renderText("<b>Background:</b><br>
                                  We write more and more using computers,
                                  smartphones and tablet and we are always in a hurry, trying to
                                  text everywhere from the bus stop to the bed or even the toilet!!
                                  Why not making it simpler and even faster?
                                  The idea is to help you have your sentences written faster by 
                                  predicting what word you are going to type next!<p>

                                  <b>The approach:</b><br>
                                  The software is able to predict the word that follow a
                                  certain text written, by computing the probability possible following words.
                                  The underlying probabilistic model of the language is a probability chain 
                                  of observing a sequence of words. In order to simplify the model we assume
                                  that a word to appear in a sentence depends on the previous k words only (Markov assumtion).<p>
                                  <b>The app:</b><br>
                                  The software consider k up to 5 that is: in order to predict the 
                                  following word in a sentence the previous 5 words (5-gram in the Natural Language processing
                                  community) are used to compute the
                                  probability of the possible combination of the chain of those 5 words and the new word (i.e., 
                                  the word to predict). 
                                  Furthermore, in the app the Kats Back-Off model is implemented in order to account for unseen
                                  words combination (n-grams) in the training. This method helps to generalize to word 
                                  combinations that might be possible, but not directly present as n-grams in the training set.                                  
                                  The app in this prototype is very easy to use: just enter a phrase (or at least one word) in the 
                                  text area and then click on the button that allows to predict the following word. More in 
                                  general the idea is to use this app in any application in which the user needs to type text. 
                                  Could be embedded in an email editor program, in the messaging software of your phone, in social
                                  media apps.<p>
                                  <b>Cool stuff:</b><br> 
                                  The text analyzed to compute the word chains and thus the probabilities of 
                                  new words comes from an English text corpus composed of 3 types of documents:
                                  Tweets, Blog Posts, and News articles. The corpus is combined of about 4 million lines of text
                                  totalling to more than 100 millions words. For this working example a small random sample from the 
                                  corpus is used that is 0.5% of the total lines of text available. The choice of such a sample is 
                                  motivated by the limitations in the computational resources of the shiny platform and the fast 
                                  response required by the app.
                                  The result with this small sample allows on one hand a relative small set of sentences (especially
                                  to be samples and recognized especially when considering long N-grams), on the other hand
                                  the app is fast and producing result also in a system with very limited resources like the shiny app.
                                  Further, the application is really light in terms of the on-line processing that is required, since 
                                  the heavy work of creating the \"dictionaries\" of words combinations (these are actually grams look-up 
                                  tables from 2-grams up to 6-grams) are pre-processed off-line. These dictionaries are loaded in
                                  memory at the start of the app.")
  
  
  
  
  
  output$sideText<-renderText("The application is able to predict the next word 
  in the text you are writing. Please fill some text in the text box on the side.</br>
      Once you have entered your text click <b>Predict next word</b> button.")
  
  observeEvent(input$submit, {
    
    print(input$textAreaIn)
    
    selectedWord<-probabilityWord(list(sixGrams,fiveGrams,fourGrams,threeGrams,twoGrams),input$textAreaIn)
    #selectedWord<-probabilityWord(list(twoGrams),input$textAreaIn)
    
    print(selectedWord)
    output$nextWord <- renderText(paste("<br>The word predicted is: ", "<h3><b>",selectedWord,"</b></h3>"))
  })
  
  
  
   
  
  
})
