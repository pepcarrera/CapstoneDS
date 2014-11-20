library(shiny)

shinyUI(fluidPage(
        titlePanel("Predict Next Word"),
        h4('This application will attempt to suggest your next word (it will give you three options) based on your input of 1, 2, or 3 words in a sentenace'),
        sidebarLayout(
        sidebarPanel( 
                textInput('word1', 'Type in first word'),
                textInput('word2', 'Type in second word'),
                textInput('word3', 'Type in third word'),
        ),      
        mainPanel(
                h3('Results of prediction'),
                textOutput('prediction'),
                h4('The above number predicts the next word of the setenace')
        ))
))