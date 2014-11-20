library(shiny)
source("helpers.R")
#Load ngram data
twoFreq <- readRDS("data/twoFreq.rds")
threeFreq <- readRDS("data/threeFreq.rds")
fourFreq <- readRDS("data/fourFreq.rds")
noWord <- readRDS("data/noWord.rds")

# Define server logic required to plot various variables against mpg
shinyServer(
        function(input, output) {
        dataInput <- reactive({
                df <- data.frame(cyl=input$cyl, hp=input$hp, wt=input$wt, am=input$am)
                return(list(df=df))
                })
                
                
                #Render prediction to the screen
                output$prediction <- renderPrint({
                        predictWord(word1=input$word1,word1=input$word2, word1=input$word3)
                        })

                }
        )