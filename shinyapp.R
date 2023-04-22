
##install.packages("shinythemes")
library(tidyverse)
library(shiny)
library(shinythemes)



ui<-fluidPage(
 titlePanel("In a galaxy far far away..."),
 sidebarLayout(
   sidebarPanel(
     textInput(inputId="text_input",label="Input text here:")
   ),
   mainPanel(
     textOutput(outputId = "text_output")
   )
 )
)

server<-function(input, output) {
  
  output$text_output<-renderText({
    paste("You typed:",input$text_input)
  })
  
}

shinyApp(ui=ui, server=server)


  