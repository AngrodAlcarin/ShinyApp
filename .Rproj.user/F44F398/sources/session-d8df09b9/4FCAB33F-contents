#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
options(shiny.maxRequestSize=300*1024^2)

# course_data<-readRDS("data/europe.rds")%>% 
#   mutate(AvgTemperatureC=round((AvgTemperatureF-32)*(5/9)))
# summary(course_data)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Europe Shiny App"),
  
  "This is my shiny demo app. It's using a dataset with temperature data from Europe between 2000 and 2019.",
  
  theme=shinytheme("superhero"),
  
  sidebarLayout(
    # Sidebar panel
    sidebarPanel("This is the sidebar panel", 
                 sliderInput(inputId = "year",label = "Year",
                             min = 2000,
                             max=2019,
                             value=2000,
                             step = 1,
                             sep=""
                 ),
                 selectInput(inputId = "country", label="Country",
                             choices=NULL,
                             multiple = TRUE),
                 selectInput(inputId = "city", label="City",
                             choices=NULL,
                             multiple=TRUE),
                 radioButtons(inputId = "tempunit", label="Temperature Unit", 
                              choices=list("Centigrade"="AvgTemperatureC", "Fahrenheit"="AvgTemperatureF"),
                              selected="AvgTemperatureC"
                 ),
                 actionButton(inputId="action",label="GO!"
                 ),
                 fileInput(inputId = "upload",label="Upload your file here (RDS)",accept = c(".rds"),multiple = FALSE)),
    
    # Main panel
    mainPanel("This is the main panel",
              tabsetPanel(type="tabs",
                          tabPanel("Info", "This tab contains general information about the dataframe this app uses.",
                                   verbatimTextOutput(outputId = "summary")),
                          tabPanel("Data","This tab contains the data this app uses.",dataTableOutput(outputId = "europetable"), ),
                          tabPanel("Plots","This tab contains some plots.",
                                   plotOutput(outputId = "countryplot"),
                                   plotOutput(outputId = "cityplot"))))
  )
)
# Define server side logic
server <- function(input, output, session) {
  
  course_data<-eventReactive(input$upload,{
    readRDS(input$upload$datapath)%>% 
      mutate(AvgTemperatureC=round((AvgTemperatureF-32)*(5/9))) %>% 
      summary()
  })
  output$text_output<-renderText({
    paste("Your inputs are:",input$year, " , ",
          input$country, " , ",input$city," , ",input$tempunit)
  })
  output$summary<-renderPrint({
    summary(course_data())
  })
  
  country_df<-eventReactive(eventExpr=input$action, valueExpr = {
    course_data() %>% 
      filter(Year==input$year) %>%
      filter(Country%in%c(input$country)) %>% 
      ungroup()
  })
  
  output$europetable<-renderDataTable(head(country_df(),n=100))
  
  city_df<-eventReactive(eventExpr=input$action, valueExpr = {
    course_data() %>% 
      filter(Year==input$year) %>%
      filter(City%in%c(input$city)) %>%
      ungroup()
  })
  output$countryplot<-renderPlot({
    {if(input$tempunit=="AvgTemperatureC"){ggplot(data=country_df())+
        geom_line(mapping=aes(x=Date,y=AvgTemperatureC, color=Country))+
        ggtitle("Country")+
        theme_classic()
    }
      else{ggplot(data=country_df())+
          geom_line(mapping=aes(x=Date,y=AvgTemperatureF, color=Country))+
          ggtitle("Country")+
          theme_classic()
      }}
    
  })
  
  output$cityplot<-renderPlot({
    {if(input$tempunit=="AvgTemperatureC"){ggplot(data=city_df())+
        geom_line(mapping=aes(x=Date,y=AvgTemperatureC, color=City))+
        ggtitle("City")+
        theme_classic()}
      else{ggplot(data=city_df())+
          geom_line(mapping=aes(x=Date,y=AvgTemperatureF, color=City))+
          ggtitle("City")+
          theme_classic()}}
    
  })
  
  observe({
    
    new_choices<-unique(course_data()$City[course_data()$Country==input$country])
    updateSelectInput(session,inputId="city",choices=new_choices)
  })
}
# Run the application
shinyApp(ui = ui, server = server)
