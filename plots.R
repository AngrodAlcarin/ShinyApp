library(tidyverse)
library(dplyr)

course_data<-readRDS("data/europe.rds")
summary(course_data)

barplot_df<-course_data %>% 
  filter(Year>=1900) %>% 
  filter(Country=="Switzerland") %>% 
  ungroup()
ggplot(data=barplot_df)+
  geom_bar(mapping=aes(x=Year, fill=AvgTemperatureF))

range(course_data$Year)

course_data<-course_data %>% 
  mutate(AvgTemperatureC=(AvgTemperatureF-32)*(5/9))

lineplot_df<-course_data %>% 
  select(Country,Year,Date,AvgTemperatureC,City) %>% 
  filter(Country%in%c("Switzerland","Germany","Austria")) %>% 
  filter(City%in%c("Bern","Geneva","Zurich","Berlin","Vienna","Bonn","Frankfurt")) %>% 
  filter(Year==2019) %>% 
  ungroup()

ggplot(data=lineplot_df)+
  geom_line(mapping=aes(x=Date,y=AvgTemperatureC,color=City))+
  geom_line(mapping=aes(x=Date,y=AvgTemperatureC,color=Country))



country_df<-reactive({
  course_data %>% 
    filter(Year==input$year) %>%
    filter(Country%in%c(input$country)) %>% 
    mutate(AvgTemperature={if(input$tempunit=="AvgTemperatureC"){AvgTemperatureC}
      else{AvgTemperatureF}})
  ungroup()
})



output$countryplot<-reactive(if(input$tempunit=="AvgTemperatureC"){renderPlot({
  ggplot(data=country_df())+
    geom_line(mapping=aes(x=Date,y=AvgTemperatureC, color=Country))
})}
else{renderPlot({
  ggplot(data=country_df())+
    geom_line(mapping=aes(x=Date,y=AvgTemperatureF, color=Country))
})})



output$countryplot<-renderPlot({reactive(
  if(input$tempunit=="AvgTemperatureC"){ggplot(data=country_df())+
      geom_line(mapping=aes(x=Date,y=AvgTemperatureC, color=Country))}
  else{ggplot(data=country_df())+
      geom_line(mapping=aes(x=Date,y=AvgTemperatureF, color=Country))})
})


     