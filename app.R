library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)
library(shinyjs)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(extrafont)

Confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

Deaths <-  readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")


Recovered <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")


report <- cbind(Confirmed[,c(1,2,3,4,ncol(Confirmed))], Deaths[,ncol(Deaths)], Recovered[,ncol(Recovered)] )

#names(report)[c(3,4,5,6,7)] <-c("lat","long","Confirmed", "Deaths", "Recovered")
names(report)[c(5,6,7)] <-c("Confirmed", "Deaths", "Recovered")


conf2 <- reshape2::melt(Confirmed, id.vars = colnames(Confirmed)[1:4])

deaths2 <- reshape2::melt(Deaths, id.vars = colnames(Deaths)[1:4])

recovered2 <-  reshape2::melt(Recovered, id.vars = colnames(Recovered)[1:4])


mapPanel <- tabPanel("map",
                     leafletOutput('map', width = '100%', height = '300px'),
                     DT::DTOutput("esTable")
)


dataPanel <- tabPanel("Data", 
                      tableOutput("dataTable")
)


ui <- navbarPage("Shiny app",
                 dataPanel,
                 mapPanel,
                 inverse=TRUE
                 
)





server <- function(input, output){
  
  output$dataTable <- renderTable({
    head(report)
  })
  
  output$map <- renderLeaflet({
    
    report %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~Long, ~Lat, popup = ~as.character(`Country/Region`), label = ~as.character(`Country/Region`))

  })
  
  output$esTable <- DT::renderDT(head(report[order(report$Confirmed, decreasing = TRUE),]))
 
  
}


shinyApp(ui = ui, server = server)