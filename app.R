library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)
library(shinyjs)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)

Confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

Deaths <-  readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")


Recovered <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")


report <- cbind(Confirmed[,c(1,2,3,4,ncol(Confirmed))], Deaths[,ncol(Deaths)], Recovered[,ncol(Recovered)] )

names(report)[c(5,6,7)] <-c("Confimed", "Deaths", "Recovered")



conf2 <- reshape2::melt(Confirmed, id.vars = colnames(Confirmed)[1:4])

deaths2 <- reshape2::melt(Deaths, id.vars = colnames(Deaths)[1:4])

recovered2 <-  reshape2::melt(recovered, id.vars = colnames(recovered)[1:4])


dataPanel <- tabPanel("Data", 
                      tableOutput("dataTable")
)


ui <- navbarPage("Shiny app",
                 dataPanel,
                 inverse=TRUE
                 
)







server <- function(input, output){
  
  output$dataTable <- renderTable({
    head(report)
  })
 
  
}


shinyApp(ui = ui, server = server)