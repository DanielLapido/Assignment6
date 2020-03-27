library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)
library(shinyjs)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinyWidgets)
library(leaflet)
library(shinythemes)
library(extrafont)
library(shinydashboard)
library("leaflet")


#########################################################################
#Data tyding
#########################################################################

Confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

Deaths <-  readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


# Recovered <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

report <- cbind(Confirmed[,c(1,2,3,4,ncol(Confirmed))], Deaths[,ncol(Deaths)])
report$`Province/State`[is.na(report$`Province/State`)]=""


names(report)[c(5,6)] <-c("Confirmed", "Deaths")


date = as.Date(names(Confirmed)[c(5:ncol(Confirmed))],  format = "%m/%e/%y")


conf2 <- reshape2::melt(Confirmed, id.vars = colnames(Confirmed)[1:4])


dates = as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y")
conf2$variable = dates


deaths2 <- reshape2::melt(Deaths, id.vars = colnames(Deaths)[1:4])


globalreport = cbind(conf2, deaths2$value)
names(globalreport)[c(5,6,7)] <-c("Date","Confirmed", "Deaths")
globalreport$`Country/Region`=as.factor(globalreport$`Country/Region`)


factors = levels(globalreport$`Country/Region`)


#########################################################################
#Navbar Panels
#########################################################################



headerRow1 <- div(id = "header",useShinyjs(),
                  box("Last update (YYYY/M/D):",dates[length(dates)], fill = TRUE, color="blue"),
)


mapPanel <- tabPanel("Latest data",
                     fluidRow(box(width = 12,
                                  infoBox("Total confirmed", sum(report$Confirmed), fill = TRUE, color="red"),
                                  infoBox("Total deaths", sum(report$Deaths), fill = TRUE, color="black")
                                  
                     )),
                     leafletOutput('map', width = '100%', height = '500px'),
                     headerRow1,
                     DT::DTOutput("dataTable")
)


evolPanel <- tabPanel("Evolution",
                      sliderInput("time", "date:",
                                  min(date),
                                  max(date),
                                  value = min(date),
                                  timeFormat="%m/%e/%y",
                                  step=3,
                                  animate=T),
                      leafletOutput('evomap', width = '100%', height = '500px'),
                      selectInput("country",
                                  "Select country:",
                                  multiple = FALSE,
                                  choices = factors,
                                  selected = "Spain"),
                      plotly::plotlyOutput("PlotlyData")
)


dataPanel <- tabPanel("Data Explorer",
                      selectInput("dates",
                                  "Select date:",
                                  multiple = TRUE,
                                  choices = dates,
                                  selected = dates[length(dates)]),
                      
                      DT::DTOutput("evolTable"),
)


ui <- navbarPage("COVID19",
                 theme = shinytheme("cerulean"),
                 mapPanel,
                 evolPanel,
                 dataPanel,
                 inverse=TRUE,
                 header = tagList(
                   useShinydashboard()
                 )
                 
)




#########################################################################
#Server
#########################################################################

server <- function(input, output){
  
  global_filtered <- reactive({
    req(input$country)
    globalreport %>%  filter (`Country/Region` %in% input$country)
  })
  
  output$dataTable <- DT::renderDT(report[order(report$Confirmed, decreasing = TRUE),])
  
  output$map <- renderLeaflet({
    
    report %>%
    filter(Confirmed > 0) %>%
    leaflet() %>%
    addTiles() %>%
    setView(25, 10, zoom = 2) %>%
    addCircleMarkers(
      ~Long,
      ~Lat,
      radius = ~  log(Confirmed, base =2),
      fillColor = "red",color = 'red',
      stroke = FALSE, fillOpacity = 0.5,
      popup = ~ paste("<font color=\"black\"><b>",toupper(`Country/Region`),"<br>",`Province/State`,"<br>","<font color=\"#484848\">", "Confirmed:","<font color=\"#FF0000\"><b>",Confirmed,"<br>","<font color=\"#484848\">", "<br>","Deaths:","<font color=\"black\"><b>",Deaths ),
      label = ~as.character(`Country/Region`))

  })
  
  
  points <- reactive({
    globalreport %>%
      filter(Date == input$time & Confirmed > 0)
  })

  
  output$evomap <- renderLeaflet({
    
    points() %>%
      leaflet() %>%
      addTiles() %>%
      setView(25, 10, zoom = 2) %>%
      addCircleMarkers(
        ~Long,
        ~Lat,
        radius = ~  log(Confirmed, base=2),
        fillColor = "red",color = 'red',
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~ paste("<font color=\"black\"><b>",toupper(`Country/Region`),"<br>",`Province/State`,"<br>","<font color=\"#484848\">", "Confirmed:","<font color=\"#FF0000\"><b>",Confirmed,"<br>","<font color=\"#484848\">", "<br>","Deaths:","<font color=\"black\"><b>",Deaths ),
        label = ~as.character(`Country/Region`))
    
  })
  
  output$PlotlyData = plotly::renderPlotly({ 
    
    p = global_filtered() %>%
      ggplot(aes(x=Date, y=Confirmed)) +
      geom_line(color="red") + ylab("Number of confirmed cases")+
      xlab("Date")+ theme_minimal()
    
    fig1 = ggplotly(p)
    
    
    r = global_filtered() %>%
      ggplot(aes(x=Date, y=Deaths)) +
      geom_line(color="black") + ylab("Number of deaths")+
      xlab("Date")+ theme_minimal()
    
    fig3=ggplotly(r)
    
    s = subplot(fig1,fig3)
    
    s %>% layout(annotations = list(
      list(x = 0.1 , y = 1.05, text = "Evolution of confirmed cases", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.9 , y = 1.05, text = "Evolution of number of deaths", showarrow = F, xref='paper', yref='paper'))
    )
    
    
  })
  
  data <- reactive({
    globalreport %>%
      filter(Date == input$dates)
  })
  
  output$evolTable <- DT::renderDT(data()[order(data()$Confirmed, decreasing = TRUE),])
  

}


shinyApp(ui = ui, server = server)