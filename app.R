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
library(extrafont)
library(shinydashboard)


#########################################################################
#Data tyding
#########################################################################

Confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

Deaths <-  readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")


Recovered <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

report <- cbind(Confirmed[,c(1,2,3,4,ncol(Confirmed))], Deaths[,ncol(Deaths)], Recovered[,ncol(Recovered)] )
report$`Province/State`[is.na(report$`Province/State`)]=""


names(report)[c(5,6,7)] <-c("Confirmed", "Deaths", "Recovered")


date = as.Date(names(Confirmed)[c(5:ncol(Confirmed))],  format = "%m/%e/%y")


conf2 <- reshape2::melt(Confirmed, id.vars = colnames(Confirmed)[1:4])

# date = as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y")
# days = as.POSIXct(paste("0",as.character(unique(conf2$variable)),sep=""), format = "%m/%e/%y")
# 
dates = as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y")
conf2$variable = dates


deaths2 <- reshape2::melt(Deaths, id.vars = colnames(Deaths)[1:4])

recovered2 <-  reshape2::melt(Recovered, id.vars = colnames(Recovered)[1:4])

globalreport = cbind(conf2, deaths2$value, recovered2$value)
names(globalreport)[c(6,7,8)] <-c("Confirmed", "Deaths", "Recovered")



#########################################################################
#Navbar Panels
#########################################################################



headerRow1 <- div(id = "header",useShinyjs(),
                  box("Last update:",dates[length(dates)], fill = TRUE, color="olive")
)

# headerRow1 <- fluidRow(id = "header",useShinyjs(),
#                        valueBox(sum(report$Confirmed), "Confirmed", icon = icon("credit-card")),
#                        valueBox(sum(report$Deaths), "Deaths", icon = icon("credit-card")),
#                        valueBoxOutput("Recovered"),
# )

# headerRow = div(id = "header",useShinyjs(),
#                  selectInput("selYear",
#                              "Select the year:",
#                              multiple = TRUE,
#                              choices = gapminder_years,
#                              selected = head(gapminder_years,3)),
#                  selectInput("selCountries",
#                              "Select the country:",
#                              multiple = TRUE,
#                              choices = gapminder_countries,
#                              selected = head(gapminder_countries,3))
# )


mapPanel <- tabPanel("Interactive Map",
                     fluidRow(box(width = 12,
                                  infoBox("Confirmed", sum(report$Confirmed), fill = TRUE, color="red"),
                                  infoBox("Recovered", sum(report$Recovered), fill = TRUE, color="green"),
                                  infoBox("Deaths", sum(report$Deaths), fill = TRUE, color="black")
                                  
                     )),
                     leafletOutput('map', width = '100%', height = '500px'),
                     headerRow1,
                     DT::DTOutput("esTable")
)


dataPanel <- tabPanel("Data explorer", 
                      DT::DTOutput("dataTable"),

)

# evolPanel <- tabPanel("Evolution", 
#                       sliderInput("time", "date",
#                                   min(days), 
#                                   max(days),
#                                   value = max(days),
#                                   timeFormat="%m/%e/%y",
#                                   step=1,
#                                   animate=T),
#                       DT::DTOutput("evolTable")
#                       #leafletOutput('evomap', width = '100%', height = '500px')
# )

evolPanel <- tabPanel("Evolution",
                      sliderInput("time", "date:",
                                  min(date),
                                  max(date),
                                  value = min(date),
                                  timeFormat="%m/%e/%y",
                                  step=3,
                                  animate=T),
                      #DT::DTOutput("evolTable"),
                      leafletOutput('evomap', width = '100%', height = '500px')
)


ui <- navbarPage("COVID19",
                 mapPanel,
                 dataPanel,
                 evolPanel,
                 inverse=TRUE,
                 header = tagList(
                   useShinydashboard()
                 )
                 
)



#########################################################################
#Server
#########################################################################

server <- function(input, output){
  
  # observe({
  #   if (input$navBar == "Evolution") {
  #     shinyjs::show("header")
  #   } else {
  #     shinyjs::hide("header")
  #   }
  # })

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
      popup = ~ paste("<font color=\"black\"><b>",toupper(`Country/Region`),"<br>",`Province/State`,"<br>","<font color=\"#484848\">", "Confirmed:","<font color=\"#FF0000\"><b>",Confirmed,"<br>","<font color=\"#484848\">","Recovered:", "<font color=\"#00ff00\"><b>",Recovered,"<font color=\"#484848\">", "<br>","Deaths:","<font color=\"black\"><b>",Deaths ),
      label = ~as.character(`Country/Region`))

  })
  
  output$esTable <- DT::renderDT(report[order(report$Confirmed, decreasing = TRUE),])
  
  
  points <- reactive({
    globalreport %>%
      filter(variable == input$time & Confirmed > 0)
  })

  
  #output$evolTable = DT::renderDT(points())
  
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     addMarkers(data = points(),popup=as.character(points()$a))
  # })
  
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
        popup = ~ paste("<font color=\"black\"><b>",toupper(`Country/Region`),"<br>",`Province/State`,"<br>","<font color=\"#484848\">", "Confirmed:","<font color=\"#FF0000\"><b>",Confirmed,"<br>","<font color=\"#484848\">","Recovered:", "<font color=\"#00ff00\"><b>",Recovered,"<font color=\"#484848\">", "<br>","Deaths:","<font color=\"black\"><b>",Deaths ),
        label = ~as.character(`Country/Region`))
    
  })
  


}


shinyApp(ui = ui, server = server)