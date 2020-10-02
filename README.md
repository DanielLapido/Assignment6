## COVID 19 Shiny app 

https://daniellapido.github.io/Covid19_shinyapp/

The 2019–20 coronavirus pandemic is an ongoing pandemic of coronavirus disease 2019 (COVID-19) caused by the severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). The disease was first identified in Wuhan, Hubei, China in December 2019. On 11 March 2020, the World Health Organization declared the outbreak a pandemic.

**The data about recoveries stopped being updated. For that reason the app was splitted into the file 'app' which contains the data until 23rd of March, and the file 'newest_app' with the most recent data but without the number of recoveries**

The aim of this app was to display the worldwide evolution since the outbreak.
It was inspired on plenty of webpages about this topic which already exist. For example:

-  [Channel news asia](https://infographics.channelnewsasia.com/wuhan/gmap.html)
-  [Coronavirus COVID-19 Global Cases by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)](https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)

The data was gathered from the [Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)](https://systems.jhu.edu/research/public-health/ncov/). 

## Table of Contents:

- Latest information: Displays the total number of confirmed, recovered and deaths cases worldwide. It also allows to click on each country and a pop up will show the information for that country.

- Evolution of the virus: A temporal world map animation is implemented so that not only the latest information is available but also the daily information of the virus since the outbreak.


## How to run the app:

There are two ways of visualizing the app:

- The first one is by just clicking in my [shinyapps.io  link](https://daniellapidomartinez.shinyapps.io/Assignment6/)
- By running runGitHub("Covid19_shinyapp","DanielLapido") in R
