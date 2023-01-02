#Author: Mauricio Moreno
#Using Tick data from summer research 2022

library(stringr)
library(tidytext)
library(tidyverse)
library(data.table)
library(rvest)
library(shiny)
library(readxl)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gapminder)
library(gifski)
library(readr)
library(leaflet)
library(geojsonio)
library(rjson)
library(shiny)
library(plotly)
library(transformr)
library(ggiraph)
library(ggthemes)
library(gapminder)
library(htmltools)
library(cat)
library(sf)
library(usmap)
library(rgdal)
library(spdplyr)
library(rmapshaper)
##################
#THIS IS WHERE YOU UPDATE THE DATASET NAME
#EITHER CHANGE THE DOWNLOADED DATASET TO "updated_dated.xlsx"
#OR CHANGE THE FILE NAME BELOW TO MATCH WHATEVER ITS CALLED IN THE FOLDER

#first input is file name, followed by specific sheet name
tick.data1 <- read_xlsx("Data/updated_data.xlsx", "Samples")[,c(1:3,7)]
tick.data2 <- read_xlsx("Data/updated_data.xlsx", "Ticks")[,c(1,15)]

##################

#rename col 1
colnames(tick.data2)[1] <- "SampleID"

#merge tables by col1 matching IDs
tick.data <- merge(tick.data1, tick.data2, by = "SampleID", all = TRUE)

#load in the county map data for vermont from the project folder
vt.map <- geojson_read("Data/county.vt.geojson",
                       what = "sp")

us.map <- geojson_read("Data/states.geojson",
                       what = "sp")

#load in Lyme disease cases at county level and show only VT
county_lyme_cases <- read.csv("Data/County_LD.csv")
state_cases <- read.csv("Data/state_LD.csv")
state_cases <- state_cases[c(1:51),c(22:42)]

#change col names
colnames(state_cases) <- c("State", "2000", "2001", "2002", "2003", "2004", 
                           "2005", "2006", "2007", "2008", "2009", "2010", 
                           "2011", "2012", "2013", "2014", "2015", "2016", 
                           "2017", "2018", "2019")

#filter for VT
county_lyme_cases <- county_lyme_cases %>%
  filter(Stname == "Vermont")

county_lyme_cases <- county_lyme_cases %>%
  mutate(Name = word(county_lyme_cases$Ctyname, 1))

#reorder columns, remove last row, and save as new file
vt.county.data <- county_lyme_cases[c(-15),c(25,5:24)]

#rewrite columns' names as just year number
colnames(vt.county.data)[2:21] <- c("2000", "2001", "2002",
                                    "2003", "2004", "2005",
                                    "2006", "2007", "2008",
                                    "2009", "2010", "2011",
                                    "2012", "2013", "2014",
                                    "2015", "2016", "2017",
                                    "2018", "2019")

#creating spatial objects
Site <- c("BRF", "BRF2", "Chipman", "Crystal", "Foote", "Frost", "Gilmore", "Gorge", 
          "Gorham", "Jackson", "Lourie", "Major", "Snowbowl", "SPIN", "UpperChipman")

lat <- c("44.031376", "44.025673", "44.033798", "43.945349", "44.012664", "43.963755", "43.960453", "43.972382", 
         "43.995429", "43.997286", "44.070652", "44.023783", "43.935062", "43.960009", "44.024308")

lng <- c("-73.08204868", "-73.08024861", "-73.16089218", "-72.96749622", "-73.1370604", "-73.00340337", "-72.97947038", "-73.07380099", 
         "-73.17455841", "-73.201088", "-73.26054985", "-73.26128495", "-72.95108946", "-73.0287938", "-73.16255824")

site.locations <- data.frame(Site, lat, lng)

addison.map <- vt.map %>%
  filter(NAME == "Addison")

tick.counts <- tick.data[,-1]
tick.counts[,1] <- substr(tick.counts$Date, 1, 4)
colnames(tick.counts)[4] <- "BbResult"

tick.counts <- tick.counts %>%
  mutate(positive = ifelse(BbResult == "N", 0, 1)) 

tick.counts$Date <- as.numeric(tick.counts$Date)

temp.ticks <- tick.counts

temp.ticks["Site"][temp.ticks["Site"] == "Chipman2"] <- "Chipman"

state_pop <- read_xlsx("Data/statepop.xlsx")
state_pop$State <- sub(".", "", state_pop$State)

colnames(temp.ticks)[3] <- "Temp"

temp.ticks$Temp <- as.numeric(temp.ticks$Temp)

ticknumbers <- temp.ticks %>%
  group_by(Date, Site)  %>%
  summarise(TickCounts = n())

avg.temp <- na.omit(temp.ticks) %>%
  group_by(Date, Site) %>%
  summarise(temp.avg = mean(Temp))
   
SiteTempTicks <- inner_join(ticknumbers, avg.temp, by = c("Site", "Date"))

server <- function(input, output) {

output$VTheatmap <- renderLeaflet({
  
  #save as new name, keeping only name and selected reactive year
  selected.vt.county.data <- vt.county.data[, c("Name", input$year), drop = FALSE]
  
  require(sp)
  
  #rename column name
  colnames(selected.vt.county.data)[2] <- "Cases"
  
  #create color palette based on bins that match range of observed values
  bins1 <- c(0, 25, 50, 75, 100, 150, 200, 300)
  colors1 <- colorBin(palette = "YlOrRd",
                      bins = bins1,
                      domain = selected.vt.county.data$Cases)
  
  #create a county name popup for viewers
  county_popup <- paste0(vt.map$NAME,
                         " county had ",
                         selected.vt.county.data$Cases,
                         " cases in ",
                         input$year,
                         ".")
  
  #creating map and adding some *flare*
  vt.map %>%
    leaflet() %>%
    addTiles()  %>%
    addPolygons(fillColor = colors1(selected.vt.county.data$Cases),
                layerId = selected.vt.county.data$Cases,
                label = county_popup,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                weight = 2,
                color = "white",
                dashArray = "3",
                opacity = 1,
                fillOpacity = .7)%>%
    setView(-72.8, 43.8, 6.5) %>%
    addLegend(pal = colors1,
              values = selected.vt.county.data$Cases,
              opacity = 1)
  
}
)

output$Addisonheatmap <- renderLeaflet({
  
  require(sp)
  
  #filter by selected reactive date
  selected.year <- tick.counts %>%
    filter(Date == input$Year)
  
  #drop some columns
  selected.year <- selected.year[,-c(1,3,4)]  
  
  #rename one of the sites
  selected.year["Site"][selected.year["Site"] == "Chipman2"] <- "Chipman"

  #create new dataset based on selected year and show site column
  selected.year <- arrange(selected.year, Site)
  
  site.agg.data <- selected.year %>%
    count(Site, sort = TRUE) %>%
    arrange(Site)
  
  positive.rate <- na.omit(selected.year) %>%
    group_by(Site, positive) %>%
    summarise(n=n()) %>%
    mutate(Freq = n/sum(n),
           count = sum(n))
  
  positive.rate <- positive.rate[positive.rate$positive == "1",]
  positive.rate <- positive.rate[,c(1,4,5)]
  
  positive.agg <- merge(positive.rate, site.agg.data, by = "Site")
  
  total.tick.data <- merge(positive.agg, site.locations, by = "Site")
  
  df.SP <- st_as_sf(total.tick.data, coords = c("lng", "lat"), crs = 4326)
  df.SP$Freq <- substr(df.SP$Freq, 0, 5)
  
  bins2 <- c(0, 0.1, 0.25, 0.4, 1)
  colors2 <- colorBin(palette = "YlOrRd",
                      bins = bins2,
                      domain = df.SP$Freq)
  
  site_popup <- paste0(df.SP$n,
                        " ticks and ",
                        df.SP$Freq,
                        " Borrelia (+) frequency n(",
                        positive.rate$count,
                         ").")
  
  #create map
  addison.map %>%
    leaflet() %>%
    addTiles()  %>%
    addPolygons(weight = 2,
                color = "white",
                dashArray = "3",
                opacity = 1,
                fillOpacity = .7)%>%
    addCircles(data = df.SP,
                     fillColor = colors2(total.tick.data$Freq),
                     fillOpacity = 1,
                     radius = 4*df.SP$n,
                     stroke = FALSE, 
                     label = df.SP$Site,
                     labelOptions = labelOptions(noHide = TRUE, offset=c(0,-12), 
                                                 textOnly = TRUE)) %>%
    addCircles(data = df.SP,
               fillOpacity = 0,
               radius = 4*df.SP$n,
               stroke = FALSE,
               label = site_popup) %>%
    setView(-73.1, 44, 10.5) %>%
    addLegend(pal = colors2,
              values = df.SP$Freq,
              opacity = 1)
  
  
  
})


output$USheatmap <- renderLeaflet({
  
  require(sp)
  
  #selected.vt.county.data <- vt.county.data[, c("Name", input$year), drop = FALSE]  
  #input$StateYear
  year_cases <- state_cases[, c("State", input$StateYear )]
  
  state_population <- state_pop[, c("State", input$StateYear)]
  
  colnames(year_cases)[2] <- "Cases"
  colnames(state_population)[2] <- "Pop"
  
  pop_cases <- merge(year_cases, state_population, by = "State")
  
  pop_cases <- pop_cases %>%
               mutate(divided100000 = Pop/100000)
  
  pop_cases <- pop_cases %>%
               mutate(per.100000 = pop_cases$Cases/pop_cases$divided100000)

  bins3 <- c(0, 1, 5, 20, 50, 100, 150, 250)
  colors3 <- colorBin(palette = "YlOrRd",
                      bins = bins3,
                      domain = pop_cases$per.100000)
  
  #create a county name popup for viewers
  state_popup <- paste0(pop_cases$State,
                         " state had ",
                         pop_cases$Cases,
                         " cases in ",
                         input$StateYear,
                         ".")
  #create map
  us.map %>%
    leaflet() %>%
    addTiles()  %>%
    addPolygons(fillColor = colors3(pop_cases$per.100000),
                layerId = pop_cases$per.100000,
                label = state_popup,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                weight = 2,
                color = "white",
                dashArray = "3",
                opacity = 1,
                fillOpacity = .7)%>%
    setView(-97, 43.8, 3) %>%
    addLegend(pal = colors3,
              values = pop_cases$per.100000,
              opacity = 1)
  
}
)

output$SiteChart <- renderPlot({
  
  SiteTempTicks$temp.avg <- as.numeric(substr(SiteTempTicks$temp.avg, 0, 5))
  
    ggplot((SiteTempTicks %>%
           filter(Site %in% input$Site)),
           aes(x = Date, y = TickCounts)) +
    geom_col(fill = "coral") +
    geom_text(aes(label = TickCounts), color = "coral", hjust = 0.5, vjust = -1) +
    geom_line(aes(x = Date, y = temp.avg*20), color = "black") +
    geom_point(aes(x = Date, y = temp.avg*20), color = "black") +
    geom_text(aes(x = Date, y = temp.avg*20, label = temp.avg), color = "black", hjust = 0, vjust = 2) +
    scale_y_continuous(name = "Total Ticks Captured",
                       sec.axis = sec_axis(~./20, name = "Average Temperature (?C)")) +
    labs(title = "Tick Counts vs. Temperature", x = "Year") +
    theme_economist()
}
)


output$textIntro<-renderUI(HTML(paste("Lyme disease is an intensifying public health concern as reported cases have more than doubled since 2004 (Rosenberg et al., 2018). There are an 
                             estimated 476,000 annual cases of Lyme disease in the US, with Vermont 
                             having the nation's second-highest incidence rate per capita (Kugeler et 
                             al., 2021). Given the rise of Lyme disease in the Northeast, understanding 
                             the ecological drivers of this tick-borne illness is crucial for decreasing 
                             infections.",
                             "In this app, we visualize national, state, and county trends, as well as 
                             seeing how abiotic factors may affect tick counts in a given year. Please 
                             feel free to play around in the other tabs to see some cool info about these 
                             ticks!",
                             sep="<br/>")))

output$textNational<-renderUI(HTML(paste("For this map, CDC data was taken for all states at the county 
                                         level and aggregated to get total state counts. This data is 
                                         only up until 2019 so it needs some updating. The darker the 
                                         state color, the higher the prevalence of lyme disease.")))

output$textVermont<-renderUI(HTML(paste("For this map, the data was filtered to just Vermont state. 
                                        Because historical counts and records are from just recently in 
                                        the past several years, we observe that the total counts by county 
                                        increase drastically. The darker the fill for the county, the higher 
                                        the case count.")))

output$textAddison<-renderUI(HTML(paste("This data used for this map comes from the aggregate Allen Lab data 
                                        from 2016 until now. By scrolling through the years, you can see which 
                                        sites were samples, how many ticks were collected, and the percentage 
                                        of collected ticks that tested positive for Borrelia. Sites with larger 
                                        radii means that more ticks were captured there, the darker the site 
                                        marker, the greater the frequency of positive results. No data was 
                                        collected in 2020 due to Covid.")))

output$textTemp<-renderUI(HTML(paste("The data used here is also aggregate data from the Allen Lab. 
                                     You may select any of our sample sites from the drop-down menu 
                                     to see trends between average temperature in a given year of 
                                     sampling and total ticks captured.")))

}


ui <- fluidPage(
  
  mainPanel(navlistPanel(
    tabPanel("About the App",
             br(),
             htmlOutput("textIntro")),
    
    tabPanel("Historical National Data by State",
             br(),
             htmlOutput("textNational"),
             sliderInput(inputId = "StateYear",
                         label = h3("Select a Year"),
                         width = "100%",
                         min = 2010, max = 2019, step = 1, value = 2019,
                         animate = TRUE, sep = ""),
             leafletOutput("USheatmap", width = "130%"),
             br()),
    
    tabPanel("Vermont State Heatmap by County",
             br(),
             htmlOutput("textVermont"),
             sliderInput(inputId = "year", 
                         label = h3("Select a Year"),
                         width = "100%",
                         min = 2000, max = 2019, step = 1, value = 2019,
                         animate = TRUE, sep = ""),
             leafletOutput("VTheatmap", width = "130%"),
             br()),
    
    tabPanel("Sample Sites in Addison County",
             br(),
             htmlOutput("textAddison"),
             sliderInput(inputId = "Year", 
                         label = h3("Select a Year"),
                         width = "100%",
                         min = 2016, max = 2022, step = 1, value = 2022,
                         animate = FALSE, sep = ""),
             leafletOutput("Addisonheatmap", width = "130%"),
             br()),
    
    tabPanel("Changing Temps and Resulting Tick Captures",
             br(),
             htmlOutput("textTemp"),
             selectInput(inputId = "Site", 
                         label = h3("Select a Site"),
                         choices = unique(SiteTempTicks$Site),
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL),
             plotOutput("SiteChart"),
             br())
    
    )
    
  )
  
)


shinyApp(ui = ui, server = server)

