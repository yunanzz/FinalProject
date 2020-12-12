# library import
library(shiny)
library(zoo)
library(Rmisc)
library(scales)
library(ggalluvial)
library(jsonlite)
library(lubridate)
library(countrycode)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(operator.tools)

#data wrangling
set.seed(0)
tr1 <- read_csv("train.csv")

tr1 %>% select(fullVisitorId, channelGrouping, date, 
               sessionId, socialEngagementType, visitId, 
               visitNumber, visitStartTime) %>% 
  map_dfr(n_distinct) %>% 
  gather() 

flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)
parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)

tr2 <- parse(tr1)
tr2 %<>% select(-one_of("campaignCode"))
fea_uniq_values <- sapply(tr2, n_distinct)
(fea_del <- names(fea_uniq_values[fea_uniq_values == 1]))
tr2 %<>% select(-one_of(fea_del))
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")
tr2 %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))

# tr2 %>% summarise_all(funs(sum(is.na(.))/n()*100)) %>% 
#   gather(key="feature", value="missing_pct")


tr2 %<>%
  mutate(date = ymd(date),
         hits = as.integer(hits),
         pageviews = as.integer(pageviews),
         bounces = as.integer(bounces),
         newVisits = as.integer(newVisits),
         transactionRevenue = as.numeric(transactionRevenue))

y2 <- tr2$transactionRevenue
tr2$transactionRevenue <- NULL
y2[is.na(y2)] <- 0

tr3 <- tr2 %>% bind_cols(as_tibble(y2))

world_map = map_data("world")

map_tr2 <- tr3
linename <- unique(map_tr2$country)
linename <- linename[-c(54, 76, 120, 161, 196, 220, 222)]
linename

country_key = data.frame(rbind(c("St. Kitts & Nevis", "Nevis"), 
                               c("Trinidad & Tobago", "Trinidad"), 
                               c("Turks & Caicos Islands", "Turks and Caicos Islands"), 
                               c("Myanmar (Burma)", "Myanmar"), 
                               c("Macedonia (FYROM)", "Macedonia"), 
                               c("Bosnia & Herzegovina", "Bosnia and Herzegovina"), 
                               c("Czechia", "Czech Republic"), 
                               c("United Kingdom", "UK"), 
                               c("United States", "USA")))
names(country_key) = c("map_tr", "world_map")

names(world_map) <- c("long", "lat", "group", "order", "country", "subregion")



# Define UI for application that draws a histogram
# Define UI
ui <- fluidPage(
  
  titlePanel("Interactive Shinny App on Customer Revenue"),
  
  tabsetPanel(
    tabPanel("Important Values from World Map",
  
    fluidRow(column=12,
           
           sidebarLayout(
             sidebarPanel(
               # Add some text and a couple of hyper links before the slider for year
               p("The map plots generated here are using data from",
                a("Google Analytics Customer Revenue Prediction", 
                  href="https://www.kaggle.com/c/ga-customer-revenue-prediction")),

              p("Marketing teams are challenged to make investments 
                 in promotional strategies. Thus, the world-wide distribution about several
                variables are important to them."),
              
              p("You can choose 3 different measurements (Pageviews/Revenue/Hits) to see their 
                distribution around the world on one selected day."),
              
               # Add some space between the text above and animated
               # slider bar below
               br(),
               
               
               # Input: 
               
               #variable
               radioButtons("variable", "Select a Measurement",
                            choices=c("Pageviews by country","Revenue(Log10) by country","Hits by country"),
                            selected = "Pageviews by country"),
               
               #date
               dateInput("date", "Select date:",
                         min = "2016-09-02",
                         max = "2017-08-01",
                         value = "2016-09-02")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("mapPlot")
             )
           )
  )),
  
  tabPanel("Revenue v.s. Users' Visit Number",
           sidebarLayout(
             sidebarPanel(
           # Add some text and a couple of hyper links before the slider for year
           p("The 80/20 rule has proven true for many businesses – 
             only a small percentage of customers produce most of the revenue. 
             As such, marketing teams are challenged to make appropriate investments 
             in promotional strategies."),
            
           p("Now we focus on the total transction revenue certain visit-number-users had made to 
             see the relationship between revenue v.s. users' visit number in different countries."),
           
           # Add some space between the text above and animated
           # slider bar below
           br(),
           
           # Input: 
          
          selectInput("country", 
                      "Choose a country to see its total revenue v.s. users' visiting number:", 
                      linename)),
          mainPanel(
          plotOutput(outputId = "lineplot"))
  ))))

# Define server
server <- function(input, output) {
  output$mapPlot <- renderPlot({
    
    mapdat1 <- map_tr2 %>% filter(date==input$date)  
    
    levelkey <- as.character(country_key$world_map)
    names(levelkey) <- country_key$map_tr
    mapdat1$country <- recode(mapdat1$country, !!!levelkey)
    
    a1 <- setdiff(unique(mapdat1$country), unique(world_map$country))
    mapdat2=mapdat1[mapdat1$country %!in% a1,]
    
    date_string <- as.character(input$date)

    if (input$variable=="Pageviews by country"){
    mapdat3 <- mapdat2 %>% group_by(country) %>% 
      summarise(pageviews = sum(pageviews)) %>% 
      ungroup()
    
    mapdat4 <- mapdat3 %>%
      right_join(world_map, by = "country")
    
    map1 <- ggplot(mapdat4, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = "pageviews"), color= "black") +
      scale_fill_viridis_c(name = "Pageviews \n (log10 scale)", 
                           option = "magma", trans="log10", direction = -1, 
                           na.value="grey90") +
      theme(panel.grid.major = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      ggtitle(paste("Pageviews on", date_string))
    
    map1}else if (input$variable=="Revenue(Log10) by country"){
      mapdat7 <- mapdat2  %>% 
        group_by(country) %>% 
        summarise(revenue = sum(value)) %>% 
        ungroup()
      
      mapdat7$revenue[mapdat7$revenue == 0] <- 1
      
      mapdat8 <- mapdat7 %>%
        right_join(world_map, by = "country")
      
      map3 <- ggplot(mapdat8, aes(x = long, y = lat, group = group)) + geom_polygon(aes_string(fill = "revenue"), color= "black") +
        scale_fill_viridis_c(name = "Revenue \n (log10 scale)", option = "magma", trans="log10", direction = -1, na.value="grey90") +
        theme(panel.grid.major = element_blank(),
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        ggtitle(paste("Revenue(log10 scale) on", date_string)) 
      
      map3
      
    }else{
      mapdat5 <- mapdat2 %>% group_by(country) %>% 
        summarise(hits = sum(hits)) %>% 
        ungroup()
      
      mapdat6 <- mapdat5 %>%
        right_join(world_map, by = "country")
      
      map2 <- ggplot(mapdat6, aes(x = long, y = lat, group = group)) + geom_polygon(aes_string(fill = "hits"), color= "black") +
        scale_fill_viridis_c(name = "Hits \n (log10 scale)", option = "magma", trans="log10", direction = -1, na.value="grey90") +
        theme(panel.grid.major = element_blank(),
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        ggtitle(paste("Hits on", date_string)) 
      
      map2
    }
    
  })
  
  #country_string <- input$country
  
  output$lineplot <- renderPlot({
    country_string <- input$country
    
    tranc_dat1 <- tr3 %>% filter(country==input$country)
    tranc_dat2 <- tranc_dat1 %>% group_by(visitNumber) %>% 
                      summarize(revenue = sum(value)) %>% 
                      ungroup()  
    
    tranc_dat2 %>%  ggplot(aes(x = visitNumber, y = revenue)) + 
      geom_line(color="steelblue") +
      theme_minimal() +
      #scale_x_continuous(breaks=c(1,10, 50, 100,200), limits = c(1, 200))+
      scale_x_continuous(limits = c(1, 100)) +
      #scale_color_discrete(name = "Country") +
      scale_y_continuous(labels = comma) +
      xlab("Visit Number") +
      ylab("Revenue") +
      ggtitle(paste("Revenue v.s. Users' Visit Number in", country_string))
   
  })
  
}


shinyApp(ui, server)