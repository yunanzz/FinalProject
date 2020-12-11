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
  
  titlePanel("Seeing Important Values from World Map"),
  
  tabsetPanel(
    tabPanel("Important Values from World Map",
  
    fluidRow(column=12,
           
           sidebarLayout(
             sidebarPanel(
               #variable
               radioButtons("variable", "Select a Measurement",
                            choices=c("Pageviews by country","Revenue(Log10) by country","Hits by country"),
                            selected = "Pageviews by country"),
               
               #country
               # selectizeInput("country", "Select up to 6 countries:", 
               #                choices = jhu_UID$Country_Region,
               #                multiple = TRUE,
               #                options = list(maxItems = 6)),
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
  
  tabPanel("Tendency Comparison",
          br(),
          selectInput("country", 
                      "Choose a country to compare with the United States:", 
                      linename),
          plotOutput(outputId = "lineplot")
  )))

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
  
  output$lineplot <- renderPlot({
    tranc_dat1 <- tr3 %>% filter(country==input$country)
    tranc_dat2 <- tranc_dat1 %>% group_by(visitNumber) %>% 
                      summarize(revenue = sum(value)) %>% 
                      ungroup()  
    
    tranc_dat2 %>%  ggplot(aes(x = visitNumber, y = revenue)) + 
      geom_line(color="steelblue") +
      theme_minimal() +
      #  scale_x_continuous(breaks=c(1,10, 50, 100,200))+
      #scale_color_discrete(name = "Country") +
      scale_y_continuous(labels = comma) +
      ggtitle("aaa")
    
    
    # dat %>%  ggplot(aes(year, life_expectancy, color = country)) + 
    #   geom_line(na.rm = TRUE) + xlab("Year") +
    #   ylab("Life Expectancy (Years)") +  
    #   scale_x_continuous(breaks = c(seq(1960, 2016, by=10), 2016), limits = c(1960, 2016)) + 
    #   scale_y_continuous(breaks = seq(10, 85, 5), limits = c(min(gapminder$life_expectancy), max(gapminder$life_expectancy))) +
    #   scale_color_discrete(name = "Country") +
    #   ggtitle("aaa")
  })
  
}


shinyApp(ui, server)