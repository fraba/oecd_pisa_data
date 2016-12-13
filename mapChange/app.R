library(shiny)
library(leaflet)
library(RColorBrewer)

library(shiny)
library(ggplot2)
library(ggrepel)
library(DT)
library(htmltools)
library(scales)

# source('/home/ubuntu/oecd_pisa/oecd_pisa_data/map.R')
setwd('~/public_git/oecd_pisa_data')
source('map.R')

library(rgdal)
world_poly <- readOGR("shp", layer = "TM_WORLD_BORDERS-0.3", verbose = FALSE)

country_selection <- 
  subset(oecd_pisa_data, !duplicated(iso3c))$iso3c
names(country_selection) <-
  subset(oecd_pisa_data, !duplicated(iso3c))$Jurisdiction

world_poly <- subset(world_poly, world_poly$ISO3 %in% country_selection)
world_poly$ISO3 <- as.character(world_poly$ISO3)

# Analysis

## Annual change mean
require(dplyr)
require(zoo)
country_yr_change <- 
  oecd_pisa_data %>%
  dplyr::arrange(iso3c, Variable, Year) %>%
  dplyr::group_by(iso3c, Variable) %>%
  dplyr::mutate(year_diff = as.numeric(diff(zoo(of_mean), na.pad = TRUE))) %>%
  dplyr::group_by(iso3c, Variable) %>%
  dplyr::mutate(year_diff_mean_by_variable = mean(year_diff, na.rm = T)) %>%
  dplyr::group_by(iso3c) %>%
  dplyr::mutate(year_diff_mean_by_country = mean(year_diff, na.rm = T))

country_yr_mean <- 
  country_yr_change %>%
  dplyr::group_by(iso3c, Variable) %>%
  dplyr::summarize(mean = mean(year_diff_mean_by_variable*100, na.rm = T))

require(reshape2)
country_yr_mean_dcasted <- dcast(country_yr_mean, iso3c ~ Variable,
                                 value.var = 'mean')

world_poly <- merge(world_poly, country_yr_mean_dcasted, by.x = 'ISO3', by.y = 'iso3c')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 5, right = 15,
                selectizeInput('cnt',
                               NULL, 
                               country_selection),
                selectInput('var', NULL, 
                            choices = c(
                              'Mean difference (Mathematics)' = 'mathematics',
                              'Mean difference (Science)' = 'science',
                              'Mean difference (Reading)' = 'reading')),
                plotOutput('hist', height = '150px', width = '100%'))
)

server <- function(input, output, session) {
  
  react_poly <- reactive({
    world_poly$mean <-  world_poly[[input$var]]
    return(world_poly)
  })
  
  
  output$hist <- renderPlot({
    ggplot(subset(country_yr_change, 
                  iso3c == input$cnt & Variable == input$var), 
           aes(x = as.factor(Year), y = year_diff * 100)) +
      geom_bar(stat = 'identity') +
      geom_hline(yintercept = mean(subset(country_yr_change, iso3c == input$cnt & Variable == input$var)$year_diff * 100, na.rm = T)) +
      theme_bw() + 
      labs(x = NULL, y = NULL, caption = "Difference from previous test (percetage points)")
  })
  
  output$map <- renderLeaflet({
    
    require(viridis)
    
    prepNumber <- function(x) {
      x <- round(x, 2)
      if (x > 0) {return(paste0("+", as.character(x)))}
      else {return(as.character(x))}
    }
    
    minmax <- c(min(react_poly()$mean, na.rm = T), max(react_poly()$mean, nar. = T))
    abs_max_value <- minmax[which.max(abs(minmax))]
    
    pal <- colorNumeric(
      palette = rev(brewer.pal(9, "RdYlBu")),
      domain = c(-abs_max_value, abs_max_value)
    )
    
    leaflet(react_poly()) %>% 
      setView(lng = 0, lat = 40, zoom = 2) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.5, color = ~pal(mean),
        popup = ~paste0("<b>", NAME, 
                        "</b><br>Mathematics: ", prepNumber(mathematics), "<br>",
                        "Reading: ", prepNumber(reading), "<br>",
                        "Science: ", prepNumber(science), "<br>")) %>%
      addProviderTiles('CartoDB.DarkMatterNoLabels') %>%
      addLegend(pal = pal, values = ~mean, opacity = 1, position = 'bottomleft')
  })
}

shinyApp(ui, server)