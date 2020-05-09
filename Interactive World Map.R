#library("WHO")
library(ggplot2)
library(dplyr)
require(maps)
library(sf)
library(tmap)
library(tmaptools)
library(plotly)

#an excel sheet with the country vode and the values
life.exp <- read.csv("iso_3166_country_codes1.csv")       # Retrieve the data

world_map <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

#making columns joining into a vector
life.exp$CODE <- as.vector(life.exp$CODE)
world_map$CODE <- as.vector(world_map$CODE)

#Merge map 

life.exp.map <- left_join(life.exp, world_map, by = "CODE")


#plot the interactive map using plotly library

fig <- plot_ly(life.exp.map, type='choropleth', locations=life.exp.map$CODE, z=life.exp.map$value, text=life.exp.map$COUNTRY, colorscale='Rainbow',
               reversescale =F)
fig