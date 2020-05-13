library(ggplot2)
library(dplyr)
require(maps)
library(sf)
library(tmap)
library(tmaptools)
library(plotly)
library(tidyverse)
library(caTools)
library(ggthemes)
library(corrgram)
library(corrplot)

df <-read.csv("owid-covid-data.csv")
#df$location <- as.character(df$location)

df <- filter(df,location == 'Mauritius')  # filter only mauritius
df<-as.data.frame(df)

df <- ggplot(df, aes(x=date, y=total_cases)) +
  geom_point(aes(color=new_cases), size=8, alpha=0.7) +
  labs(title='COVID19 in Mauritius as at 09/05/2020 for Total cases vs New Cases',y = "Total Cases", x = "Date")+
  scale_color_continuous(trans = 'reverse')+ # reversing the legend label
  theme_bw()+
  scale_fill_manual(values=c('green','red')) #manually filling the color

print(df)


#df$location <- as.character(df$location)
df2 <-read.csv("owid-covid-data.csv")
df2 <- filter(df2,location == 'Mauritius')  # Keep data for 2015 and for both sex
df2<-as.data.frame(df2)

df2<-ggplot(df2, aes(x=date, y=new_cases)) +
  geom_point(aes(shape=location, color=new_deaths), size=8, alpha=0.7) +
  labs(title='COVID19 in Mauritius as at 09/05/2020 for New Cases vs New Deaths ',y = "Total Cases", x = "Date")+
  scale_color_continuous(trans = 'reverse')+
  theme_stata()
print(df2)


