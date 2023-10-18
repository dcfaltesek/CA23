#all the weddings
library(dplyr)
library(lubridate)
library(readr)

#import code
weddings <- read_csv("weddings.csv")

#added the total column
weddingsB<-weddings %>% 
  mutate(Total = Experience+Food+Dress+Venue)

#use an if_else mutate to create a new column
weddingsC<-weddingsB %>% 
  mutate(Generation = if_else(Age < 25, "Millenial", ifelse(Age >= 25 & Age <=40, "GenX", "Boomer")))

#add our judgements about the states with a %in%

#create the vector - these are arbitrary choices
mid<-c("Florida", "Massachusetts", "Illinois")

#mutate it
weddingsD<-weddingsC %>% 
  mutate(meh = if_else(State %in% mid, "gross", "baller"))

#add a compted column
weddingsE<-weddingsD %>% mutate(sugar = Age-Age.1)

#parse and compute a new column
weddingsF<-weddingsE %>% 
  #open the mutate to add a column called year
  mutate("Year" = 
           #say what kind of time data you want output
           year(
             #parse the date data which is already there 12/18/09
             mdy(Date))) %>% 
  #this is a personal favorite time format, which day of the year it is...
  mutate("day of year" = yday(mdy(Date))) 

#create a synthetic dataframe and join it

#three vectors
Year<-c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,2010,2011,2012,2013,2014, 2015, 2016, 2016, 2017, 2018, 2019, 2020, 2021,2022,2023)
Pantone<-c("Cerulean", "Fuschia Rose", "Aqua Sky", "Tigerlilly", "Blue Turquoise", "Sand Dollar", "Chilli Pepper", "Blue Iris", "Mimosa", "Turqoise", "Honeysuckle", "Tangerine Tango", "Emerald", "Radiant Orchid", "Marasala", "Serenity", "Rose Quartz", "Greenery", "Ultra Violet", "Living Coral", "Classic Blue", "Ultimate Gray", "Illuminating", "Very Peri", "Viva Magenta")
Family<-c("Blue", "Red", "Blue", "Red", "Red","Blue", "Neutral", "Dark", "Dark", "Bright", "Green", "Red",  "Green", "Red", "Neural", "Pastel", "Pastel", "Green", "Dark", "Red", "Blue", "Neutral", "Bright", "Blue", "Red")

#merge those vectors into a dataframe
color_data<-data.frame(Year, Pantone, Family)

weddingsG<-inner_join(weddingsF, color_data)