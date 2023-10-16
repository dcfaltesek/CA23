#I want us to move ahead to a skill that is a little ahead of where we are on the sched
#joins
#what is a key variable? Write a note to yourself here...








#Bonus skill time, let's load a few things
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)


#this is a new one...
library(lubridate)
#it parses dates in a variety of easy to understand ways
#for now we just want a list of the years when our show was filmed

#here is how this code works 
weddingsF<-weddingsE %>% 
  #open the mutate to add a column called year
  mutate("Year" = 
           #say what kind of time data you want output
           year(
             #parse the date data which is already there 12/18/09
             mdy(Date))) %>% 
  #this is a personal favorite time format, which day of the year it is...
  mutate("day of year" = yday(mdy(Date))) 

#so now we can have some fun
weddingsF %>% 
  ggplot(aes(`day of year`, Total, colour=Budget))+geom_jitter()+scale_colour_distiller(palette = "Oranges")


#now we can do something else entirely
#what are the values of year in weddings
weddingsF %>% distinct(Year)

#here is the hardway to do the homework
#three vectors
Year<-c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,2010,2011,2012,2013,2014, 2015, 2016, 2016, 2017, 2018, 2019, 2020, 2021,2022,2023)
Pantone<-c("Cerulean", "Fuschia Rose", "Aqua Sky", "Tigerlilly", "Blue Turquoise", "Sand Dollar", "Chilli Pepper", "Blue Iris", "Mimosa", "Turqoise", "Honeysuckle", "Tangerine Tango", "Emerald", "Radiant Orchid", "Marasala", "Serenity", "Rose Quartz", "Greenery", "Ultra Violet", "Living Coral", "Classic Blue", "Ultimate Gray", "Illuminating", "Very Peri", "Viva Magenta")
Family<-c("Blue", "Red", "Blue", "Red", "Red","Blue", "Neutral", "Dark", "Dark", "Bright", "Green", "Red",  "Green", "Red", "Neural", "Pastel", "Pastel", "Green", "Dark", "Red", "Blue", "Neutral", "Bright", "Blue", "Red")

#merge those vectors into a dataframe
color_data<-data.frame(Year, Pantone, Family)
color_data

#ok, that makes sense, so lets join those up
inner_join(weddingsF, color_data)

#oh and lets store that
weddingsG<-inner_join(weddingsF, color_data)

weddingsG %>% 
  ggplot(aes(Pantone, Total, colour=Experience))+geom_jitter()+scale_colour_distiller(palette = "Greens")+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+facet_grid(~Family)


#now we are going to go BIG
#join your new DATASET homework to WEDDINGS by STATE








#at this point you can go ahead and hit a map
weddingsH<-weddingsG %>% 
  group_by(State) %>% 
  summarize("Fun Quotient" = mean(Experience, na.rm = TRUE))

states <- map_data("state")
head(states)
#these are just a bunch of points, almost like a map is just a bunch of polygons...

#we just run a basic ggplot but with the 
ggplot(states, aes(x = long, y = lat, fill = region, group = group)) + 
  geom_polygon(color = "white") + 
  coord_fixed(1.3) +
  #this takes of the list of 50 colors, the warning is that it is an old code
  #but it checks out
  guides(fill=FALSE)


#now let's join on our data
#what is the KEY VARAIBLE

#I will conform OUR data to the SHAPEFILEs
#run this THEN assign it as weddingsJ
weddingsJ<-weddingsH %>% 
  #the dat have to be really similar, so tolower
  mutate("region" = tolower(State))


#so this is a real winner eh?
inner_join(states, weddingsJ)


#don't run the theme if you don't want to party
ggthemr("grass")

#and reset if you need to
ggthemr_reset()


#here is the map
full_join(states, weddingsJ) %>% 
  ggplot(aes(x = long, y = lat, fill = `Fun Quotient`, group = group)) + 
  geom_polygon(color = "black") + 
  coord_fixed(1.3)

#another scale if you want +scale_fill_distiller(palette="Spectral", direction=1)

#this might explain why we THOUGHT we saw State var last time, Mass is BIG FUN, Florida is mid. 





