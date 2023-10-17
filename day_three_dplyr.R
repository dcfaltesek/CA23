#day three - intermediate basics
#before we get into the dplyr stuff, let's give a key skill a try - loading a dataset
library(dplyr)

#click on cocaine and then import

#quick views
head(cocaine)
glimpse(cocaine)

#don't worry if this doesn't make sense
cocaine %>% 
  ggplot(aes(potency, price, size=weight, colour=state))+geom_point()

#and another quicky
starwars

#but fo rour needs today, let's do flights to start...
library(nycflights13)
flights<-flights

#the VAST majority of the labor in any project will involve data collection and cleaning
#we are starting our big projects in week 7 so we have time to get the datasets together, the actual projects might only take a few hours...

#let's see some CORE functions
#I just want flights from JFK
flights %>% 
  filter(origin == "JFK")

#what if we want from JFK AND on United?
flights %>% 
  filter(origin == "JFK" & carrier == "UA")

#let's get really tricky - United OR Delta
#which is delta again?
nycflights13::airlines

#here we go - TWO PIPES!
flights %>% 
  filter(origin == "JFK") %>% 
  #what in the world is that?! its like an & but for OR
  filter(carrier == "UA" | carrier )
  
#now find your birthday and the most delayed flight...
#yes YOU CAN DO IT!
#write your code here



#STORE YOUR BIRTHDAY AS SOMETHING USING AN ARROW <-

#the most delayed ever
flights %>% 
  slice_max(arr_delay, n=10)

#here is another cool trick - you can arrange the data easily too

flights %>% 
  slice_max(arr_delay, n=10) %>% 
  arrange(desc(distance))


#here is a fancy one...
flights %>% 
  #group all the flights by each carrier
  group_by(carrier) %>% 
  #get the mean and ignore NA values (which in this case are cancelled flights)
  #I want the summary statistic to be called delay
  summarize(delay = mean(dep_delay, na.rm=TRUE)) %>% 
  #arrange that
  arrange(desc(delay))


#but why are things delayed?
flights %>% 
  ggplot(aes(time_hour, dep_delay, colour=distance))+geom_point()

#so we see some grend space
flights %>% 
  group_by(month) %>% 
  summarize(mean(dep_delay, na.rm=TRUE))


#let's start with some new data... the weddings dataset
#get that loaded...

#now take a look at it
head(weddings)

#let's do some basic grammar of graphics
#we define the dataset, then the aesthetics, then the geom
ggplot(weddings)

#THAT GRAPH PRODUCED: 

ggplot(weddings, aes(Budget, Guests, colour=State))

#THAT GRAPH PRODUCED:

ggplot(weddings, aes(Budget, Guests, colour=State))+geom_jitter()

#AND BECAUSE WE CAN...look at that, we are doing some MATH INSIDE THE FUNCTION
ggplot(weddings, aes(Budget, Guests, colour=State, size = Dress+Venue+Experience))+geom_jitter()

#Let's write a new one with a discrete X and Y axis
ggplot(weddings, aes(Result, Budget.1, colour=Experience))+geom_jitter()

#let's play around for a minute, which other GEOMS will be awesome here...

#Let's do the basic birthday plot...


