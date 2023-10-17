#day six - key variables
#libraries
library(dplyr)
library(ggplot2)
library(ggthemr)

#a good bit of this package may be written in FORTRAN
library(car) 

#optional
ggthemr("carrot")
#we have been working with TIDY data, the tidy philosophy has three axioms
#a. each variable has a column
#b. each observation has a row
#c. each value has a cell
#this is also "long data" more on this next week

#also - TIBBLES - are a special class of data frames that
#a. allow you to have more creative names
#b. throw better error messages
#c. print more coherently 
#you won't see that many tibbles in your travels unless you intentionally make them
#data.frame is far more common (this is why I use reflexively)

#at this point I think you have good control of the basic functions like writing a filter

#the more advanced tools are those that create new like MUTATE
#on Wednesday we looked at a formula about brides which did some nested logic, let's work there now

#to start with 
glimpse(weddings)

#we need to start thinking about the idea of the KEY variable 
#this is a special column of information, I think of it like a zipper

#here is a column
weddings$State

#some of our coolest things (like maps) will involve zippering two datasets together
#we can also use a key column to make things a little less annoying
#like a serial filter

#let's manually create a list of states that are MID
mid<-c("Florida", "Massachusetts", "Illinois")

#we can play with this in fun ways with a special pipe
weddings %>% 
  filter(State %in% mid)

#RESULT: 
weddings %>% 
  filter(State %in% mid)

#now lets do this the wild way
weddingsD<-weddingsC %>% 
  mutate(meh = if_else(State %in% mid, "gross", "baller"))

#and graphic that bad boi
weddingsD %>% 
  ggplot(aes(Total,fill=as.factor(Result)))+
  geom_density(alpha = 0.7) +
  facet_grid(~meh)

#ok that is legit interesting - florida/mass/illinois have weird stuff

#NOW SLOW DOWN the function

#first call and store
weddingsD<-weddingsC %>%
  #mutate and name
  mutate(meh = 
           #the logical test - is the STATE in MID
           if_else(State %in% mid,
                   #if YES the value of the cell...
                   "gross", 
                   #if NO the value of the cell
                   "baller"))


#what if we wanted to nest even more of these
baller<-c("California", "Arizona", "New York")

weddingsD<-weddingsC %>%
  #mutate and name
  mutate(meh = 
           #the logical test - is the STATE in MID
           if_else(State %in% mid,
                   #if YES the value of the cell...
                   "mid", 
                   #IF NO then we RUN A NEW TEST
                   if_else(State %in% baller,
                           #value if a baller
                          "baller",
                          #value if neither MID nor BALLER
                          "meh"
                          )))

#run it again
weddingsD %>% 
  ggplot(aes(Total,fill=as.factor(BudgetPerGues)))+
  geom_density(alpha = 0.7) +
  #OVERRIDE CARROT
  theme_excel() +
  scale_fill_excel()+
  facet_grid(~meh)

#window functions
weddingsD %>% 
  group_by(meh) %>% 
  #what do you want to know for the average
  summarize(mean(Age), mean(Budget))


weddingsE<-weddingsD %>% mutate(sugar = Age-Age.1)

#so what really matters
#time for the FANCY MATH!
#it is FRIDAY after all
fit<-aov(Total ~ Generation*sugar*Budget.1*State, data=weddingsE)
plot(fit)
summary(fit)

