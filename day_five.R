#day five: continuous and discrete
library(ggplot2)
library(dplyr)
library(wesanderson)
library(ggthemes)
library(ggdark)
library(ggtech)
library(ggthemr)
library(munsell)
library(ggthemr)
#first things first, there is a missing value, the very last wedding does not have a state. The following is DESTRUCTIVE CODE

#WHAT STATE IS IT? LET'S FIGURE IT OUT
weddings[432,14] <- ""


#Let's store a simple discrete continuous from weddings
library(ggplot2)
library(colorspace)
library(viridis)
#notice, no geom is attached here...
W<-ggplot(weddings, aes(State, Budget, colour=Result))

#let's do this the wrong way...
W+geom_jitter()+theme(axis.text.x = element_text(angle = 60, hjust=1))

#ANSWER FOR YOURSELF: why is jitter wrong here?

#Let's do it the right way...
W+geom_violin()+theme(axis.text.x = element_text(angle = 60, hjust=1))

#Fixing labels
W+theme(axis.text = element_text(size = 14), axis.text.x = element_text(angle = 45),axis.text.y = element_text(angle = -45))+
  ggtitle("WOW!")


#Colors, first continuous colors
ggplot(weddings, aes(x = State, y = Experience, fill=Experience))+geom_bar(stat = "identity")+scale_fill_continuous_diverging()+theme(axis.text.x = element_text(angle = 60, hjust=1))

#let's make if fancy...
ggplot(weddings, aes(Age, Budget)) +
  geom_point(aes(colour = as.factor(Result)))+
  scale_colour_brewer(palette = "Purples")+facet_wrap(~State)

#some quick basic scales
RColorBrewer::display.brewer.all()

#and again...
ggplot(weddings, aes(Age, Budget, colour=as.factor(Result))) +
  geom_point()+
  scale_colour_brewer(palette = "Spectral")+facet_wrap(~State)

#and now with a fun viridis scale 
ggplot(weddings, aes(Budget, Experience, colour=as.factor(Result))) +
  geom_point(aes(color = as.factor(Result))) +
  geom_smooth(aes(color = as.factor(Result), fill = as.factor(Result)), method = "lm") + 
  #try switching the option, it does A-D
  scale_color_viridis(discrete = TRUE, option = "C")+
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  labs(colour = "Result")+
  labs(title = "Weddings are fun?")
  

#viridis base
barplot(1:20, col = viridis(20))

#so you are saying we could use a discrerte for state with viridis?
weddingsB %>% 
  ggplot(aes(Total, Budget, colour=State))+geom_point()+
  scale_color_viridis(discrete = TRUE, option = "C") + theme(legend.position = "top")

  


#but make it continous and beavery
ggplot(weddings, aes(Age, Result, colour=Budget)) +
  geom_point()+
  scale_colour_distiller(palette = "Oranges")+facet_wrap(~Age)

#and thus the curse of dimensionality...
weddings %>% 
  group_by(Bride.1) %>% 
  count() %>% 
  arrange(desc(n))

#the karens v. melissas?





#what about ages? are older brides more fun?
weddingsC<-weddingsB %>% 
  mutate(Generation = if_else(Age < 25, "Millenial", ifelse(Age >= 25 & Age <=40, "GenX", "Boomer")))

#now we play with generation...
weddingsC %>% 
  filter(Result != "NA") %>% 
  ggplot(aes(Total, Budget, colour=Generation,alpha = Experience, size=Dress, shape=as.factor(Budget.1)))+
  geom_jitter()+
  dark_mode(theme_solarized()) +
  scale_fill_solarized()+
  facet_grid(~Result)

#did that make any sense at all?  
  
  
#more examples for you to run at home...

#once again forcing the discrete...
ggplot(weddings, aes(x = Budget, fill = as.factor(Result))) +
  geom_histogram(binwidth = 100)

p<-ggplot(weddings, aes(x = Budget, fill = as.factor(Result))) +
  geom_histogram(position = "dodge", binwidth = 1000)

p+scale_fill_brewer(palette = "Oranges",direction = -1) +
  theme_dark()

#we need to get a handle on summary functions
#what if we wanted to count all the weddings in each state?

#first we need our handy data pliers...
library(dplyr)
#name the dataset
weddings %>% 
  #how should we group
  group_by(State) %>% 
  #our summary function
  count()

#ok and we can plot that
weddings %>% 
  #how should we group
  group_by(State) %>% 
  #our summary function
  count() %>% 
  ggplot(aes(State, n))+geom_jitter()




#what if we want to see the five most expensive weddings?
weddings %>% 
  slice_max(Budget, n=5)

#how about 20 random weddings
weddings %>% 
  slice_sample(n=20)

#the manipulate area is pretty clear 

#what if we just wanted to sort...
weddings %>% 
  arrange(desc(Episode))

#just a classic summary function 
weddings %>% 
  group_by(Age) %>% 
  summarize(Average_Spend=mean(Budget)) %>% 
  ggplot(aes(Age, Average_Spend))+geom_density_2d_filled()
