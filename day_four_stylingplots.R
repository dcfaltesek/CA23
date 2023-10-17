#now how do we get this to be more stylish - day 4
library(ggplot2)
library(dplyr)
library(wesanderson)
library(ggthemes)
library(ggdark)
library(ggtech)
library(ggthemr)
library(munsell)
library(ggthemr)

#let's store a basic plot with weddings so we can play with styles more
#this one should get us some fun continuous variables
w<-ggplot(weddings, aes(Budget, Experience, colour=Food))

#now we can play with some geoms
w + geom_jitter()

#this is continuous bi-variate
w + geom_density_2d()
w+geom_smooth()

#and a quick discrete
#quick note - the as.factor is important, more on that Wednesday
x<-ggplot(weddings, aes(as.factor(Result), Experience, colour=Budget))
x+geom_violin()
x+geom_boxplot()

#woah - that outlier for winners, who is that? What is the story?
#write some exploratory dplyr code here...



#lets try a few custom color schemes
#here is a wesanderson continuous
weddings %>%
  ggplot(aes(Budget, Experience, colour=Dress)) + geom_jitter() +scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"))

#more about what this means next time...
weddings %>%
  ggplot(aes(Budget, Experience, colour=as.factor(Result))) + geom_jitter() +scale_color_manual(values = wes_palette("GrandBudapest2", type = "discrete"))


#ok lets rock and roll manually
weddings %>% 
  ggplot(aes(Venue, Dress, colour=Budget))+geom_jitter()

#that is way too dark... what if we manually assigned the colours?
weddings %>% 
  ggplot(aes(Venue, Dress, colour=Budget))+geom_jitter()+scale_color_gradient(low="red", high="yellow")

#getting better, but we really want to see the high ones
weddings %>% 
  ggplot(aes(Venue, Dress, colour=Budget))+geom_jitter()+
  #play with the midpoint and color values...
  scale_color_gradient2(low="red", mid = "orange", high="yellow", midpoint=50000)


#we might need to learn a new skill, let's add a column to our data
weddingsB<-weddings %>% 
  mutate(Total = Experience+Food+Dress+Venue)




#ok, here comes some fun, TWO geoms!
#but we need to see more chaos here...
weddingsB %>% 
  ggplot(aes(Total, Experience, colour=Result))+geom_smooth()+geom_point()


#that clears it up a bit
weddingsB %>% 
  ggplot(aes(Total, Experience, colour=as.factor(Result)))+geom_point()

#we might want this to be tabular for our human brains
weddingsB %>% 
  group_by(Result) %>% 
  summarize(mean(Total), sd(Total), sd(Venue))


#lets make things more colorful 
weddingsB %>% 
  ggplot(aes())


fit<-aov(Total ~ Result*Budget, data = weddingsB)
summary(fit)

#let's make it pretty
weddingsB %>% ggplot(aes(Total, fill=as.factor(Result)))+
  geom_density(alpha = .5)+
  dark_mode(theme_solarized()) +
  scale_fill_solarized() +
  theme(legend.position = "top")+
  ggtitle("What score does each place get?")

#what is the deal with third place?
#shall we investigate?
#let's come up with a scheme to help

#how about some data?
weddingsB %>% 
  group_by(Result) %>% 
  summarize(mean(Experience), sd(Experience), mean(Total), sd(Total))

#lets try a new function
weddingsB %>% 
  filter(Result ==3) %>% 
  count(Total) %>% 
  print(n=23)

#back to styling
#real nice basic plot to store for experiments
ww<-weddingsB %>% 
  ggplot(aes(Total, Budget, colour=Experience))+geom_jitter()

#take a look at a few of these...from ggthemes
ww+theme_foundation()
ww+theme_airbnb_fancy()
ww+theme_solarized()
#for more just type in theme_ and a bunch of options will appear

#there is another great styling package called ggthemr
#with this package you set from a global command
ggthemr("grape")

#now just run our base command
ww

#the power of these global commands is that it can save you a lot of time
ggthemr("flat dark")
ww

#to reset
ggthemr_reset()

#and we are back to normal
qplot(weddingsB$Total)

#one more idea for today, faceting
