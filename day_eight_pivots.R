#OK so I just gotta know, what do you think of the green ranger?
library(dplyr)
library(tidyr)

#go ahead an import dataset zords
zords

#Someone quick use the Dragon Dagger!

#ok can you make a GGPLOT OF IT? LIKE BY SEASON?


Seasons<-c("Dino", "Thunder", "Ninja","Shogun", "Zeo", "Aquatar", "SuperZeo")
Number<-c(1,2,3,4,5,6,7)
ranger_seasons<-data.frame(Seasons, Number)

#but this isn't easy at all
head(zords)

#that is because it is WIDE data, which is really nice for a human brain to look at
#computers need LONG data

zords %>% 
  #first step - we need to take everything EXCEPT the first column and make it LONGER
  pivot_longer(-Ranger)

#Well that was awesome, but it is missing some things, we need to get it to say the right stuff for name and value
zords %>% 
  pivot_longer(-Ranger, names_to="Seasons", values_to="Zord")

#well that was easy
library(dplyr)
long_rangers<-zords %>% 
  pivot_longer(-Ranger, names_to="Seasons", values_to="Zord") 

inner_join(long_rangers, ranger_seasons, by="Seasons")

#so now we can plot that nonsense...
library(ggplot2)
inner_join(long_rangers, ranger_seasons) %>% 
  ggplot(aes(Number, Zord, colour=Ranger))+geom_point()

#for your analysis
full_rangers<-inner_join(long_rangers, ranger_seasons)

#a bit more conceptual example
#lets add a bride number...
weddingsH<-weddingsG %>% 
  mutate(Number = 1:dim(weddingsG)[1])

weddingsI<-weddingsH %>% 
  filter(Season<3) %>% 
  select(Bride.1, Food, State) %>% 
  pivot_wider(names_from=-c(Bride.1, Food), values_from = Food, values_fn=mean)

View(weddingsI)  

#ok lets keep thinking 
weddingsJ<-weddingsH %>% 
  #get rid of the bride.1, no need to curse ourselves with names here...
  select(Food, State, Season) %>% 
  pivot_wider(names_from=-c(State, Food), values_from = Food, values_fn=mean)

View(weddingsJ) 


weddingsK<-weddingsH %>% 
  filter(State == "New York")

final_fit<-aov(Result ~ Food*Dress*Venue*Budget*Bride, data = weddingsK)
plot(final_fit)
summary(final_fit)
