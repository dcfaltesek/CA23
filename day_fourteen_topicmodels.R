library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tm)
library(stringr)

#to start we will use the classic AP stories example
#this is an antique
data("AssociatedPress")

#this object is a DTM
AssociatedPress


#this is our core mechanic - we pass our DTM to LDA; K is number of topics.
#this is an antique example designed for a two topic spread
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

#cross-check and verify
ap_lda

#did we pick the right number of topics
#perplexity should start high, then drop, then often increase again at the end
perplexity(ap_lda)


#and what are those
#these are the words which represent each topic
terms(ap_lda, k=5)


#but which words...
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#group by TOPIC and get the TOP TEN
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#now make a plot of that
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#this cute but largely useless for in situ models
#this will give us a better look into the differences
beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#and a visual on that
beta_wide %>% 
  ggplot(aes(topic1, topic2, colour=log_ratio))+geom_text(aes(label=term))

#OK here is the problem with tidytext, they don't rep that this is what we all wanted
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#and this is the real deep key - NOT IN THE TEXTBOOK
ap_wide<-ap_documents %>%
  pivot_wider(id_cols=document, 
              names_from = topic,
              values_from = c(gamma))

#would you look at that...
View(ap_wide)

#so we don't get too frustrated lets rename the columns
colnames(ap_wide)[2]<-"A"
colnames(ap_wide)[3]<-"B"

#and there is your visual
ap_wide %>% 
  ggplot(aes(A,B, colour=document))+geom_point()


#now here is the next key step...
#we need to convert 


#import with a different name...

#first we will just combined the lyric lines back into a single object
songsB<-taylor_songs %>% 
  group_by(track_name) %>% 
  select(c(album_name,track_name,song_lyric))

#this is quanteda code that we are using as a loader
#NEW SKILL - CALLING FUNCTIONS FRMO AN UNATTACHED PACKAGE
song_corpus<-quanteda::corpus(songsB$song_lyric, docvars = data.frame(song=songsB$track_name, album=songsB$album_name))


#now we can go ahead and call the object, read the results carefully
song_corpus

#this is a CORPUS object, so it is the texts with some metadata
#in this case we associated the TRACK and ALBUM names as those "DOCVARS"



#Let's use the songs then and model those
#quanteda can allow export to many distinct methods

#three distinct steps
#corpus to DFM
song_dfm<-quanteda::dfm(song_corpus, verbose=FALSE)
#tidy it
D <-tidy(song_dfm)

#really simple stopwords
get_stopwords()

more<-data.frame(word=c("(",")",",","'","?","like", "\\"))
D<-D %>% 
  rename("word"="term") %>% 
  anti_join(get_stopwords()) %>% 
  anti_join(more) %>% 
  rename("term"="word")

#you jsut want to be sure that whatever pre-processing you do, the rename to TERM goes LAST  


#output as a DTM
song_dtm<-D%>%
  cast_dtm(document, term, count)


#now what...

#right back to our method, let's go 7 topics though because 9 albums
song_lda <- LDA(song_dtm, k = 7, control = list(seed = 1234))
tm_documents <- tidy(song_lda, matrix = "gamma")


#from here on it gets into special dan territory 
tm_wide<-tm_documents %>%
  pivot_wider(id_cols=document, 
              names_from = topic,
              values_from = c(gamma))

#and now that we have done the hard part, here is where we go big
#now we need to back-connect our data, so we are going to do some stuff here with STRINGR
#if you get gud with regex, good job
tm_songs<-tm_wide %>% 
  #there is a ton of stuff happening in this line
  #it is a mutate, so it makes a new line
  #the result is named what the reference column was in the original dataset
  #this line is a pre-processor
  mutate(six_id=as.double(str_replace_all(document, "text", ""))) %>% 
  #this is the business end line of the cluster
  inner_join(taylor_songs)
View(tm_songs)

#this next vector should be equal to the number of topics 
A<-1:7
#that is not quite graphable, so now this
tm_processed<-tm_songs %>% 
  #let's just drop the text stuff
  pivot_longer(cols = -c(six_id,document, album_name, track_name, song_lyric))

#take a look, its really sort of ugly in long format but let's rock and roll
View(tm_processed)

#visual time
#this is a version of a confusion matrix
tm_processed %>% 
  #drop low scores, use a high threshold here
  filter(value > .95) %>% 
  ggplot(aes(name,album_name,colour=album_name))+geom_point()

#this implementation of LDA did NOT perform well

#and so we know
terms(song_lda, k=7)

