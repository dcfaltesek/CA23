weddings<-weddings %>% 
  mutate(reference=1:dim(weddings)[1])

weddings_per_line<-weddings %>%
  unnest_tokens(word, Name)%>%
  count(reference, word, sort=TRUE) %>% 
  rename(per_line = n)


afinn<-get_sentiments("afinn")
#if there is a prompt, say yes

#for today we are NOT removing stopwords
#the dimensionality reduction of the AFINN join makes this redundant
#attach the scores
with_scores<-weddings_per_line%>%
  #THIS IS THE INNERJOIN I WAS YELLING ABOUT!
  inner_join(afinn, by="word")


scores_weddings<-with_scores%>%
  group_by(reference)%>%
  #notice our per line strategy is SUM
  summarize(line_value=sum(value), line_var=sd(value))

wedding_sentiments<-inner_join(weddings, scores_weddings, by="reference")

View(wedding_sentiments)

wedding_sentiments %>% ggplot(aes(reference, line_value, colour=State))+geom_jitter()
