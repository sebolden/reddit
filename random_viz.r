users <- data %>% 
  select(author,subreddit,score) %>%
  group_by(author,subreddit) %>% 
  summarise(weight=n(), mean_score=mean(score)) 


users %>% group_by(subreddit) %>% 
  summarise(avgweight=mean(weight), avgscore=mean(mean_score)) %>% 
  ggplot(aes(avgweight, avgscore)) +
  scale_x_log10() +
  scale_y_log10() +
#  geom_jitter() +
  geom_text(aes(label=subreddit),cex=3) 
























