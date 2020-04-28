library(tidyverse)
library(tm)
library(factoextra)
library(ggthemes)
library(viridis)
library(textmineR)
library(ggrepel)

x <- getcsv('subset_with_clusters.csv')

# PLOT --------------------------------------------------------------------
colnames(x)
#"score" "id"             "subreddit"      "quantile_group" "year"           "text"           "author"         "cluster"       
#[9] "top_words"      "co"             "class"         
myc <- c(  "red_high" = "#ff6361"
           , "red_low" = "tomato4"
           , "blue_high" = "#00abb6"
           , "blue_low" = "#003f5c"
           , "canon" = "#966f98")

df <- x[x$year != "canon",]
df <- df[df$year!="2014",]

c10 <- df[df$cluster<11,]
c20 <- df[df$cluster<21 & df$cluster>10,]
c30 <- df[df$cluster<31 & df$cluster>20,]
c40 <- df[df$cluster<41 & df$cluster>30,]
c50 <- df[df$cluster>41,]
#colnames(x)
sub_score <- c10 %>% group_by(subreddit,year,score,cluster,top_words,class) %>% summarise(avg.score = mean(score))
colnames(sub_score)

sub_score <- sub_score[order(sub_score$avg.score), ]  # sort

# did this for all 5 DFs to get the combined plot
ggplot(sub_score,aes(class, avg.score, fill=class)) +
  geom_bar(position='dodge',stat='identity') +
  facet_wrap(~top_words,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("subreddit group & cluster: score ranges over time", subtitle="clusters 1-10")





ggplot(c10, aes(x=year, y=score, fill=class)) + 
  geom_bar(stat="identity", width=.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

rm(c10,c20,c30,c40,c50)


xwt$quantile_group[xwt$quantile_group=="not_applicable"] <- "canon"
c10 <- xwt[xwt$cluster<11,]
c20 <- xwt[xwt$cluster<21 & xwt$cluster>10,]
c30 <- xwt[xwt$cluster<31 & xwt$cluster>20,]
c40 <- xwt[xwt$cluster<41 & xwt$cluster>30,]
c50 <- xwt[xwt$cluster>41,]

ggplot(c10,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 1-10")

ggplot(c20,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 11-20")


ggplot(c30,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 21-30")


ggplot(c40,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 31-40")

ggplot(c50,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 41-50")
























































