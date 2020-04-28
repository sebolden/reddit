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

ggplot(x,aes(year, cluster, color=class)) +
  geom_jitter(alpha=.4) +
  #geom_label_repel(aes(label=cluster), size=3) +
  scale_color_manual(values=myc) 

x <- x %>% left_join(cnames,by="cluster")
x$name <- as.factor(x$name)

c10 <- x[x$cluster<11,]
c20 <- x[x$cluster<21 & x$cluster>10,]
c30 <- x[x$cluster<31 & x$cluster>20,]
c40 <- x[x$cluster<41 & x$cluster>30,]
c50 <- x[x$cluster>41,]
#colnames(x)

# did this for all 5 DFs to get the combined plot
ggplot(c10,aes(year, score, fill=subcol)) +
  geom_bar(position='dodge', stat='identity') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("subreddit group & cluster: score ranges over time", subtitle="clusters 1-10")
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
























































