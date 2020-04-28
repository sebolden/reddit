# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(igraph)
library(viridis)
library(ggrepel)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)

# DATA --------------------------------------------------------------------
data <- makeBabyDF(getcsv('trp_nw_full.csv'))
linx <- getLinks(data)
fname <- unique(linx$subreddit)

# MODULARITY --------------------------------------------------------------
mdf15 <- modByYear(2015)
mdf16 <- modByYear(2016)
mdf17 <- modByYear(2017)
mdf18 <- modByYear(2018)
mdf19 <- modByYear(2019)
mdf20 <- modByYear(2020)

mdf <- rbind(mdf15, mdf16, mdf17, mdf18, mdf19, mdf20)
mdf <- na.omit(mdf)
rm(mdf15, mdf16, mdf17, mdf18, mdf19, mdf20)

myc <- getColors()

modDF <- modDF %>% rename(subreddit_group=co)
p <- ggplot(modDF, aes(x=rel_modularity, y=rep_modularity,group=subreddit)) +
    geom_abline(slope=1, intercept=0, color="tomato3",lwd=1) +
    geom_point(aes(color=subreddit_group), size=3) +
    scale_color_manual(values=myc) +
    geom_label_repel(aes(label=subreddit), size=3.7) +
    #theme_clean() +
    ggtitle("Relationships modularity by replies modularity, 2015-2020")

ggplotly(p)

ggplot(mdf, aes(year, rep, color=subreddit)) +
  geom_point() +
  geom_line(aes(group=subreddit)) +
  #geom_label_repel(aes(label=subreddit)) + 
  facet_wrap(~subreddit,nrow=4) +
  scale_color_viridis_d()


ggplot(mdf, aes(year, rel, color=subreddit)) +
  geom_point() +
  geom_line(aes(group=subreddit)) +
  #geom_label_repel(aes(label=subreddit)) + 
  facet_wrap(~subreddit,nrow=4) +
  scale_color_viridis_d()





ggplot(mdf, aes(x=rel,rep,group=subreddit)) +
  geom_abline(slope=1, intercept=0, color="tomato3",lwd=1) +
  geom_point(aes(color=subreddit), size=3) +
  scale_color_viridis_d() +
  geom_label_repel(aes(label=subreddit), size=1.8) +
  #theme_clean() +
  theme(legend.position = "none") +
  facet_wrap(~year) +
  ggtitle("Relationships modularity by replies modularity, 2015-2020") 



ggplot(mdf, aes(year,rep,group=subreddit, color=rel)) +
  geom_point(size=3) +
  scale_color_viridis_c() +
  geom_line(aes(group=subreddit)) +
  #theme_clean() +
  facet_wrap(~subreddit) +
  ggtitle("Replies modularity, 2015-2020", subtitle="color=relationship modularity") 

ggplot(mdf, aes(year,rep,group=subreddit, color=rel)) +
  geom_point(size=3) +
  scale_color_viridis_c() +
  geom_line(aes(group=subreddit)) +
  theme_fivethirtyeight() +
  facet_wrap(~subreddit,nrow=3) +
  ggtitle("Replies modularity, 2015-2020", subtitle="color=relationship modularity") +
  theme(axis.text.x = element_text(angle=90)) 
  

ggplot(mdf, aes(year,rel,group=subreddit, color=rep)) +
  geom_point(size=3) +
  scale_color_viridis_c() +
  geom_line(aes(group=subreddit)) +
  theme_fivethirtyeight() +
  facet_wrap(~subreddit,nrow=3) +
  ggtitle("Relationship modularity, 2015-2020", subtitle="color = replies modularity") +
  theme(axis.text.x = element_text(angle=90)) 

getColors()

ggplot(mdf, aes(group=subreddit)) +
  geom_point(aes(year,rel),size=3, color="#007783") +
  geom_point(aes(year,rep),size=3, color="#D38BA0") +
  scale_color_viridis_c() +
  geom_line(aes(year,rel,group=subreddit), color="#007783") +
  geom_line(aes(year,rep,group=subreddit), color="#D38BA0") +
  theme_fivethirtyeight() +
  facet_wrap(~subreddit,nrow=3) +
  ggtitle("Relationship & modularity, 2015-2020", subtitle="blue=relationships  |  purple = replies") +
  theme(axis.text.x = element_text(angle=90)) 













