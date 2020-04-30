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

# ORIGINAL CODE (can skip this) -------------------------------------------
#mod <- getModByYear()
#deg <- getDegreeByYear()
#mod$year <- as.numeric(mod$year)
#deg$year <- as.numeric(deg$year)
#nw <- mod %>% left_join(deg, by=c('year', 'subreddit'))
#nw <- nw %>% select(year, everything()) %>% select(subreddit, everything())
#linx$mod=0
#linx$mod[linx$is_mod=="moderator"] <- 1
#linx$admin=0
#linx$admin[linx$is_mod=="admin"] <- 1
#wt <- linx %>% group_by(subreddit,year) %>% summarise(total.comments=n())
#substats <- linx %>% group_by(subreddit, year) %>% summarise(avg.score=mean(score), total.controv=sum(controversiality), total.submitter=sum(is_submitter), total.locked=sum(locked), total.mod=sum(mod), total.admin=sum(admin))
#nw <- nw %>% inner_join(substats, by=c('subreddit', 'year'))
#nw <- nw %>% inner_join(wt, by=c('subreddit', 'year'))
#nw$sub_group[nw$subreddit %in% blue] <- 'blue'
#nw$sub_group[nw$subreddit %in% red] <- 'red'
#write.csv(nw, 'network_data_subreddit_year.csv', row.names=F)

# DATA --------------------------------------------------------------------
data <- makeBabyDF(getcsv('trp_nw_full.csv'))
linx <- getLinks(data)
fname <- unique(linx$subreddit)
nw <- getcsv('network_data_subreddit_year.csv')

# VIZ: modularity ---------------------------------------------------------------------
myc <- getColors()

ggplot(nw, aes(x=rel_modularity, y=rep_modularity,group=subreddit)) +
  geom_abline(slope=1, intercept=0, color="tomato3",lwd=1) +
  geom_point(aes(color=sub_group), size=3) +
  scale_color_manual(values=myc) +
  geom_label_repel(aes(label=subreddit), size=3.7) +
  #theme_clean() +
  ggtitle("Relationships modularity by replies modularity, 2015-2020")


# VIZ: assorted -----------------------------------------------------------

colnames(nw)
#"subreddit"   "year"   "rel_modularity"    "rep_modularity"   "degree"   "avg.score"   "total.controv"   "total.submitter"   "total.locked"    total.mod"      "total.admin"     "total.comments"  "sub_group" 

ggplot(nw, aes(year, total.controv, fill=sub_group)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=myc) +
  facet_wrap(~sub_group) +
  theme_bw() + 
  ggtitle("total number of controversial comments by year")

ggplot(nw, aes(year, total.mod, fill=sub_group)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=myc) +
  facet_wrap(~sub_group) +
  theme_bw() +
  ggtitle("total number of moderator comments by year")

ggplot(nw, aes(year, degree, size=avg.score, color=sub_group)) +
  geom_point() +
  geom_line(aes(group=subreddit),size=.5) +
  scale_color_manual(values=myc) +
  facet_wrap(~subreddit) +
  theme_bw()

ggplot(nw, aes(group=subreddit)) +
  geom_point(aes(x=year, y=degree), color="#3FC476") +
  geom_point(aes(x=year, y=total.mod), color="#FF6361") +
  geom_point(aes(x=year, y=avg.score), color="#966F98") +
  geom_line(aes(x=year, y=degree), color="#3FC476") +
  geom_line(aes(x=year, y=total.mod), color="#FF6361") +
  geom_line(aes(x=year, y=avg.score), color="#966F98") +
  facet_wrap(~subreddit) +
  theme_fivethirtyeight() +  

ggplot(nw, aes(year, degree, color=sub_group)) +
  geom_area() +
  scale_color_manual(values=myc) +
  facet_wrap(~subreddit) +
  theme_bw()











