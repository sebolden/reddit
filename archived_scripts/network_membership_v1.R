# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(igraph)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(viridis)
library(lubridate)


# GET DATA ----------------------------------------------------------------
data <- makeBabyDF(getcsv('trp_nw_full.csv'))
myc.eq <- c(blue = "#003f5c", red= "#ff6361", equal="#BC5090")
myc <- c(blue = "#003f5c", red= "#ff6361")

# SOME STUFF --------------------------------------------------------------
colnames(membership)
# "class.x"   "author"    "date"      "rwt"       "bwt"       "greater"   "total"     "prop.red"  "prop.blue"
colnames(auth.mvmt)
# "author"    "date"      "rw"        "bw"        "avg.score" "total"     "prop.red"  "prop.blue" "class"    

ggplot(auth.mvmt, aes(date, avg.score, fill=class)) +
  geom_area(alpha=.4) +
  scale_fill_manual(values=myc.eq) 






ggplot() +
  geom_jitter(data=auth.mvmt %>% group_by(author) %>% filter(sum(rw) > sum(bw)) %>% ungroup()
, aes(x=logscore, y=prop.red, color=class), alpha=.3) +
  geom_jitter(data=auth.mvmt %>% group_by(author) %>% filter(sum(bw) > sum(rw)) %>% ungroup()
, aes(x=logscore, y=prop.red, color=class), alpha=.3) +
  scale_y_log10() +
  scale_x_log10() +
  #geom_line(aes(date, avg.score), alpha=.5, color="black") +
  scale_color_manual(values=myc) +
  facet_wrap(~date) +
  ggtitle("log score by log proportion of user comments/year that belong to 'red' subreddits", subtitle="blue comments are comments where the proportion of 'red' comments  is >0.5") +
  #scale_size(range=c(1,6)) +
  theme_fivethirtyeight() 


















