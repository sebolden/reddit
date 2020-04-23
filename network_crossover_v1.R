# PACKAGES  --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)
library(ggthemes)
library(stringi)
library(viridis)

# PREP  --------------------------------------------------------------------
data <- getData()

# group users by time+color
x <- getMonthYearMembership(data)
y <- getYearMembership(x)

# convert to proportions
x <- getProps(x)
y <- getProps(y)

# remove some of the unnecessary columns
x <- x %>% select(greater, author, year, date, rwt, bwt, total, prop.red, prop.blue)
y <- y %>% select(greater, author, year, rwt, bwt, total, prop.red, prop.blue)

# subset to only include users who have proportions other than 0/1
sx <- x[x$prop.red>0,]
sx <- sx[sx$prop.red<1,]

sy <- y[y$prop.red>0,]
sy <- sy[sy$prop.red<1,]

# dunno how tf 2014 ended up in here 
sx <- sx[sx$year!="2014",]
sy <- sy[sy$year!="2014",]

# messing around with an even smaller subset, idk
df <- sx[sx$prop.red > .3,]
df <- df[df$prop.red < .4,]

# PLOT --------------------------------------------------------------------

# an attempt at plotting? 
myc <- getColors()

ggplot(sy, aes(author,year,color=greater)) +
  geom_point(aes(size=prop.red),alpha=.3) +
  geom_point(aes(size=prop.blue),alpha=.3) +
  scale_color_manual(values=myc) +
  theme_classic()+
  theme(axis.text.y=element_blank()) +
  coord_flip()

ggplot(data=sx, aes(author, date,color=greater,size=prop.red)) +
  geom_point(alpha=.7) +
  scale_color_manual(values=myc) +
  theme_classic() +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

ggplot(data=sy, aes(author, year,color=greater,size=prop.red)) +
  geom_point(alpha=.4) +
  scale_color_manual(values=myc) +
  theme_classic() +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

# THIS ONE IS MASSIVE: 0/10, DO NOT RECOMMEND TRYING TO PLOT.
"
ggplot(data=x, aes(author, date,color=greater,size=prop.red)) +
  geom_point(alpha=.7) +
  scale_color_manual(values=myc) +
  theme_classic() +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 
"






























































