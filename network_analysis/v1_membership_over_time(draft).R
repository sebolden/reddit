library(tidyverse)
library(igraph)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(viridis)

data <- makeBabyDF(getcsv('trp_nw_full.csv'))

yr <- getMembership(data)

auth <- data %>% select(author, score, year)
auth <- auth %>% group_by(author, year) %>% summarise(avg.score = mean(score))
auth$date <- auth$year
auth <- auth[,-2]

mv <- yr %>% group_by(author) %>% mutate(total_red = sum(rwt), total_blue=sum(bwt)) %>% ungroup()
mv <- mv %>% filter(total_red > 1) %>% filter(total_blue>1)
mv <- mv %>% inner_join(auth, by=c("author", "date"))

mv <- mv %>% group_by(author, date) %>% summarise(rw=sum(rwt), bw=sum(bwt),avg.score=unique(avg.score))
mv$total <- mv$rw+mv$bw
mv$prop.red <- (mv$rw/mv$total)
mv$prop.blue <- (mv$bw/mv$total)

mv$class[mv$prop.red>mv$prop.blue] <- "red"
mv$class[mv$prop.red<mv$prop.blue] <- "blue"
mv$class[mv$prop.red==mv$prop.blue] <- "equal"

test <- mv[1:91,]

myc <- c(blue = "#003f5c", red= "#ff6361", equal="#BC5090")
mv$logscore <- mv$avg.score
mv$logscore[mv$avg.score < 1] <- 0.5

ggplot(mv, aes(date, prop.red, color=class, group = author, size=avg.score)) +
  geom_jitter(alpha=.5) +
  #scale_x_log10() +
  geom_line(aes(date, prop.red, group=author), alpha=.5, color="black") +
  scale_color_manual(values=myc) +
  #facet_wrap(~date) +
  scale_size(range=c(1,6)) +
  theme_fivethirtyeight()



myc <- c(blue = "#003f5c", red= "#ff6361")
m1 <- mv %>% filter(prop.red>prop.blue)
m2 <- mv %>% filter(prop.blue>prop.red)
pp <- ggplot() +
  geom_jitter(data=m1, aes(x=logscore, y=rw, color=class), alpha=.3) +
  geom_jitter(data=m2, aes(x=logscore, y=bw, color=class), alpha=.3) +
  scale_y_log10() +
  scale_x_log10() +
  #geom_line(aes(date, avg.score), alpha=.5, color="black") +
  scale_color_manual(values=myc) +
  facet_wrap(~date) +
  #scale_size(range=c(1,6)) +
  theme_fivethirtyeight() 
pp





















