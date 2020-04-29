# PACKAGES ----------------------------------------------------------------
library(htmlwidgets)
library(tidyverse)
library(here)
library(circlize)
library(reshape)
library(migest)
library(dplyr)
library(chorddiag)
library(igraph)
library(arcdiagram)
library(plotly)
library(ggthemes)
library(viridis)
options(scipen=999)


# DATA --------------------------------------------------------------------
data <- makeBabyDF(getcsv('trp_nw_full.csv'))
linx <- getLinks(data)

# MUST run getCrossoverDF function first
xoverDF <- getCrossoverDF(data)
xoverMA <- getCrossoverMatrix(xoverDF) 

colnames(x) <- c("sub1", "sub2", "n", "perc", "year")

p <- x %>%  arrange(sub1) %>% 
    ggplot(aes(sub1, sub2, fill= perc)) + 
    geom_tile() +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 45)) +
    coord_flip() +
    ggtitle("percent of unique users from subreddit X also found in subreddit Y")

ggplotly(p)


library(RColorBrewer)
mc <- brewer.pal(n=26, 'PuBuGn')
chorddiag(xoverMA, palette = 'PuBuGn')

















