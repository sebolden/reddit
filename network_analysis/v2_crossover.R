# CONTEXT -----------------------------------------------------------------
"Requires functions from the `all_functions.R' file."

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
data <- getcsv('COMMENT_CSVS_BABY.csv')
xoverMA <- getcsv('user_crossover_matrix.csv') ## OR: xoverMA <- getCrossoverMatrix(data)
xoverDF <- getcsv('user_crossover_df.csv') ## OR xoverDF <- xgetCrossoverDF(data)


bnames <- c("socialjustice101", "SocJus", "WitchesVsPatriarchy", "Feminism", "feminisms", "FemmeThoughts", "FemsLib", "Radical_Feminists", "FemaleDatingStrategy", "WhereAreTheFeminists", "bropill", "PunchingMorpheus", "againstmensrights", "TheBluePill", "feminismformen", "TheMensCooperative", "MaleSupportNetwork", "MensLib", "GenderCriticalGuys")
rnames <- c("exredpill", "LibFemExposed", "PinkpillFeminism", "Trufemcels", "FeMRADebates", "AskFeminists", "asktrp", "RedPillRetention", "RedPillWorkplace", "marriedredpill", "RedPillNonMonogamy", "altTRP", "GEOTRP", "TRPcore", "trpgame", "TRPOfftopic", "ThankTRP", "redpillbooks", "redpillmedia", "redpillmusic", "RedPillParenting", "RedPillWomen", "RedPillWives", "TheRedPill", "pussypassdenied")

bdf <- xoverDF[xoverDF$x %in% bnames,]
bdf <- bdf[bdf$y %in% bnames,]

rdf <- xoverDF[xoverDF$x %in% rnames,]
rdf <- rdf[rdf$y %in% rnames,]

bmat <- getCrossoverMatrix(bdf)
rmat <- getCrossoverMatrix(rdf)

rm(bnames,rnames,bdf,rdf)

# PREPPING ----------------------------------------------------------------
xdf <- xoverDF %>% select(x, y, x_y)
colnames(xdf) <- c("from", "to", "weight")
E(arc_edges)$weight

View(xdf)
arc_edges <- graph_from_adjacency_matrix(xoverMA)
ll <- layout.drl(arc_edges)
ll <- layout.fruchterman.reingold(arc_edges)


chorddiag(bmat,groupnamePadding = 25, groupnameFontsize=11)
chorddiag(rmat,groupnamePadding = 25, groupnameFontsize=11)


# PLOTTING ----------------------------------------------------------------

x <- xoverDF[xoverDF$perc != 100.00,]
x <- x %>% rename(sub1 = x) %>% rename(sub2=y)
x <- x[x$sub1 != "TheRedPill" & x$sub1 != "trpgame",]
x <- x[x$sub1 != "TRPcore" & x$sub1 != "TRPOffTopic",]
x <- x[x$sub1 != "ThankTRP" & x$sub1 != "RedPillWorkplace",]
x <- x[x$sub1 != "RedPillWomen" & x$sub1 != "RedPillWives",]
x <- x[x$sub1 != "RedPillRetention" & x$sub1 != "RedPillParenting",]
x <- x[x$sub1 != "RedPillNonMonogamy" & x$sub1 != "redpillmusic",]
x <- x[x$sub1 != "redpillmedia" & x$sub1 != "redpillbooks",]
x <- x[x$sub1 != "marriedredpill" & x$sub1 != "GEOTRP",]
x <- x[x$sub1 != "altTRP" & x$sub1 != "asktrp",]
x <- x[x$sub2 != "TheRedPill" & x$sub2 != "trpgame",]
x <- x[x$sub2 != "TRPcore" & x$sub2 != "TRPOffTopic",]
x <- x[x$sub2 != "ThankTRP" & x$sub2 != "RedPillWorkplace",]
x <- x[x$sub2 != "RedPillWomen" & x$sub2 != "RedPillWives",]
x <- x[x$sub2 != "RedPillRetention" & x$sub2 != "RedPillParenting",]
x <- x[x$sub2 != "RedPillNonMonogamy" & x$sub2 != "redpillmusic",]
x <- x[x$sub2 != "redpillmedia" & x$sub2 != "redpillbooks",]
x <- x[x$sub2 != "marriedredpill" & x$sub2 != "GEOTRP",]
x <- x[x$sub2 != "altTRP" & x$sub2 != "asktrp",]
x <- x[x$sub2 != "Femslib" & x$sub2 != "MensLib",]
x <- x[x$sub1 != "Femslib" & x$sub1 != "MensLib",]



p <- x %>% arrange(sub1) %>% 
  ggplot(aes(sub1, sub2, fill= perc)) + 
    geom_tile() +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 45)) +
    coord_flip()
ggplotly(p)  












