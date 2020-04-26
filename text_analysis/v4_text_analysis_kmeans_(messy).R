library(tm)
library("factoextra")


# GET DATA ----------------------------------------------------------------
data <- getComments()
txt<- getcsv('text_files_combined.csv')

# PREP DATA ---------------------------------------------------------------

x <- subset_quantiles(data,.95,.05,.02)
x <- combine_and_clean(x, txt)

myCorpus <- Corpus(VectorSource(x$text))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus<- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, stripWhitespace)
dtm <- TermDocumentMatrix(myCorpus,control = list(minWordLength = 1))
dtm_tfxidf <- weightTfIdf(dtm)
m1 <- as.matrix(dtm_tfxidf)
m<-t(m1)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
head(m_norm)

fviz_nbclust(m_norm, kmeans
             ,method = "wss"
             , k.max=15) 

fit.km <- kmeans(m_norm, 13, nstart = 25)
#fit.small <- kmeans(m_norm, 7, nstart = 25)
for (i in 1:13) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(fit.km$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")}

# Visualize
docids <- colnames(dtm_tfxidf)
x$docids <- docids

library(cluster)
library(fpc)

xx$year <- as.numeric(xx$year)
class(xx$year)
# Fig 01
plotcluster(xx, fit.km$cluster)

??clusplot()

X <- data.frame(c1=c(0,1,2,4,5,4,6,7),c2=c(0,1,2,3,3,4,5,5))
m <- as.matrix(dtm_tfxidf)

x$group[x$quantile_group=="low"] <- 2
x$group[x$quantile_group=="high"] <- 3
x$group[x$quantile_group=="not_applicable"] <- 1


xx <- x %>% select(docids, group)
plot(xx,col=fit.km$cluster)

points(fit.km$center,col=1:2,pch=8,cex=1)


head(fit.km$cluster)
length(fit.km$cluster)
x$cluster <- fit.km$cluster
View(x)















sc <- data %>% select(id,score)
colnames(sc) <- c("textid", "score")
colnames(x) <- c("id", "subreddit", "quantile_group", "year", "text", "author", "docids", "group", "cluster", "textid", "subreddit.y", "quantile_groupy", "yeary", "authory")
x <- x %>% select(id,textid,subreddit,quantile_group,year,text,author,cluster)
xx <- x %>% left_join(sc, by="textid")
xx <- x %>% left_join(xx, by="textid")
colnames(xx)
xx <- xx %>% select(id,textid,cluster,subreddit,quantile_group, year,text)



x <- xx
rm(xx)




























