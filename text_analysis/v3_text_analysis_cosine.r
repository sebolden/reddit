
# SOURCE: https://rpubs.com/saqib/DocumentClustering

# PACKAGES ----------------------------------------------------------------
library(tm)
library(proxy)
library(RTextTools)
library(fpc)   
library(wordcloud)
library(cluster)
library(tm)
library(stringi)
library(proxy)
library(wordcloud)
library(tidyverse)



# GET DATA ----------------------------------------------------------------
data <- import_clean_comments()
txt <- getcsv('text_files_combined.csv')

# PREP CORPUS ----------------------------------------------------------------
#xp <- remove_empty(data)
# for each subreddit, subset to only include top/bottom 10% scoring comments
# and then sample 1% of those comments
x <- subset_quantiles(data, .90, .10, .005)
x <- combine_and_clean(x, txt) 
x <- x %>% select(text, everything())

# create the corpus
corpus <- Corpus(VectorSource(x$text))


ndocs <- length(corpus)
minTermFreq <- ndocs * 0.01
maxTermFreq <- ndocs * .5


# PREP DTM ----------------------------------------------------------------

dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))))

dtm.matrix <- as.matrix(dtm)
wordcloud(colnames(dtm.matrix), dtm.matrix[1000,], max.words = 20)

dtm.tfidf <- weightTfIdf(dtm, normalize = TRUE)
dtm.matrix.tfidf <- as.matrix(dtm.tfidf)
wordcloud(colnames(dtm.matrix.tfidf), dtm.matrix[1000, ], max.words = 20)
inspect(dtm.tfidf)
m  <- as.matrix(dtm)

#You could also take is cluster, and calculate the distance from the center for individual documents 
#Note that kmeans is going to use euclidean distance, rather than cosine distance
#To cope with that, you can normalize each vector by dividing by its length, which is just the sqrt of the sum of its squared entries

# toss a buncha stuff so R doesn't try to fucking murder me
rm(corpus,dtm,dtm.matrix.tfidf,dtm.tfidf,txt, dtm.matrix)

# I'm really hoping that the code below does the math for me
# but there is also a function for euclidean distance if not

distMatrix <- dist(m, method="cosine",normalized=T)
dmat <- as.matrix(distMatrix)
dmat[10:20,10:20]

# from Stack Overflow: https://stackoverflow.com/questions/29750519/r-calculate-cosine-distance-from-a-term-document-matrix-with-tm-and-proxy
#diag(distMatrix) <- NA
cosine_dist <- apply(distMatrix, 2, mean, na.rm=TRUE)

x$distMat <- cosine_dist
summary(x$distMat) # this part is screenshotted


View(x)
#write.csv(x, "centroids_possibly.csv", row.names=F)

##########################################################################
"I haven't tried this little chunk of code here, but I thought it might be useful to hang onto for if/when I try creating cosine distances/centroids for a larger volume of docs."
# Since tm's term document matrices are just sparse "simple triplet matrices" from the slam package, you could use the functions there to calculate the distances directly from the definition of cosine similarity. NOTE: for this to work, you should not coerce the tdm into a regular matrix, i.e don't do tdm <- as.matrix(tdm).
library(slam)
cosine_dist_mat <- crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
##########################################################################


# CLUSTERING --------------------------------------------------------------


ward.clust <- hclust(distMatrix,method="ward.D")
cent.clust <- hclust(distMatrix,method="centroid")
avg.clust <- hclust(distMatrix,method="average")
med.clust <- hclust(distMatrix,method="median")
ward2.clust <- hclust(distMatrix,method="ward.D2")
sing.clust <- hclust(distMatrix,method="single")
comp.clust <- hclust(distMatrix,method="complete")
mc.clust <- hclust(distMatrix,method="mcquitty")

coph.ward <- cophenetic(ward.clust)
coph.cent <- cophenetic(cent.clust)
coph.avg <- cophenetic(avg.clust)
coph.med <- cophenetic(med.clust)
coph.ward2 <- cophenetic(ward2.clust)
coph.sing <- cophenetic(sing.clust)
coph.comp <- cophenetic(comp.clust)
coph.mc <- cophenetic(mc.clust)

cor(distMatrix, coph.ward) # 0.08355185      BIG YIKES
cor(distMatrix, coph.cent) # 0.3280035       ehhhhhhhhhhhhhh
cor(distMatrix, coph.avg) # 0.6145705        better
cor(distMatrix, coph.med) # 0.222921         bleh
cor(distMatrix, coph.ward2) # 0.04175146     l o l
cor(distMatrix, coph.sing) # 0.1818138       yuck


cor(distMatrix, coph.comp) # 0.5079201       meh
cor(distMatrix, coph.mc) # 0.554887          idfk

# SO: afaik.clust, i should go w the 'average' method??
rm(ward.clust, ward2.clust, cent.clust, mc.clust, med.clust, sing.clust, comp.clust)

#plot(avg.clust, cex=0.9, hang=-1)
rect.hclust(avg.clust, k=5)
clusterCut <- cutree(avg.clust, 70)
table(clusterCut, x$quantile_group) # OMFG, jesus

plot(ward.clust, cex=0.9, hang=-1)
rect.hclust(ward.clust, k=10)
clusterCut <- cutree(ward.clust, 40)
table(clusterCut, x$quantile_group) # OMFG, jesus

#kmeans – determine the optimum number of clusters (elbow method)
#look for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
##### This made R mad, so i'm skipping for now :(

###########################################################################
wss <- 2:10
for (i in 2:10) wss[i] <- sum(kmeans(distMatrix,centers=i,nstart=25)$withinss)
plot(2:10, wss[2:10], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
###########################################################################
km.res <- kmeans(distMatrix, 4, nstart = 25)
###########################################################################
truth.K <- 3600
clustering.kmeans <- kmeans(distMatrix, truth.K) 
###########################################################################
kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)}
###########################################################################

















































## clustering.kmeans <- kmeans(tfidf.matrix, truth.K)


# partitioning one (K-Means), a hierarchical one (Bottom-up merging) and a density-based one (HDBScan)
















