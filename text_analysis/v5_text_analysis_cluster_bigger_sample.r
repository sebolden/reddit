# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(tm)
library(factoextra)
library(ggthemes)
library(viridis)
library(textmineR)


# SOURCE(s) ---------------------------------------------------------------
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html

# DATA --------------------------------------------------------------------
data <- getcsv('clean_comments.csv')
txt<- getcsv('text_files_combined.csv')

# PREP TEXT ---------------------------------------------------------------

x <- subset_quantiles(data,.95,.05,0.70)
x <- combine_and_clean(x, txt)

# create a document term matrix 
dtm <- CreateDtm(doc_vec = x$text, # character vector of documents
                 doc_names = x$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

dim(dtm)
# 23550 1808366


# TF-IDF and cosine similarity
tf_mat <- TermDocFreq(dtm)

# look at most frequent ngrams
head(tf_mat[ order(tf_mat$term_freq, decreasing = TRUE) , ], 20)
# look at the most frequent bigrams
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 20)
# remove common ngrams from the DTM
dtm <- dtm[ , ! stringr::str_detect(colnames(dtm),"women|men|people|time|man|woman|make|good|life|things|work|sex|shit|feel|guy|thing|girl|lot|fuck|male|back|point|find|social|years|relationship|person|give|bad|day|love|world|gender|long|friends|problem|part|fact|hard|lot|feel|means|place|talk|made|put|making|start|understand|kind")]
# remove common bigrams from the DTM
dtm <- dtm[ , ! stringr::str_detect(colnames(dtm),"men_women|red_pill|women_men|long_term|years_ago|women_women|blue_pill|high_school|gender_roles|men_men|youtube_watch|wikipedia_org|blah_blah") ]

## UPDATED
tf_mat <- TermDocFreq(dtm)
# look at most frequent ngrams
head(tf_mat[ order(tf_mat$term_freq, decreasing = TRUE) , ], 20)
# look at the most frequent bigrams
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 20)
rm(tf_bigrams)



tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
tfidf <- t(tfidf)
rm(tf_mat)
rm(dtm)
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
rm(tfidf)
csim <- csim %*% t(csim)
#R’s various clustering functions work with distances, not similarities. We convert cosine similarity to cosine distance by subtracting it from 1. This works because cosine similarity is bound between 0 and 1. While we are at it, we’ll convert the matrix to a dist object.
csim <- as.dist(1 - csim)
#The last step is clustering. There are many clustering algorithms out there. My preference is agglomerative hierarchical clustering using Ward’s method as the merge rule. Compared to other methods, such as k-means, hierarchical clustering is computationally inexpensive.

#In the example below, I choose to cut the tree at 10 clusters. This is a somewhat arbitrary choice. I often prefer to use the silhouette coefficient. You can read about this method here. Performing this is an exercise I’ll leave to the reader.

cdist <- hclust(cdist, "ward.D")

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]})

# create a summary table of the top 5 words defining each cluster
clustering <- cutree(hc, 50)
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")}),
                              stringsAsFactors = FALSE)

cluster_summary

saveRDS(hc, file="hierarchical_clustering.RDS")
saveRDS(cluster_summary, "hierarchical_clustering_50c.RDS")

# plot a word cloud of one cluster as an example
wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "blue"),
                     main = "Top words in cluster 100")

x$cluster <- clustering