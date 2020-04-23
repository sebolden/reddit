# PACKAGES ----------------------------------------------------------------
# pmi
library(tidyverse)
library(tidytext)
library(widyr)
library(irlba)
library(broom)

# word2vec/keras
library(reticulate)
library(purrr)
library(text2vec) 
library(Rtsne)
library(plotly)
library(stringr)
library(keras)

# source 3 (k means clustering)
library(tm)
library(dbscan)
library(proxy)
library(colorspace)

# CONTEXT  --------------------------------------------------------------------
"CODE SOURCE: 
 https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html

 OTHER FILES REQUIRED:
 text_analysis_v2_functions.R

 CSVS REQUIRED:
 comments_clean_df.csv
 text_files_combined.csv"

# IMPORT DATA --------------------------------------------------------------------
data <- read.csv("comments_clean_df.csv", header=T, stringsAsFactors = F)
txt <- read.csv("text_files_combined.csv", header=T, stringsAsFactors = F)

# PREPARE DF --------------------------------------------------------------------
x <- remove_empty(data)
# for each subreddit, subset to only include top/bottom 10% scoring comments
# and then sample 2% of those comments
x <- subset_quantiles(x, .90, .10, .02)
x <- combine_and_clean(x, txt) # should end up with 13,901 obs

# SKIPGRM PROBABILITIES  --------------------------------------------------------------------

# create context window with length 8
tidy_skipgrams <- x %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, id, ngramID) %>%
  unnest_tokens(word, ngram)

# calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- x %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

# calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

# normalize probabilities
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>% select(word1 = word, p1 = p), by = "word1") %>%
  left_join(unigram_probs %>% select(word2 = word, p2 = p), by = "word2") %>%
  mutate(p_together = p / p1 / p2)

# test it out
normalized_prob %>% filter(word1 == "pill") %>% arrange(-p_together)

# PMI MATRIX  --------------------------------------------------------------------

# create PMI matrix
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

# remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

# run SVD
pmi_svd <- irlba(pmi_matrix, 256, maxit = 500)

# output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

# identify synonyms using the word vectors created above
search_synonyms(word_vectors,word_vectors["pill",])
search_synonyms(word_vectors,word_vectors["red",])
search_synonyms(word_vectors,word_vectors["alpha",])
search_synonyms(word_vectors,word_vectors["fuck",])

# plot 200 words from the model in 2 dimensional space
tiny.pmi <- irlba(pmi_matrix, 2, maxit = 500)

# output the word vectors:
tiny.wv  <- tiny.pmi$u
rownames(tiny.wv) <- rownames(pmi_matrix)

# grab 100 words
words2plot<-as.data.frame(tiny.wv[300:350,])
words2plot$word<-rownames(words2plot)

# plot
ggplot(words2plot, aes(x=V1, y=V2, label=word))+
  geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
  theme_minimal()+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")

# Keras and W2V?? ---------------------------------------------------------
# jk, tensorflow isn't working

# sim2 ---------------------------------------------------------

"So, all of these approaches basically spit out a vector to represent an object in your dataset

//

The R Blog you sent seems to be doing this on a document by document basis.  SVD actually computes both document and word vectors.  Word2Vec and PCA really just compute word vectors.

//

Cosine similarity (for our purposes) is really only meaningful for vectors here.

//

So, if you have word vectors, you can compute the a vector for any document by taking the average of all of the words in each document - that means, taking the averages of entries in their respective positions in each vector.

//

The average is sometimes called the centroid in a vector space.

//

What I would do is go back to https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html and after SVD is run, pull out the word vectors, and find the centroid for each document.

//

the sim2 method does (I think) compute a matrix of cosine similarities.

//

However, it might not be necessary to derive that right away.  For instance, you could cluster (using k-means or something), to identify document clusters, and then from there figure out if different clusters correspond to different topics or popularity rankings. (edited) 

//

It would be interesting to see if canon documents are in the same spot, or cluster with certain forum posts, or cluster with popular posts

//

You could also take is cluster, and calculate the distance from the center for individual documents 

//

Note that kmeans is going to use euclidean distance, rather than cosine distance

//

To cope with that, you can normalize each vector by dividing by its length, which is just the sqrt of the sum of its squared entries

//

euclidean distances with normalized vectors are equivalent to cos distances.

//

I think a question to consider is just what kind of output you want, and how you want to analyze it?"
  
  
# attempt 1: k-means clustering
# source: https://gist.github.com/luccitan/b74c53adfe3b6dad1764af1cdc1f08b7

## dist.matrix = proxy::dist(pmi_svd, method = "cosine")
####### Error in crossprod(x, y) : non-conformable arguments

## dist.matrix = proxy::dist(pmi_matrix, method = "cosine")
####### Error in proxy::dist(pmi_matrix, method = "cosine") : 
####### Can only handle data frames, vectors, matrices, and lists!


dist.matrix = proxy::dist(word_vectors, method = "cosine")

# PARTITIONING ------------------------------------------------------------

"
the source set their truth.k to 16 bc that's how many documents they have.
when i do it, i get an error.
so. ?.
" 

truth.K <- 13901
partition.kmeans <- kmeans(word_vectors, truth.K)
##### Error in sample.int(m, k) : 
##### cannot take a sample larger than the population when 'replace = FALSE'

# HIERARCHICAL  ------------------------------------------------------------
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2")
# this one worked, but im still not sure where, if anywhere, it's getting document-level info from

# DENSITY-BASED  ------------------------------------------------------------
"
this code ran.
but i got a space error.
" 

clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)
##### Error: cannot allocate vector of size 715.7 Mb

# PLOTTING  ------------------------------------------------------------

"
Another space error.
"
points <- cmdscale(dist.matrix, k = 2) # Running the PCA
##### Error: cannot allocate vector of size 715.7 Mb
















































