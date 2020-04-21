library(randomForest)
library(tidyverse)
library(tidytext)
library(broom)
library(stringr)
library(plyr)
library(dplyr)
library(magrittr)
library(tm)
library(proxy)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(text2vec)

###### SOURCE CODE #####
# https://www.hvitfeldt.me/blog/using-pca-for-word-embedding-in-r/

##### FUNCTIONS #####
quantile_filter <- function(df, upper_quantile, lower_quantile, sample_percentage) {
  nval <- nrow(df[df$score < quantile(df$score,lower_quantile),])
  sub_high <- df[df$score > quantile(df$score,upper_quantile),]
  sub_high$quantile_group <- "high"
  sub_high <- sub_high %>% sample_frac(sample_percentage)
  if(nval < 1) {
    sub_low <- df
    sub_low$quantile_group <- "REMOVE"
    sub_low <- sub_low %>% sample_frac(sample_percentage)}
  if(nval >= 1) {
    sub_low <- df[df$score < quantile(df$score,lower_quantile),]
    sub_low$quantile_group <- "low"
    sub_low <- sub_low %>% sample_frac(sample_percentage)}
  sub_merged <- rbind(sub_low, sub_high)
  return(sub_merged)}

subset_by_score <- function(df_name, upper_quantile, lower_quantile, sample_percentage) {
  subnames <- unique(df_name$subreddit)
  print(subnames)
  df <- df_name %>% select(body, id, subreddit, year, score, author)
  subset_the_subreddits <- function(x) {
    print(paste0("working on r/", x))
    tmp <- df %>% filter(subreddit==x)
    sub <- quantile_filter(tmp, upper_quantile, lower_quantile, sample_percentage)
    return(sub)}
  dfs_rejoined <- bind_rows(lapply(subnames,subset_the_subreddits))
  colnames(dfs_rejoined) <- c("text", "id", "subreddit", "year", "score", "author", "quantile_group")
  n_removed <- (nrow(dfs_rejoined[dfs_rejoined$quantile_group=="REMOVE",]) +
                  nrow(dfs_rejoined[dfs_rejoined$text=="",]) +
                  nrow(dfs_rejoined[dfs_rejoined$text=="[deleted]",]) +
                  nrow(dfs_rejoined[dfs_rejoined$text=="[removed]",])) 
  dfs_rejoined <- dfs_rejoined %>% filter(quantile_group != "REMOVE") %>% 
    filter(text != "") %>% filter(text != "[deleted]") %>% filter(text != "[removed]")
  
  n_subs <- length(unique(dfs_rejoined$subreddit))
  print(paste0("there are ", n_removed, " comments that were removed :("))
  print(paste0("there are ", n_subs, " subreddits in your DF"))
  return(dfs_rejoined)}

##### SUBSET THE COMMENT DATA #####

# take top 95th and bottom 5th percentiles of scores for each individual subreddit, then take a sample of 5% of those messages
subsetted <- subset_by_score(data, .95, .05, .05)

# 'class' is what i'll use to distinguish btwn reddit comments vs. canon content
subsetted$class <- "reddit"

# R gets less angry at me when I cut down on the number of columns ¯ \_(ツ)_/¯
x <- subsetted %>% select(text, id, subreddit, quantile_group, class, year)

##### IMPORT THE TEXT (CANON) CSV #####
txt <- read.csv("text_files_combined.csv", header=T, stringsAsFactors = F)

txt <- txt %>% 
  select(text, doc_id) %>% 
  rename(id = doc_id)

# adding some columns with filler content to make the rbind easier
txt$subreddit <- "canon"
txt$quantile_group <- "not_applicable"
txt$class <- "canon"
txt$year <- "not_applicable"


##### COMBINE TEXT AND COMMENTS #####
x <- rbind(x, txt)

##### PRE-PROCESS TEXT DATA #####
x$text <- gsub("[[:punct:]]", "", x$text) 
x$text <- iconv(x$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')

x.clean <- x %>%
  mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
  select(id, subreddit, quantile_group, year, text)


##### PCA #####

# FROM SOURCE: extract all unigrams, bigram and remove stopwords.
data_counts <- map_df(1:3, ~ unnest_tokens(x.clean, word, text,token = "ngrams", n = .x)) %>%
  anti_join(stop_words, by = "word")

# FROM SOURCE: focus on the top 10000 most used words for the remainder of the analysis.
top10000 <- data_counts %>%
  count(word, sort = TRUE) %>%
  slice(1:10000) %>%
  select(word)

#FROM SOURCE: we will then count the words again, but this time we will count the word occurrence within each document and remove the underused words.
unnested_words <- data_counts %>%
  count(id, word, sort = TRUE) %>%
  inner_join(top10000, by = "word")

#FROM SOURCE: We then cast the data.frame to a sparse matrix.
sparse_word_matrix <- unnested_words %>%
  cast_sparse(id, word, n)

#FROM SOURCE:  This stage will take some time, but that is the trade-off we will be making when using word embedding. We take some computation time up front in exchange for quick computation later down the line.
word_scaled <- scale(sparse_word_matrix)
word_pca <- irlba::prcomp_irlba(word_scaled, n = 64)

#FROM SOURCE: Then we will create a meta data.frame to take care of tweets that disappeared when we cleaned them earlier.
meta <- tibble(id = dimnames(sparse_word_matrix)[[1]]) %>%
  left_join(x.clean[!duplicated(x.clean$id), ], by = "id")
class(x$id)
class_df <- data.frame(word_pca$x) %>%
  mutate(id = as.character(meta$id))

# this is adding 64 columns to the original DF with my sampled columns. Each of the 64 columns is the PCA value for that comment/canon doc.
xx <- x %>% left_join(class_df, by = "id")
#rm(x)
#rm(xx)

##### TRAINING AND TESTING ??? #####

# tbh i ran all this thinking it was going to help me get to the word embeddings

# but like halfway thru i realized it was just straight-up testing whether the PCA things can meaningfully predict what [insert categorical variable here] a given document belongs to. I finished running the analysis bc it seemed kinda interesting, but it's skippable in terms of embedding methods.

# the original source had 1 categorical variable; i had two: subreddit and score group. Probably intuitively (since there are fewer categories), score group performed a lot better than subreddit. So like -- if nothing else, i think this section sort of says "yeah, ok, probably low scoring and high scoring comments are doing different things" (???)


##### GLM: by subreddit #####

class_df <- data.frame(word_pca$x) %>%
  mutate(subreddit = factor(meta$subreddit),
         split = sample(0:1, NROW(meta), replace = TRUE, prob = c(0.2, 0.8)))

#FROM SOURCE: # We now have a data frame with 64 explanatory variables instead of the 10000 we started with. This a huge reduction which hopefully should pay off. For this demonstration will we try using two kinds of models. Standard logistic regression and a random forest model. Logistic regression is a good baseline which should be blazing fast now since the reduction have taken place and the random forest model which generally was quite slow should be more manageable this time around.


model <- glm(subreddit ~ ., 
             data = filter(class_df, split == 1), 
             family = binomial)

y_pred <- predict(model, 
                  type = "response",
                  newdata = filter(class_df, split == 0) %>% select(-subreddit))

y_pred_logical <- if_else(y_pred > 0.5, 1, 0)

(con <- table(y_pred_logical, filter(class_df, split == 0) %>% pull(subreddit)))
# this table was waaaaaaaaaay too big to paste into the script, but I saved a copy in Notepad++. 

sum(diag(con)) / sum(con)
## [1] 0.3138622
# ^ i think this means that the accuracy is crappy AF?

##### RANDOM FOREST: by subreddit


class_df <- data.frame(word_pca$x) %>%
  mutate(subreddit = factor(meta$subreddit),
         split = sample(0:1, NROW(meta), replace = TRUE, prob = c(0.2, 0.8)))

model <- randomForest(subreddit ~ ., 
                      data = filter(class_df, split == 1))

y_pred <- predict(model, 
                  type = "class",
                  newdata = filter(class_df, split == 0) %>% select(-subreddit))

(con <- table(y_pred, filter(class_df, split == 0) %>% pull(subreddit)))
sum(diag(con)) / sum(con)
## [1] 0.5136105
## ^ better, but not, like, great

##### GLM: by score group #####

class_df <- data.frame(word_pca$x) %>%
  mutate(quantile_group = factor(meta$quantile_group),
         split = sample(0:1, NROW(meta), replace = TRUE, prob = c(0.2, 0.8)))

model <- glm(quantile_group ~ ., 
             data = filter(class_df, split == 1), 
             family = binomial)

y_pred <- predict(model, 
                  type = "response",
                  newdata = filter(class_df, split == 0) %>% select(-quantile_group))

y_pred_logical <- if_else(y_pred > 0.5, 1, 0)

(con <- table(y_pred_logical, filter(class_df, split == 0) %>% pull(quantile_group)))
##               
## y_pred_logical     high    low     not_applicable (canon)
##              0     2015    1238    26
##              1     66      45      11
sum(diag(con)) / sum(con)
## [1] 0.6057042 
## ^ already a lot better than by subreddit

##### RANDOM FOREST: by score group #####

class_df <- data.frame(word_pca$x) %>%
  mutate(quantile_group = factor(meta$quantile_group),
         split = sample(0:1, NROW(meta), replace = TRUE, prob = c(0.2, 0.8)))

model <- randomForest(quantile_group ~ ., 
                      data = filter(class_df, split == 1))

y_pred <- predict(model, 
                  type = "class",
                  newdata = filter(class_df, split == 0) %>% select(-quantile_group))

(con <- table(y_pred, filter(class_df, split == 0) %>% pull(quantile_group)))
##        
#y_pred           high    low        not_applicable (canon)
#high           1972      1126        22
#low             111      134         0
#not_applicable    1      0           21

sum(diag(con)) / sum(con)
## [1] 0.6279894
## ^ not as much improvement here as i saw with subreddit??

##### COSINE MAYBE?!?!?!?! #####

## NOTE: Original source ends here. They didn't discuss document comparison at all, so I had to dig around to figure out what the next steps might be.

## For the remainder, I used this source:

## https://www.r-bloggers.com/using-cosine-similarity-to-find-matching-documents-a-tutorial-using-senecas-letters-to-his-friend-lucilius/

## The source linked above uses a TF-IDF matrix to make their sparse matrix; i think i used a PCA matrix??? Either way, I started with something different than they did. 

##So I'm not sure if what I ended up with is actually making a cosine matrix (embeddings? i don't know the terms). But the actual comparison work looks/sounds like what we've been discussing, so i'm hoping this isn't completely off-base?

similarities <- sim2(sparse_word_matrix, method = "cosine", norm = "l2") 

sparse_word_matrix[1:10, 1:4]
#saveRDS(similarities, "word_similarities_dfCmatrix.drs")

# next few lines I'm just testing if it worked:
# i added a function to the original code because I wanted to see the actual text in addition to the document names


similarities <- sim2(sparse_word_matrix, method = "cosine", norm = "l2") 
sparse_word_matrix[1:10, 1:4]
#saveRDS(similarities, "SIMILARITIES_COSINE_MATRIX.RDS")


add_text <- function(df, numberch) {
  fname <- df$similarities.id.[numberch]
  ftxt <- as.character(x$text[x$id==fname])
  df$text[numberch] <- ftxt
  return(df)}

get_similar_docs <- function(similarities, id, n_recommendations = 3){
  sims <- sort(similarities[id, ], decreasing = TRUE)[1:(2 + n_recommendations)]
  sims <- data.frame(similarities[id], sims)
  sims$similarities.id. <- rownames(sims)
  sims$text <- "words"
  for (i in 1:5) {
    sims <- add_text(sims,i)}
  return(sims)
  #sort(similarities[id, ], decreasing = TRUE)[1:(2 + n_recommendations)]
}

test_comparison <- get_similar_docs(similarities, 10)
test_comparison

##### HOW TO MAKE THE DATA MANAGEABLE??? #####

## I have NO IDEA how to compare 17k texts against 17k texts; and this is less than 0.38% of the original set of comments. 

## I am able to get three types of objects:

# 1) a "long" df (46373601 obs of 3 variables)
summ <- summary(similarities)

df <-data.frame(Origin = rownames(similarities)[summ$i],
                Destination = colnames(similarities)[summ$j],
                Weight      = summ$x)

df <- df[df$Weight != 1.0000000,]


# 2) a matrix

df <- as.matrix(similarities)

# 3) a 17k by 17k df
df <- as.data.frame(as.matrix(similarities))


## They're too big; i have no idea what to do with them?? Like--how to aggregate? I am literally "?????????????" at this point.  

































