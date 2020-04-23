# FUNCTIONS ---------------------------------------------------------


#---------------------------------------------------------
remove_empty <- function(df) {
  df <- df %>% select(score, author, year, subreddit, id, body) %>% 
    filter(subreddit != "") %>% 
    filter(subreddit != "2.0") %>% 
    filter(subreddit != "4.0") %>% 
    filter(author != "[deleted]") %>% 
    filter(author != "[removed]") %>% 
    filter(body   != "[deleted]") %>% 
    filter(body   != "[removed]") %>% 
    filter(body   != "")
  return(df)}


#---------------------------------------------------------
quantile_filter <- function(df, up_q, low_q, perc) {
  val <- nrow(df[df$score < quantile(df$score,low_q),])
  high <- df[df$score > quantile(df$score,up_q),]
  high$quantile_group <- "high"
  high <- high %>% sample_frac(perc)
  if(val >= 3) {
    low <- df[df$score < quantile(df$score,low_q),]
    low$quantile_group <- "low"
    low <- low %>% sample_frac(perc)
    d.f <- rbind(low, high)
    return(d.f)}}

#---------------------------------------------------------
subset_quantiles <- function(df, up_q, low_q, perc) {
  subnames <- unique(df$subreddit)
  df <- df %>% select(body, id, subreddit, year, score, author)
  
  subset_the_subreddits <- function(x) {
    print(paste0("working on r/", x))
    tmp <- df %>% filter(subreddit==x)
    sub <- quantile_filter(tmp, up_q, low_q, perc)
    return(sub)}
  
  d.f <- bind_rows(lapply(subnames,subset_the_subreddits))
  colnames(d.f) <- c("text", "id", "subreddit", "year", "score", "author", "quantile_group")
  d.f$class <- "reddit"
  return(d.f)}

#---------------------------------------------------------
combine_and_clean <- function(commentDF, txtDF) {
  commentDF <- commentDF %>% select(-score)
  txtDF <- txtDF %>% 
    select(text, doc_id) %>% 
    rename(id = doc_id)
  
  txtDF$subreddit <- "canon"
  txtDF$year <- "not_applicable"
  txtDF$author <- "not_applicable"
  txtDF$quantile_group <- "not_applicable"
  txtDF$class <- "canon"
  
  x <- rbind(commentDF, txtDF)
  
  x$text <- gsub("&gt;", " ", x$text)
  x$text <- gsub("[[:punct:]]", " ", x$text) 
  x$text <- iconv(x$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  x$text <- tolower(x$text)
  
  x.clean <- x %>%
    mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
    select(id, subreddit, quantile_group, year, text, author)
  
  x.clean <- x.clean[!is.na(x.clean$text),]
  return(x.clean)}

#---------------------------------------------------------
# from Julia Silge
# originally found at: https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html
search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)}
















