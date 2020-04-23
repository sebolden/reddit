# GENERAL -----------------------------------------------------------------
"remove_empty(df)
 getOrderedNames()
 getColors"

remove_empty <- function(df) {
  df <- df %>% select(score, author, date, year, subreddit, id, body) %>% 
    filter(subreddit != "") %>% 
    filter(subreddit != "2.0") %>% 
    filter(subreddit != "4.0") %>% 
    filter(subreddit != "creepsupport") %>% 
    filter(author != "[deleted]") %>% 
    filter(author != "[removed]") %>% 
    filter(body   != "[deleted]") %>% 
    filter(body   != "[removed]") %>% 
    filter(body   != "")
  df$date <- as.Date(df$date)
  df$monthyear <- format(df$date, "%Y-%m")
  df$class <- 'none'
  df$class[df$subreddit=="altTRP"] <- 'red'
  df$class[df$subreddit=="GEOTRP"] <- 'red'
  df$class[df$subreddit=="redpillbooks"] <- 'red'
  df$class[df$subreddit=="redpillmedia"] <- 'red'
  df$class[df$subreddit=="asktrp"] <- 'red'
  df$class[df$subreddit=="redpillmusic"] <- 'red'
  df$class[df$subreddit=="RedPillParenting"] <- 'red'
  df$class[df$subreddit=="RedPillWomen"] <- 'red'
  df$class[df$subreddit=="RedPillWives"] <- 'red'
  df$class[df$subreddit=="ThankTRP"] <- 'red'
  df$class[df$subreddit=="TheRedPill"] <- 'red'
  df$class[df$subreddit=="TRPCore"] <- 'red'
  df$class[df$subreddit=="trpgame"] <- 'red'
  df$class[df$subreddit=="trpofftopic"] <- 'red'
  df$class[df$subreddit=="bropill"] <- 'blue'
  df$class[df$subreddit=="exredpill"] <- 'blue'
  df$class[df$subreddit=="FeMRADebates"] <- 'blue'
  df$class[df$subreddit=="MaleSupportNetwork"] <- 'blue'
  df$class[df$subreddit=="MensLib"] <- 'blue'
  df$class[df$subreddit=="PunchingMorpheus"] <- 'blue'
  df$class[df$subreddit=="socialjustice101"] <- 'blue'
  df$class[df$subreddit=="SocJus"] <- 'blue'
  df$class[df$subreddit=="WitchesVsPatriarchy"] <- 'blue'
  df$class[df$subreddit=="exredpill"] <- 'blue'
  print(unique(df$subreddit[df$class=='blue']))
  print(unique(df$subreddit[df$class=='red']))
  return(df)}

getOrderedNames <- function() {
  fnames <- c(  "altTRP", "asktrp", "GEOTRP", "redpillbooks"
                , "redpillmedia", "redpillmusic", "RedPillParenting", "RedPillWomen"
                , "RedPillWives", "ThankTRP", "TheRedPill", "TRPCore"
                , "trpgame", "trpofftopic", "bropill", "exredpill"
                , "FeMRADebates", "MaleSupportNetwork", "MensLib", "PunchingMorpheus"
                , "socialjustice101", "SocJus", "WitchesVsPatriarchy")
  return(fnames)}

getColors <- function(bluev, redv, equalv) {
  color.list <- c(bluev = "#003f5c", redv = "#ff6361", equalv = "#bc5090")
  return(color.list)}


# NETWORK MEMBERSHIP  -------------------------------------------------------
getMembership <- function(df) {
  df <- data %>% select(author, year, date, monthyear, class, subreddit)
  authorGrouping <-  df %>% 
    group_by(author, monthyear, class) %>%
    summarise(weight=n()) %>%
    arrange(author)
  red <- authorGrouping %>% 
    filter(class=="red") %>% 
    group_by(author, monthyear)  %>% 
    rename(red_weight=weight)
  blue <- authorGrouping %>% 
    filter(class=="blue") %>% 
    group_by(author, monthyear) %>% 
    rename(blue_weight=weight)
  authorGrouping <- red %>% 
    full_join(blue, by=c("author", "monthyear"))
  authorGrouping <- authorGrouping %>% 
    select(author, monthyear, red_weight, blue_weight) %>% 
    rename(rwt=red_weight) %>% rename(bwt=blue_weight) %>% 
    rename(date = monthyear)
  authorGrouping$bwt[is.na(authorGrouping$bwt)] <- 0
  authorGrouping$rwt[is.na(authorGrouping$rwt)] <- 0
  authorGrouping$greater <- "equal"
  authorGrouping$greater[authorGrouping$bwt > authorGrouping$rwt] <- "blue"
  authorGrouping$greater[authorGrouping$rwt > authorGrouping$bwt] <- "red"
  return(authorGrouping)}



# TEXT ANALYSIS -----------------------------------------------------------
"quantile_filter(df, up_q, low_q, perc)
 subset_quantiles(df, up_q, low_q, perc)
 combine_and_clean(commentDF, textDF)
 search_synonyms(word_vectors, selected_vector)"

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

# from Julia Silge
# originally found at: https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html
search_synonyms <- function(word_vectors, selected_vector) {
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames, similarity = unrowname.x.)
  similarities %>% arrange(-similarity)}
