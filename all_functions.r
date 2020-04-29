# GENERAL -----------------------------------------------------------------
getcsv <- function(string) {
  csv <- read.csv(string, header=T, stringsAsFactors = F)}

keep_remove <- function(string) {rm(list=setdiff(ls(), string))} 

getComments <- function() {
  comments <- getcsv('COMMENT_CSVS_BABY.csv')
  return(comments)}

getColors <- function() {
  color.list <- c(blue = "#003f5c", red = "#ff6361")
  return(color.list)}

# IMPORT AND CLEANUP  -------------------------------------------------------
cleanAllComments <- function(all_data) {
  # removing columns, e.g. those that aren't of interest; those with majority NA
  all_data <- all_data %>% select(-c(author_flair_background_color, author_flair_css_class, author_flair_richtext, author_flair_template_id, author_flair_type, author_patreon_flair, associated_award, collapsed_because_crowd_control, author_cakeday, steward_reports, can_gild, subreddit_name_prefixed, subreddit_type, rte_mode, treatment_tags, subreddit_id, reply_delay, nest_level, created))
  # add binary columns that classify whether a comment has been deleted or removed
  all_data$deleted <- 0
  all_data$removed <- 0
  all_data$deleted[all_data$body=="[deleted]"] <- 1
  all_data$removed[all_data$body=="[removed]"] <- 1
  all_data$deleted <- as.factor(all_data$deleted)
  all_data$removed <- as.factor(all_data$removed)
  # the next chunk of code just cleans up factor/dummy variables
  # i end up ditching most of them 
  # but the code remains in case i ever return to them
  all_data$author_premium <- as.character(all_data$author_premium)
  all_data$author_premium[all_data$author_premium=="True"] <- 1
  all_data$author_premium[all_data$author_premium=="False"] <- 0
  all_data$author_premium[all_data$author_premium==""] <- 0
  all_data$author_premium <- as.factor(all_data$author_premium)
  all_data$is_submitter[all_data$is_submitter=="True"] <- 1
  all_data$is_submitter[all_data$is_submitter=="False"] <- 0
  all_data$is_submitter[all_data$is_submitter==""] <- 0
  all_data$is_submitter <- as.factor(all_data$is_submitter)
  all_data$distinguished[all_data$distinguished=="Moderator"] <- 1
  all_data$distinguished[all_data$distinguished==""] <- 0
  all_data$distinguished <- as.factor(all_data$distinguished)
  all_data$locked[all_data$locked=="True"] <- 1
  all_data$locked[all_data$locked=="False"] <- 0
  all_data$locked[all_data$locked==""] <- 0
  all_data$locked <- as.factor(all_data$locked)
  all_data$send_replies[all_data$send_replies=="True"] <- 1
  all_data$send_replies[all_data$send_replies=="False"] <- 0
  all_data$send_replies[all_data$send_replies=="0.0"] <- 0
  all_data$send_replies[all_data$send_replies==""] <- 0
  all_data$send_replies <- as.factor(all_data$send_replies)
  all_data$subreddit <- as.factor(all_data$subreddit)
  all_data$no_follow[all_data$no_follow=="True"] <- 1
  all_data$no_follow[all_data$no_follow=="False"] <- 0
  all_data$no_follow[all_data$no_follow=="1397532812.0"] <- 0
  all_data$no_follow[all_data$no_follow==""] <- 0
  all_data$no_follow <- as.factor(all_data$no_follow)
  all_data$stickied[all_data$stickied=="True"] <- 1
  all_data$stickied[all_data$stickied=="False"] <- 0
  all_data$stickied[all_data$stickied=="0.0"] <- 0
  all_data$stickied[all_data$stickied==""] <- 0
  all_data$stickied <- as.factor(all_data$stickied)
  all_data$collapsed[all_data$collapsed=="True"] <- 1
  all_data$collapsed[all_data$collapsed=="False"] <- 0
  all_data$collapsed[all_data$collapsed==""] <- 0
  all_data$collapsed <- as.factor(all_data$collapsed)
  all_data$controversiality <- as.factor(all_data$controversiality)
  all_data$mod_removed[all_data$mod_removed=="True"] <- 1
  all_data$mod_removed[all_data$mod_removed==""] <- 0
  all_data$mod_removed <- as.factor(all_data$mod_removed)
  all_data$user_removed[all_data$user_removed=="True"] <- 1
  all_data$user_removed[all_data$user_removed==""] <- 0
  all_data$user_removed <- as.factor(all_data$user_removed)
  all_data$score_hidden[all_data$score_hidden=="True"] <- 1
  all_data$score_hidden[all_data$score_hidden=="False"] <- 0
  all_data$score_hidden[all_data$score_hidden==""] <- 0
  all_data$score_hidden <- as.factor(all_data$score_hidden)
  all_data$is_edited <- 0
  all_data$is_edited[!is.na(all_data$edited)] <- 1
  all_data <- all_data %>% select(-edited)
  all_data$is_mod <- all_data$distinguished
  all_data <- all_data %>% select(-distinguished)
  # convert utc to readable dates
  all_data$date <- as.POSIXct(all_data$created_utc, origin="1970-01-01")
  # add a column that specifies year only
  all_data$year <- year(all_data$date)
  # don't need the epoch version anymore
  all_data <- all_data %>% select(-created_utc)
  # i have way too much data so it's easier to just work with the absolute essentials
  # but skip this step if yr computer's RAM can tolerate it, i guess
  cleaned <- all_data %>% select(date,year,subreddit,author,score,id, parent_id,link_id,body,author_flair_text,author_fullname,gildings,is_submitter,locked,stickied,controversiality,is_edited,is_mod,deleted,removed)
  cleaned <- cleaned %>% filter(subreddit != "") %>% 
    filter(subreddit != "2.0") %>% 
    filter(subreddit != "4.0") %>% 
    
  return(cleaned)}

makeBabyDF <- function(df) {
  # subset to only include the very most bare minimum variables
  df <- df %>% select(date, year, author, id, link_id, parent_id, subreddit, score, body) %>% 
    filter(author != "[deleted]") %>% 
    filter(author != "[removed]") %>% 
    filter(body   != "[deleted]") %>% 
    filter(body   != "[removed]") %>% 
    filter(body   != "")
  return(df)}


combineCommentsAndCanon <- function(commentDF, txtDF) {
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
  x$text <- gsub("http", " ", x$text)
  x$text <- gsub("www", " ", x$text)
  x$text <- gsub("https", " ", x$text)
  x$text <- gsub("amp", " ", x$text)
  x$text <- gsub("[[:punct:]]", " ", x$text) 
  x$text <- iconv(x$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  x$text <- tolower(x$text)
  x.clean <- x %>%
    mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
    select(id, subreddit, quantile_group, year, text, author)
  x.clean <- x.clean[!is.na(x.clean$text),]
  return(x.clean)}



# NETWORK MEMBERSHIP  -------------------------------------------------------
getMonthYearMembership <- function(df) {
  df <- data %>% select(author, year, date, monthyear, class, subreddit)
  x <-  df %>% 
    group_by(author, monthyear, class) %>%
    summarise(weight=n()) %>%
    arrange(author)
  red <- x %>% 
    filter(class=="red") %>% 
    group_by(author, monthyear)  %>% 
    rename(red_weight=weight)
  blue <- x %>% 
    filter(class=="blue") %>% 
    group_by(author, monthyear) %>% 
    rename(blue_weight=weight)
  x <- red %>% 
    full_join(blue, by=c("author", "monthyear"))
  x <- x %>% 
    select(author, monthyear, red_weight, blue_weight) %>% 
    rename(rwt=red_weight) %>% rename(bwt=blue_weight) %>% 
    rename(date = monthyear)
  x$bwt[is.na(x$bwt)] <- 0
  x$rwt[is.na(x$rwt)] <- 0
  x$greater <- "equal"
  x$greater[x$bwt > x$rwt] <- "blue"
  x$greater[x$rwt > x$bwt] <- "red"
  x$year <- stri_extract_first_regex(x$date, "^[^-]+")
  x$year <- as.Date(x$year, "%y")
  x$year <- year(x$year)
  return(x)}

getYearMembership <- function(df) {
  xr <- df %>% filter(greater=='red') %>% group_by(author,year) %>% summarise(red_wt = sum(rwt))
  xb <- df %>% filter(greater=='blue') %>% group_by(author,year) %>% summarise(blue_wt = sum(bwt))
  xe <- df %>% filter(greater=='equal') %>% group_by(author,year) %>% summarise(red_wt = sum(rwt), blue_wt=sum(bwt))
  
  xx <- xr %>% full_join(xb, by=c("author","year"))
  xx <- xx %>% full_join(xe, by=c("author","year"))
  colnames(xx) <- c('author', 'year', 'rwt', 'bwt', 'rwt1', 'bwt1')
  xx$rwt[is.na(xx$rwt)] <- 0
  xx$bwt[is.na(xx$bwt)] <- 0
  xx$rwt1[is.na(xx$rwt1)] <- 0
  xx$bwt1[is.na(xx$bwt1)] <- 0
  
  xx$rwt3 <- xx$rwt+xx$rwt1
  xx$bwt3 <- xx$bwt+xx$bwt1
  
  xx <- xx %>% select(author, year, rwt3, bwt3) %>%
    rename(bwt = bwt3) %>% rename(rwt=rwt3)
  
  xx$greater <- "equal"
  xx$greater[xx$bwt > xx$rwt] <- "blue"
  xx$greater[xx$rwt > xx$bwt] <- "red"
  return(xx)}

getProps <- function(df) {
  df$total <- (df$rwt+df$bwt)
  df$prop.red <- round((df$rwt/df$total),digits=2)
  df$prop.blue <- round((df$bwt/df$total),digits=2)
  return(df)}

# USER CROSSOVER  -------------------------------------------------------
xover_single <- function(UYstr,str) {
  ux <- unique(data$author[data$subreddit==str]) 
  uy <- unique(data$author[data$subreddit==UYstr]) 
  val <- length(which(ux %in% uy))
  uy_length <- length(uy)
  perc <- ((val/uy_length)*100)
  obj <- tibble(x_y=0, perc=0, x=0, y=0)
  obj$x_y <- val
  obj$perc <- perc
  obj$x <- str
  obj$y <- UYstr
  return(obj)}

getCrossoverDF <- function(df) {
  subnames <- as.character(unique(df$subreddit))
  f <- function(x) {
    str1 <- x
    print(str1)
    snames <- as.character(unique(df$subreddit))
    d.f <- bind_rows(lapply(snames,xover_single, str=str1))
    return(d.f)}
  df_final <- bind_rows(lapply(subnames,f))
  df_final <- df_final %>% select(y, everything()) %>% select(x, everything())
  return(df_final)}

getCrossoverMatrix <- function(df) {
  print("friendly reminder: this version of the matrix replaces 100.00 values with 0 to avoid within-subreddit cluttering in visualizations. remove the line before the return(xover) statement in the getCrossoverMatrix() function to keep 100.00 values.")
  #df <- userdf(df)
  df <- df %>% select(-'x_y') 
  df <- pivot_wider(df, names_from = x, values_from = perc)
  df <- as.data.frame(df)
  rownames(df) <- df$y
  df <- df %>% select(-y) 
  df <- as.matrix(df)
  df <- round(df, digits=2)
  df <- ifelse(df==100.00,0,df)
  return(df)}

# TEXT ANALYSIS -----------------------------------------------------------

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
  df <- df %>% select(body, id, subreddit, year, score, author)
  df$nchar <- nchar(df$body)
  df <- df[df$subreddit != "SocJus",]
  df <- df[df$subreddit != "redpillmedia",]
  df <- df[df$subreddit != "trpgame",]
  subnames <- unique(df$subreddit)
  df <- df[df$nchar > 1000,]
  subset_the_subreddits <- function(x) {
    print(paste0("working on r/", x))
    tmp <- df %>% filter(subreddit==x)
    sub <- quantile_filter(tmp, up_q, low_q, perc)
    return(sub)}
  d.f <- bind_rows(lapply(subnames,subset_the_subreddits))
  d.f <- d.f %>% select(-nchar)
  colnames(d.f) <- c("text", "id", "subreddit", "year", "score", "author", "quantile_group")
  d.f$class <- "reddit"
  return(d.f)}
















