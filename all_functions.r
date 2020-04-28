# GENERAL -----------------------------------------------------------------
getcsv <- function(string) {
  csv <- read.csv(string, header=T, stringsAsFactors = F)}

keep_remove <- function(string) {rm(list=setdiff(ls(), string))} 

getColors <- function() {color.list <- c(blue = "#003f5c", red = "#ff6361")
  return(color.list)}

getGroups <- function(df) {
  bb <- c("bropill", "PunchingMorpheus", "againstmensrights", "TheBluePill", "feminismformen"
          , "TheMensCooperative", "MaleSupportNetwork", "MensLib","exredpill", "FeMRADebates")
  rr <- c("asktrp", "RedPillRetention", "RedPillWorkplace", "marriedredpill", "RedPillNonMonogamy", "altTRP"
          , "TRPOfftopic", "ThankTRP", "redpillbooks", "redpillmedia", "redpillmusic", "RedPillParenting"
          , "RedPillWomen", "RedPillWives", "TheRedPill", "pussypassdenied", "TRPOffTopic")
  df$co[df$subreddit %in% bb] <- "blue"
  df$co[df$subreddit %in% rr] <- "red"
  return(df)}

makeBabyDF <- function(df) {
  print('heads up: removing text data')
  print('heads up: removing deleted users and content')
  df <- df %>% 
    select(date, year, author, id, link_id, parent_id, subreddit, score, author_flair_text, gildings
           , is_submitter, locked, stickied, controversiality, is_edited, is_mod) %>% 
    filter(subreddit != "") %>% filter(subreddit != "2.0") %>% filter(subreddit != "4.0") %>% 
    filter(author != "[deleted]") %>% filter(author != "[removed]") %>% 
    #filter(body != "[deleted]") %>% filter(body != "[removed]") %>% filter(body != "") %>% 
    filter(year != 2014)
  return(df)}

# BASIC NETWORK STUFF -------------------------------------------------------
getLinks <- function(df) {
  df$parent_id <- str_remove(df$parent_id, "t3_")
  df$parent_id <- str_remove(df$parent_id, "t1_")
  c.author <- df %>% select(id, author) %>% rename(target_author=author)
  c.target <- df %>% select(id, parent_id, author) 
  output <- c.target %>% left_join(c.author, by=c("parent_id"="id"))
  colnames(output) <- c("original_comment_id", "parent_id", "parent_author", "target_author")
  tmp <- tibble(output$original_comment_id, output$target_author)
  colnames(tmp) <- c("id", "target_author")
  tmp$id <- as.character(tmp$id)
  df$id <- as.character(df$id)
  obj <- df %>% left_join(tmp, by=c('id'))
  obj <- obj[!duplicated(obj$id),]
  obj <- obj[!is.na(obj$target_author),]
  obj <- obj %>% select(target_author, everything()) %>% 
    select(author, everything()) %>% 
    rename(from = author) %>% 
    rename(to = target_author)
  return(obj)}
  
modByYear <- function(x) {
  tmp <- linx %>% filter(year==x)
  fname <- unique(tmp$subreddit)
  modDF <- data.frame(numeric(0),numeric(0),numeric(0),character(0), stringsAsFactors=F)
  year <- x
  colnames(modDF) <- c("rel_modularity", "rep_modularity", "subreddit")
  for (i in 1:26) {
    rep.avg <- replies_modularity(tmp[tmp$subreddit==fname[i],])
    rel.avg <- relationships_modularity(tmp[tmp$subreddit==fname[i],])
    modDF[nrow(modDF)+1, ] <- c(rel.avg, rep.avg, year, fname[i])}
  modDF$rel_modularity <- as.numeric(modDF$rel_modularity)
  modDF$rep_modularity <- as.numeric(modDF$rep_modularity)
  return(modDF)}

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


# JOSHS FUNCTIONS ---------------------------------------------------------

weightByOccurrence<-function(data) {
  data$to <- as.character(data$to)
  data %>% rename(src=from) %>% rename(target=to) %>% 
      mutate(direction = pmin(src,target) == src)%>%
      mutate(s2 = pmin(src,target),target = pmax(src,target),src=s2) %>%    
      group_by(src,target) %>%
      summarise(weight=n()) %>%
      ungroup() %>%
      arrange(-weight)}
  
getRelationships <- function(df) {
  tmp <- df %>% rename(src=from) %>% rename(target=to)
  tmp$src <- as.character(tmp$src)
  tmp$target <- as.character(tmp$target)
  tmp <- tmp %>% mutate(direction = pmin(src,target) == src)%>%
    mutate(s2 = pmin(src,target),target = pmax(src,target),src=s2) %>%
    group_by(link_id,src,target) %>%
    summarise(weight = min(sum(direction),sum(!direction))) %>% filter(weight > 0) %>% ungroup() %>%
    group_by(src,target) %>% summarise(weight=sum(weight)) %>% arrange(-weight)
  return(tmp)}

relationships_modularity <- function(linkedDF) {
  forum.rel <- getRelationships(linkedDF) ## ** MIGHT NOT BE THE RIGHT FUNCTION **
  relationships.graph <- makeGraph(forum.rel)
  relationships.modularity <- cluster_louvain(relationships.graph)
  relationships.mean <- mean(relationships.modularity$modularity)
  return(relationships.mean)}

replies_modularity <- function(linkedDF) {
  forum.rep <- weightByOccurrence(linkedDF) ## ** MIGHT NOT BE THE RIGHT FUNCTION **
  replies.graph <- makeGraph(forum.rep)
  replies.modularity <- cluster_louvain(replies.graph)
  replies.mean <- mean(replies.modularity$modularity)
  return(replies.mean)}

makeGraph<-function(data) {
    g <- graph_from_data_frame(data, directed=F)
    V(g)$degree_centrality <- centr_degree(g)$res
    return(g)}

# TEXT ANALYSIS -----------------------------------------------------------

quantile_filter <- function(df, up_q, low_q, perc) {
  set.seed(12345)
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
  # remove subreddits who won't meet any of the quantile filtering criteria
  df <- df %>% filter(subreddit !="redpillmedia") %>% 
    filter(subreddit != "trpgame") %>% 
    filter(subreddit != "TheMensCooperative") %>% 
    filter(subreddit != "RedPillNonMonogamy")
  # get a list of unique subreddit names
  subnames <- unique(df$subreddit)
  # work w just the essentials
  # add # of characters/comment to DF
  df$nchar <- nchar(df$body)
  # only keep comments with more than 1k char
  df <- df[df$nchar > 1000,] 
      # run e/t through the quantiles function
      subset_the_subreddits <- function(x) {
        print(paste0("working on r/", x))
        tmp <- df %>% filter(subreddit==x)
        sub <- quantile_filter(tmp, up_q, low_q, perc)
        return(sub)}
  # combine all the subreddit data
  d.f <- bind_rows(lapply(subnames,subset_the_subreddits))
  # don't need character # anymore
  d.f <- d.f %>% select(-nchar)
  # fix colnames
  colnames(d.f) <- c("text", "id", "subreddit", "year", "score", "author", "quantile_group")
  return(d.f)}

combine_and_clean <- function(commentDF, txtDF) {
  commentDF <- commentDF %>% select(-score)
  txtDF <- txtDF %>% 
    select(text, doc_id) %>% 
    rename(id = doc_id)
  txtDF$subreddit <- "canon"
  txtDF$year <- "canon"
  txtDF$author <- "canon"
  txtDF$quantile_group <- "canon"
  x <- rbind(commentDF, txtDF)
  x$text <- gsub("&gt;", " ", x$text)
  x$text <- gsub("http", " ", x$text)
  x$text <- gsub("www", " ", x$text)
  x$text <- gsub("https", " ", x$text)
  x$text <- gsub("â€“", " ", x$text)
  x$text <- gsub("â€œ", " ", x$text)
  x$text <- gsub("â€™", "", x$text)
  x$text <- gsub("amp", " ", x$text)
  x$text <- gsub("[[:punct:]]", "", x$text) 
  x$text <- tolower(x$text)
  x$text <- trimws(x$text)
  x.clean <- x %>%
    mutate(text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
    select(id, subreddit, quantile_group, year, text, author)
  x.clean <- x.clean[!is.na(x.clean$text),]
  return(x.clean)}

getCorr <- function(df) {
  df <- df %>% select(id, everything()) %>% 
    select(class, everything()) %>% 
    select(-c(subreddit,quantile_group,author,year,co,logscore))
  rh <- df %>% filter(class=="red_high")
  rl <- df %>% filter(class=="red_low")
  bh <- df %>% filter(class=="blue_high")
  bl <- df %>% filter(class=="blue_low")
  
  mrh <- rh[,3:214]
  mbh <- bh[,3:214]
  mrl <- rl[,3:214]
  mbl <- bl[,3:214]

  crh <- as.data.frame(cor(mrh))
  crh <- crh[-1,]
  crl <- as.data.frame(cor(mrl))
  crl <- crl[-1,]
  cbh <- as.data.frame(cor(mbh))
  cbh <- cbh[-1,]
  cbl <- as.data.frame(cor(mbl))
  cbl <- cbl[-1,]
  
  cbl$class <- "blue_low"
  crl$class <- "red_low"
  cbh$class <- "blue_high"
  crh$class <- "red_high"
  
  tmp <- rbind(crh,cbh,crl,cbl)
  return(tmp)}


# IMPORT AND CLEANUP  -------------------------------------------------------
df_grouping <- function(all_data) {
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
  return(cleaned)}










