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

makeBabyDF <- function(df) {
  # subset to only include the very most bare minimum variables
  df <- df %>% select(date, year, author, id, link_id, parent_id, subreddit, score, body) %>% 
    filter(author != "[deleted]") %>% 
    filter(author != "[removed]") %>% 
    filter(body   != "[deleted]") %>% 
    filter(body   != "[removed]") %>% 
    filter(body   != "")
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

getYearlyMetrics <- function() {
  mod <- getModByYear()
  deg <- getDegreeByYear()
  df <- mod %>% left_join(deg, by = c('year', 'subreddit'))
  return(df)}

makeGraph<-function(data) {
  g <- graph_from_data_frame(data, directed=F)
  V(g)$degree_centrality <- centr_degree(g)$res
  return(g)}

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
  forum.rel <- getRelationships(linkedDF)
  relationships.graph <- makeGraph(forum.rel)
  relationships.modularity <- cluster_louvain(relationships.graph)
  relationships.mean <- mean(relationships.modularity$modularity)
  return(relationships.mean)}

replies_modularity <- function(linkedDF) {
  forum.rep <- weightByOccurrence(linkedDF)
  replies.graph <- makeGraph(forum.rep)
  replies.modularity <- cluster_louvain(replies.graph)
  replies.mean <- mean(replies.modularity$modularity)
  return(replies.mean)}

# NETWORK MEMBERSHIP  -------------------------------------------------------
getMembership <- function(df) {
  z <- df %>% select(author, year, class, subreddit) %>% rename(date=year)
  q <-  z %>% 
    group_by(author, date, class, subreddit) %>%
    summarise(sub_weight=n())
  red <- q %>% filter(class=="red")
  blue <- q %>% filter(class=="blue") 
  red$rwt <- red$sub_weight
  blue$bwt <- blue$sub_weight
  red <- red %>% select(-sub_weight)
  blue <- blue %>% select(-sub_weight)
  x <- red %>% full_join(blue, by=c("author", "date")) %>% 
    select(author, date, rwt, bwt)
  x$bwt[is.na(x$bwt)] <- 0
  x$rwt[is.na(x$rwt)] <- 0
  x$greater <- "equal"
  x$greater[x$bwt > x$rwt] <- "blue"
  x$greater[x$rwt > x$bwt] <- "red"
  x$total <- (x$rwt+x$bwt)
  x$prop.red <- round((x$rwt/x$total),digits=2)
  x$prop.blue <- round((x$bwt/x$total),digits=2)
  return(x)}

# MODULARITY --------------------------------------------------------------

modByYear <- function(x) {
  tmp <- linx %>% filter(year==x)
  fname <- unique(tmp$subreddit)
  modDF <- data.frame(numeric(0),numeric(0),numeric(0),character(0), stringsAsFactors=F)
  year <- x
  colnames(modDF) <- c("rel_modularity", "rep_modularity", "year", "subreddit")
  for (i in 1:26) {
    rep.avg <- replies_modularity(tmp[tmp$subreddit==fname[i],])
    rel.avg <- relationships_modularity(tmp[tmp$subreddit==fname[i],])
    modDF[nrow(modDF)+1, ] <- c(rel.avg, rep.avg, year, fname[i])}
  modDF$rel_modularity <- as.numeric(modDF$rel_modularity)
  modDF$rep_modularity <- as.numeric(modDF$rep_modularity)
  return(modDF)}

getModByYear <- function() {
  mdf15 <- modByYear(2015)
  mdf16 <- modByYear(2016)
  mdf17 <- modByYear(2017)
  mdf18 <- modByYear(2018)
  mdf19 <- modByYear(2019)
  mdf20 <- modByYear(2020)
  mdf <- rbind(mdf15, mdf16, mdf17, mdf18, mdf19, mdf20)
  mdf <- na.omit(mdf)
  return(mdf)}


# DEGREE ------------------------------------------------------------------
degreeByYear <- function(x) {
  tmp <- linx %>% filter(year==as.character(x))
  fname <- unique(tmp$subreddit)
  degDF <- data.frame(numeric(0), numeric(0),character(0), stringsAsFactors=F)
  colnames(degDF) <- c("year", "degree", "subreddit")
  for (i in 1:26) {
    sub <- tmp[tmp$subreddit==fname[i],]
    print(unique(sub$subreddit))
    g <- graph_from_data_frame(sub)
    fn <- unique(sub$subreddit)
    deg.avg <- mean(degree(g))
    degDF[nrow(degDF)+1, ] <- c(x, deg.avg, fn)}
  degDF$year <- as.numeric(degDF$year)
  degDF$degree <- as.numeric(degDF$degree)
  return(degDF)}

getDegreeByYear <- function() {
  mdf15 <- degreeByYear(2015)
  mdf16 <- degreeByYear(2016)
  mdf17 <- degreeByYear(2017)
  mdf18 <- degreeByYear(2018)
  mdf19 <- degreeByYear(2019)
  mdf20 <- degreeByYear(2020)
  mdf <- rbind(mdf15, mdf16, mdf17, mdf18, mdf19, mdf20)
  mdf <- na.omit(mdf)
  return(mdf)}

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

combineCommentsAndCanon <- function(commentDF, canonDF) {
  commentDF <- commentDF %>% select(-score)
  canonDF <- canonDF %>% 
    select(text, doc_id) %>% 
    rename(id = doc_id)
  canonDF$subreddit <- "canon"
  canonDF$year <- "not_applicable"
  canonDF$author <- "not_applicable"
  canonDF$quantile_group <- "not_applicable"
  canonDF$class <- "canon"
  x <- rbind(commentDF, canonDF)
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












