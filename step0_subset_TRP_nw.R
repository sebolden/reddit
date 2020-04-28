data <- getcsv('trp_nw.csv')

subsetnames <- c("bropill", "PunchingMorpheus", "againstmensrights", "TheBluePill", "feminismformen", "TheMensCooperative", "MaleSupportNetwork", "MensLib","exredpill", "FeMRADebates", "asktrp", "RedPillRetention", "RedPillWorkplace", "marriedredpill", "RedPillNonMonogamy", "altTRP", "TRPOfftopic", "ThankTRP", "redpillbooks", "redpillmedia", "redpillmusic", "RedPillParenting", "RedPillWomen", "RedPillWives", "TheRedPill", "pussypassdenied", "TRPOffTopic")


data <- data[data$subreddit %in% subsetnames,]
rm(subsetnames)

data <- data %>% select(
  subreddit, date, year, score, author
  , deleted, removed
  , is_submitter, locked, stickied, is_edited, is_mod
  , author_flair_text, body
  ,id, parent_id, link_id)

data <- data %>% 
  filter(body != "[deleted]") %>% 
  filter(body!="[removed]") %>% 
  filter(author!="[deleted]")

data$col <- getGroups(data)
#write.csv(data, "trp_nw.csv', row.names=F)