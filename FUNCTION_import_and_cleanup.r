


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
