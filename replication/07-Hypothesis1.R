#==============================================================================
# 07-Hypothesis1.R
# Purpose: Check Hypothesis1 by comparing follow relationship and actions
# and create figures 4-1 and 4-2
# Author: Keisuke Idemitsu
#==============================================================================

library(dplyr)
library(ggplot2)
library(DBI)
library(stringr)
library(tidyr)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data"  ## designate data store folder


############ Step1: Check follow relationship ############

load(paste0(data.folder, "all_follow.rdata"))

## check whether each follower is following the opposite ideology opinion leaders or not
all_follow$follow <- rep(NA,nrow(all_follow))
all_follow$follow <- ifelse(all_follow$category == "Liberals" &
                              all_follow$conservative + all_follow$right_wing == 0, "zero",
                            ifelse(all_follow$category == "Liberals", "one or more",
                                   ifelse(all_follow$category %in% c("Conservatives","Netto uyoku") &
                                            all_follow$liberal == 0, "zero", "one or more")))
save(all_follow, file=paste0(data.folder, "all_follow.rdata"))

## create frequency table and omit Media followers
table_follow <- table(all_follow$follow, all_follow$category) %>% data.frame(stringsAsFactors = F)
table_follow <- table_follow[table_follow$Var2 != "Media",]

## create a bar chart
ggplot(table_follow) + geom_bar(aes(Var2, Freq, fill=Var1), stat="identity") +
  xlab("category") + 
  scale_x_discrete(labels=c("Liberal\nfollowers", "Conservative\nfollowers", "Netto-uyoku\nfollowers")) +
  scale_fill_brewer(palette = "Reds",name = "follow\nthe opposite") +
  theme_bw()
ggsave(paste0(figures.folder, "Figure 4-1 follow-zero.pdf"), width = 6, height = 3)



############ Step2: Prepare data ############

## pick up data from SQLite database
SQL.path <- paste0(data.folder, "tl_followers.sqlite")
db <- dbConnect(RSQLite::SQLite(), SQL.path)
tw2 <- dbGetQuery(db, "
 SELECT text, screen_name, user_id_str,
 in_reply_to_screen_name, in_reply_to_status_id_str, in_reply_to_user_id_str,
 expanded_url
                  FROM tl_followers")

## merge category data
load(paste0(data.folder, "all_follow.rdata"))
tw2 <- tw2 %>% left_join(all_follow[,c("id_str","category")], by=c("user_id_str"="id_str"))

save(tw2, file=paste0(data.folder, "tw2.rdata"))



############ Step3-1: Check interactions (retweet) ############

load(paste0(data.folder, "tw2.rdata"))

## extract row numbers of retweets
rows <- grep("^RT @[0-9_A-Za-z]+", tw2$text)

## add screen names of retweets destination
tw2$rt_screen_name <- rep(NA, nrow(tw2))
tw2$rt_screen_name[rows] <- str_extract(tw2$text[rows], '^RT @[0-9_A-Za-z]+') %>%
  gsub("^RT @", "", .)

## create a new dataframe which contain only retweets toward opinion leaders
rt_tw2 <- tw2[tolower(tw2$rt_screen_name) %in% tolower(freq_accounts$screen_name),
              c("screen_name","category","rt_screen_name","text")]
## add cluster data
rt_tw2 <- rt_tw2 %>%
  left_join(freq_accounts[,c("screen_name","cluster")], by=c("rt_screen_name"="screen_name"))
rt_tw2 <- na.omit(rt_tw2)

save(rt_tw2, file=paste0(data.folder, "rt_tw2.rdata"))

## spread data for each follower
rt_table <- spread(count(rt_tw2[,c("screen_name","cluster")], 
                         screen_name, cluster), cluster, n, fill = 0)
## merge category data
rt_table <- rt_table %>%
  left_join(rt_tw2[,c("screen_name","category")] %>% unique(), by="screen_name")

## check whether each follower is retweeting the opposite ideology opinion leaders or not
rt_table$rt <- rep(NA, nrow(rt_table))
rt_table$rt <- ifelse(rt_table$category=="Liberals" & 
                        rt_table$`2`+rt_table$`3`+rt_table$`5`==0, "zero",
                      ifelse(rt_table$category=="Liberals", "one or more",
                             ifelse(rt_table$category %in% c("Conservatives","Netto uyoku") &
                                      rt_table$`1`==0, "zero",
                                    ifelse(rt_table$category %in% c("Conservatives","Netto uyoku"), "one or more", NA))))

## create a dataframe to merge actions later
target_act <- all_follow[all_follow$id_str %in% targets,]
target_act <- target_act %>% left_join(rt_table[,c("screen_name","rt")], by="screen_name")

save(target_act, file=paste0(data.folder, "target_act.rdata"))



############ Step3-2: Check interactions (quote retweet) ############

## extract row numbers of quote retweets
rows <- grep("https://twitter.com/", tw2$expanded_url)

## add screen names of quote retweets destination (in lower case)
tw2$cite_twitter <- rep(NA, nrow(tw2))
tw2$cite_twitter[rows] <- sapply(strsplit(tw2$expanded_url[rows], "/"), "[", 4)

## create a new dataframe which contain only quote retweets toward opinion leaders
ct_tw2 <- tw2[tw2$cite_twitter %in% tolower(freq_accounts$screen_name),
              c("category","screen_name","cite_twitter","text")]

## add cluster data
freq_accounts$merge <- tolower(freq_accounts$screen_name)
ct_tw2 <- ct_tw2 %>%
  left_join(freq_accounts[,c("merge","cluster")], by=c("cite_twitter"="merge"))

save(ct_tw2, file=paste0(data.folder, "ct_tw2.rdata"))

## spread data for each follower
ct_table <- spread(count(ct_tw2[,c("screen_name","category","cluster")], 
                         screen_name, cluster), cluster, n, fill = 0)

## merge category data
ct_table <- ct_table %>%
  left_join(ct_tw2[,c("screen_name","category")] %>% unique(), by="screen_name")

## check whether each follower is retweeting the opposite ideology opinion leaders or not
ct_table$ct <- rep(NA, nrow(ct_table))
ct_table$ct <- ifelse(ct_table$category=="Liberals" & 
                        ct_table$`2`+ct_table$`3`+ct_table$`5`==0, "zero",
                      ifelse(ct_table$category=="Liberals", "one or more",
                             ifelse(ct_table$category %in% c("Conservatives","Netto uyoku") &
                                      ct_table$`1`==0, "zero",
                                    ifelse(ct_table$category %in% c("Conservatives","Netto uyoku"), "one or more", NA))))

## merge to dataframe
load(paste0(data.folder, "target_act.rdata"))
target_act <- target_act %>% left_join(ct_table[,c("screen_name","ct")], by="screen_name")

save(target_act, file=paste0(data.folder, "target_act.rdata"))



############ Step3-3: Check interactions (reply) ############

## extract row numbers of replies
rows <- grep("^@[0-9_A-Za-z]+", tw2$text)

## add screen names of replies destination
tw2$mt_screen_name <- rep(NA, nrow(tw2))
tw2$mt_screen_name[rows] <- str_extract(tw2$text[rows], '@[0-9_A-Za-z]+')
tw2$mt_screen_name <- gsub("@", "", tw2$mt_screen_name)

## create a new dataframe which contain only replies toward opinion leaders
mt_tw2 <- tw2[tolower(tw2$mt_screen_name) %in% tolower(freq_accounts$screen_name),
              c("category","screen_name","mt_screen_name","text")]
mt_tw2 <- mt_tw2 %>%
  left_join(freq_accounts[,c("screen_name","cluster")], by=c("mt_screen_name"="screen_name"))

save(mt_tw2, file=paste0(data.folder, "mt_tw2.rdata"))

## spread data for each follower
mt_table <- spread(count(mt_tw2[,c("screen_name","category","cluster")], 
                         screen_name, cluster), cluster, n, fill = 0)

## merge category data
mt_table <- mt_table %>%
  left_join(mt_tw2[,c("screen_name","category")] %>% unique(), by="screen_name")

## check whether each follower is retweeting the opposite ideology opinion leaders or not
mt_table$mt <- rep(NA, nrow(mt_table))
mt_table$mt <- ifelse(mt_table$category=="Liberals" & 
                        mt_table$`2`+mt_table$`3`+mt_table$`5`==0, "zero",
                      ifelse(mt_table$category=="Liberals", "one or more",
                             ifelse(mt_table$category %in% c("Conservatives","Netto uyoku") &
                                      mt_table$`1`==0, "zero",
                                    ifelse(mt_table$category %in% c("Conservatives","Netto uyoku"), "one or more", NA))))

## merge to dataframe
load(paste0(data.folder, "target_act.rdata"))
target_act <- target_act %>% left_join(mt_table[,c("screen_name","mt")], by="screen_name")

save(target_act, file=paste0(data.folder, "target_act.rdata"))



############ Step4: Compare and create figures ############

load(paste0(data.folder, "target_act.rdata"))

## add "zero" to users who do not follow any accounts
target_act$rt[is.na(target_act$rt)] <- "zero"
target_act$mt[is.na(target_act$mt)] <- "zero"
target_act$ct[is.na(target_act$ct)] <- "zero"

## check whether all of three actions are zero or not
target_act$act <- rep(NA, nrow(target_act))
target_act$act <- ifelse(target_act$rt=="zero" & target_act$mt=="zero" & target_act$ct=="zero",
                         "zero", "one or more")

## compare between categories
table_act <- table(target_act$act, target_act$category) %>% data.frame(stringsAsFactors = F)
table_act <- table_act[table_act$Var2 != "Media",]

## create a graph
ggplot(table_act) + geom_bar(aes(Var2, Freq, fill=Var1), stat="identity") +
  xlab("category") + 
  scale_x_discrete(labels=c("Liberal\nfollowers", "Conservative\nfollowers", "Netto-uyoku\nfollowers")) +
  scale_fill_brewer(palette = "Reds",name="responses to\nthe opposite") +
  theme_bw()
ggsave(paste0(figures.folder, "Figure 4-2 response-zero.pdf"), width = 6, height = 3)


## for each action
table_rt <- table(target_act$rt, target_act$category) %>% data.frame(stringsAsFactors = F)
table_rt <- table_rt[table_rt$Var2 != "Media",]

ggplot(table_rt) + geom_bar(aes(Var2, Freq, fill=Var1), stat="identity") +
  xlab("category") + 
  scale_x_discrete(labels=c("Liberal\nfollowers", "Conservative\nfollowers", "Netto-uyoku\nfollowers")) +
  scale_fill_brewer(palette = "Reds",name = "retweet opposite") +
  theme_bw()
ggsave(paste0(figures.folder, "App Figure 2-1 retweet-zero.pdf"), width = 6, height = 3)


table_qt <- table(target_act$ct, target_act$category) %>% data.frame(stringsAsFactors = F)
table_qt <- table_qt[table_qt$Var2 != "Media",]

ggplot(table_qt) + geom_bar(aes(Var2, Freq, fill=Var1), stat="identity") +
  xlab("category") + 
  scale_x_discrete(labels=c("Liberal\nfollowers", "Conservative\nfollowers", "Netto-uyoku\nfollowers")) +
  scale_fill_brewer(palette = "Reds",name = "quote retweet\nopposite") +
  theme_bw()
ggsave(paste0(figures.folder, "App Figure 2-2 quote retweet-zero.pdf"), width = 6, height = 3)

table_rp <- table(target_act$mt, target_act$category) %>% data.frame(stringsAsFactors = F)
table_rp <- table_rp[table_rp$Var2 != "Media",]

ggplot(table_rp) + geom_bar(aes(Var2, Freq, fill=Var1), stat="identity") +
  xlab("category") + 
  scale_x_discrete(labels=c("Liberal\nfollowers", "Conservative\nfollowers", "Netto-uyoku\nfollowers")) +
  scale_fill_brewer(palette = "Reds",name = "reply opposite") +
  theme_bw()
ggsave(paste0(figures.folder, "App Figure 2-3 reply-zero.pdf"), width = 6, height = 3)

