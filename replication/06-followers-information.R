#==============================================================================
# 06-clustering-followers.R
# Purpose: Clustering followers, sampling them randomly and get their timelines
# and create figure 3 and appendix figure 1
# Author: Keisuke Idemitsu
#==============================================================================

library(tweetscores)
library(dplyr)
library(ggplot2)
library(pwr)
library(streamR)
library(DBI)

oauth.folder <- "./credentials"  ## designate the folder storing oauth files
figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data"  ## designate data store folder


############ Step1: Get users information ############

load(paste0(data.folder,"all_follow.rdata"))  ## data processed by python

## get information of accounts
info <- getUsersBatch(ids=all_follow_narm$id_str,oauth=oauth.folder)
all_follow <- all_follow %>% left_join(info, by="id_str")

save(all_follow, file=paste0(data.folder, "all_follow.rdata"))



############ Step2: Clustering followers ############

load(paste0(data.folder, "all_follow.rdata"))

## classify liberal, conservative or right-wing
all_follow$category <- rep(NA,nrow(all_follow))
all_follow$category[all_follow$liberal > all_follow$conservative] <- "Liberals"
all_follow$category[all_follow$conservative > all_follow$liberal] <- "Conservatives"
all_follow$category[all_follow$category == "Conservatives" &
                      (all_follow$right_wing*25 + all_follow$matome*5) >= 3] <- "Netto uyoku"

all_follow$category[(all_follow$liberal*43 + all_follow$conservative*42) <= 2] <- "Media"

## order the factor levels
all_follow$category <- factor(all_follow$category,
                              levels=c("Liberals","Media","Conservatives","Netto uyoku"))

save(all_follow, file=paste0(data.folder, "all_follow.rdata"))



############ Step3: Create plots ############

## pie chart
cluster_fol <- table(all_follow$category,dnn="Cluster") %>%
  data.frame(stringsAsFactors = F)  ## create frequency table
cluster_fol$Percentage <- cluster_fol$Freq / sum(cluster_fol$Freq)  ## add percentage

pdf(paste0(figures.folder, "Figure 3 FLpercentage.pdf"), width = 5, height = 5)
pie(cluster_fol$Freq, 
    labels = c("Liberals \n 45% (191,934)","Media \n 24% (102,785)", "Conservatives \n 11% (47,082)", 
               "Netto uyoku \n 19% (82,350)"),
    col = c("#00CED1", "#9ACD32", "Orange", "#FF1493"))
dev.off()

## scatter plot
ggplot(all_follow[sample(nrow(all_follow), 10000),]) + 
  geom_jitter(aes(conservative, liberal, color=category), alpha=0.7) +
  labs(x="The rate of following Conservative/Netto uyoku leaders and Matome sites",
       y="The rate of following Liberal leaders") +
  scale_color_manual(values = c("#00CED1", "#9ACD32", "Orange", "#FF1493")) +
  theme_bw() +
  guides(color=guide_legend("Cluster"))
ggsave(paste0(figures.folder, "App Figure 1 FLscatter.pdf"), width = 7, height = 4)



############ Step4: Choose random samples ############

load(paste0(close.folder, "all_follow.rdata"))

## choose rows of active accounts (status count is more than 100)
actives <- which(all_follow$statuses_count > 100)
table(all_follow$category[actives])  ## number of accounts in each category

## decide sample size
## where the number of accounts in the smallest category (Conservatives: 27339)
pwr.2p2n.test(h=0.2, 27339, n2=NULL, sig.level=0.05, power=0.9,
              alternative="two.sided")

## choose 400 as sample size for each category
set.seed(123)
sample_lib <- sample(all_follow$id_str[
  all_follow$statuses_count > 100 &
    all_follow$category=="Liberals" &
    is.na(all_follow$screen_name)==F
  ], 400)

set.seed(123)
sample_med <- sample(all_follow$id_str[
  all_follow$statuses_count > 100 &
    all_follow$category=="Media" &
    is.na(all_follow$screen_name)==F
  ], 400)

set.seed(123)
sample_con <- sample(all_follow$id_str[
  all_follow$statuses_count > 100 &
    all_follow$category=="Conservatives" &
    is.na(all_follow$screen_name)==F
  ], 400)

set.seed(123)
sample_net <- sample(all_follow$id_str[
  all_follow$statuses_count > 100 &
    all_follow$category=="Netto uyoku" &
    is.na(all_follow$screen_name)==F
  ], 400)

## merge target accounts
targets <- c(sample_lib, sample_med, sample_con, sample_net)

save(targets, file=paste0(data.folder, "targets.rdata"))



############ Step5: Get timelines of random samples ############

load(paste0(data.folder, "targets.rdata"))

## designate a folder to store timelines
folder.name <- paste0(data.folder, "timeline_follower/")

## get timelines of opinion leaders
for (i in 1:length(targets)){
  filename=paste0(folder.name, targets[i], ".json")
  try(getTimeline(filename=filename, n=3200, oauth.folder, id=targets[i]))
  cat(i)
}

## create files list
files <- list.files(folder.name, full.names = F) %>% gsub(".json", "", .)

## merge into dataframe
df <- list()
for (i in 1:length(files)){
  filename <- paste0(close.folder, "timeline_follower/",files[i],".json")
  tw <- try(parseTweets(filename))
  if (nrow(tw)>0){df[[i]] <- tw}
  cat(i)
}
tw <- do.call(rbind, df)

## store dataframe into SQLite database
SQL.path <- paste0(data.folder, "tl_followers.sqlite")  ## designate new SQLite path
db <- dbConnect(RSQLite::SQLite(), SQL.path)
dbWriteTable(db, "tl_followers", tw, append=T)
dbDisconnect(db)

