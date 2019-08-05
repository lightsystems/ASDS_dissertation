#==============================================================================
# 02-get-followers&timelines.R
# Purpose: specify opinion leaders and get their followers and timelines
# Author: Keisuke Idemitsu
#==============================================================================

library(DBI)
library(tweetscores)
library(streamR)

oauth.folder <- "./credentials"  ## designate the folder storing oauth files
data.folder <- "./data"  ## designate data store folder


############ Step1: Opinion leader specification ############

## get data from SQLite database
SQL.path <- "./tweets.sqlite"  ## designate SQL path
db <- dbConnect(RSQLite::SQLite(), SQL.path)  ## create a new SQLite file
tweets <- dbGetQuery(db, "SELECT * FROM tweets")
dbDisconnect(db)  ## disconnect to database

## specify frequently retweeted accounts (those who are retweeted 100 times or more)
freq_accounts <- table(tweets$retweet_screen_name, dnn = "screen_name") %>%
  sort(decreasing=T) %>% data.frame(stringsAsFactors = F) %>%
  subset(Freq>=100)
freq_accounts$screen_name <- as.character(freq_accounts$screen_name)  ## convert factors to character

## get accounts info
account_info <- getUsersBatch(screen_names=freq_accounts$screen_name, oauth=oauth.folder)

## choose accounts who have more than 10000 followers
freq_accounts <- account_info[account_info$followers_count > 10000,]

save(freq_accounts,file=paste0(data.folder,"freq_accounts.rdata"))



############ Step2: Get followers lists of opinion leaders ############

## create a folder to store followers lists
folder.name <- paste0(data.folder, "followers_lists/")

## removing those that we already did
accounts.done <- list.files(folder.name)
accounts.left <- freq_accounts$screen_name[freq_accounts$screen_name %in%
                                gsub(".rdata", "", accounts.done) == FALSE]
accounts.left <- accounts.left[!is.na(accounts.left)]

# loop over each account
while (length(accounts.left) > 0){
  # sample randomly one account to get followers
  new.user <- sample(accounts.left, 1)
  cat(new.user, " -- ", length(accounts.left), " accounts left!\n")   
  # download followers (with some exception handling...) 
  error <- tryCatch(
    followers <- getFollowers(screen_name=new.user, oauth=oauth.folder), error=function(e) e)
  if (inherits(error, 'error')) {
    cat("Error! On to the next one...")
    next
  }
  # save to file and remove from lists of "accounts.left"
  file.name <- paste0(folder.name, new.user, ".rdata")
  save(followers, file=file.name)
  accounts.left <- accounts.left[-which(accounts.left %in% new.user)]
}



############ Step3: Get timelines of opinion leaders ############

## create a folder to store timelines of leaders
folder.name <- paste0(data.folder, "timeline_leader/")

## specify remaining targets
targets <- freq_accounts$screen_name[
  ! freq_accounts$screen_name %in% 
    gsub(".json","",list.files(folder.name))]

for (i in 1:length(targets)){
  ## get timelines of opinion leaders
  filename <- paste0(folder.name, targets[i], ".json")
  try(getTimeline(filename=filename, n=3200, oauth.folder, screen_name=targets[i]))
  cat(i)
}

## store data into SQLite database
files <- list.files(folder.name, full.names = T)
df <- list()
for (i in 1:length(files)){
  tmp <- try(parseTweets(files[i]))  ## perse data
  if (nrow(tw)>0){df[[i]] <- tmp}
  cat(i)
}
tw <- do.call(rbind, df)

## create a new SQLite database
SQL.path <- paste0(data.folder, "tl_leaders.sqlite")
db <- dbConnect(RSQLite::SQLite(), SQL.path)
dbWriteTable(db, "tl_leaders", tw)
dbDisconnect(db)
