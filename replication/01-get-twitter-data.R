#==============================================================================
# 01-get-twitter-data.R
# Purpose: download Tweets from Twitter API
# Details: Tweets are stored as SQLite
# Author: Keisuke Idemitsu
#==============================================================================

library(rtweet)
library(DBI)
library(readr)
library(dplyr)

raw.data.folder <- "./data/raw"  ## designate raw data folder


############ Step1: Tweet collection ############

### prepare information
keywords <- c("外国人","移民")
location <- '35.4122,139.4130,2000km'  ## define the realm of Japan as circle
n_collect <- 40  ## number of collection
## lines to keep from whole data
required_lines <- c("user_id","status_id","created_at","screen_name","text",
                    "is_retweet","retweet_user_id","retweet_screen_name",
                    "location","account_lang")

### collect data automatically
interval <- 3600
x <- 0
repeat {
  startTime <- Sys.time()  ## keep starting time
  rate.limit <- 180  ## keep Twitter API limit
  
  df <- list()  ## prepare data frame
  for (i in 1:length(keywords)){
    tmp <- try(search_tweets(keywords[i], n=n_collect, geocode=location))
    if ((typeof(tmp) != "character")&(length(tmp)!=0)){
      df[[i]] <- data.frame(tmp[,required_lines])
    }
    rate.limit <- rate.limit - 1  ## reduce limit
    if (rate.limit == 0){
      cat("hit the limit... wait for 15 mins")
      Sys.sleep(900)
      rate.limit <- 180
    }
  }
  df <- do.call(rbind, df)  ## combine data
  file.name <- paste0("JP", sprintf("%f.csv", Sys.time()))  ## keep date and time to filename
  write.csv(df, file.name)  ## write file to csv
  
  ## sleep until an hour after the start time
  sleepTime <- startTime + interval - Sys.time()
  if (sleepTime > 0){
    if (attr(sleepTime, "units") == "mins"){sleepTime <- sleepTime*60}
    Sys.sleep(sleepTime)
  }
}



############ Step2: Store data to SQLite ############

SQL.path <- "./tweets.sqlite"  ## designate SQL path

db <- dbConnect(RSQLite::SQLite(), SQL.path)  ## create a new SQLite file
lf <- list.files(path = folder, pattern = x, full.names = T)  ## list of files
df <- list()  ## prepare for dataframe

## read each file
for (i in 1:length(lf)){
  tmp <- read_csv(lf[i], col_names = T)
  ### column name
  if (colnames(tmp[2]) == "X"){tmp <- tmp[,colnames(tmp) != c("X1","X")]}
  else{tmp <- tmp[,colnames(tmp) != c("X1")]}
  df[[i]] <- tmp
}
df <- do.call(rbind, df)  ## conbine each data
df <- df %>% distinct(status_id, .keep_all = T) ## eliminate duplicates

## write dataframe to SQLite database
dbWriteTable(db, "tweets", df, append=T)
dbDisconnect(db)  ## disconnect to database
