#==============================================================================
# 08-Hypothesis2.R
# Purpose: Check Hypothesis2 by comparing the amount of actions and quoted URLs
# and create tables 1-1~1-3, table 2 and figure 5
# Author: Keisuke Idemitsu
#==============================================================================

library(tidyr)
library(dplyr)
library(circlize)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data"  ## designate data store folder


############ Step1-1: Retweet interaction ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "rt_tw2.rdata"))
rt_tw2$cluster <- as.factor(rt_tw2$cluster)  ## convert to factor

## write a table 
write.csv(table(rt_tw2$category, rt_tw2$cluster), file=paste0(figures.folder, "Table 1-1 rt_cluster.csv"))

## spread data to create a plot
rt_cluster2 <- rt_tw2[,c("category","cluster")] %>% group_by(category, cluster) %>%
  summarise(count=n())
## convert to factor and set names of factor levels
rt_cluster2$cluster <- as.factor(rt_cluster2$cluster)
levels(rt_cluster2$cluster) <- c("Liberal*", "Conservatives*", "Netto uyoku*", "Media*", "Matome*")

## create a palette to visualise
grid.col2 <- c("Liberals"="#00CED1", "Media"="#9ACD32", "Conservatives"="Orange", "Netto uyoku"="#FF1493",
               "Liberal*"="#00CED1", "Conservatives*"="Orange", "Netto uyoku*"="#FF1493",
               "Media*"="#9ACD32", "Matome*"="#32CD32")

pdf(paste0(figures.folder, "Figure 5-1 rt-interactions.pdf"), width = 5, height = 5)
chordDiagram(rt_cluster2, transparency = 0.5, grid.col = grid.col2, big.gap = 45)
dev.off()



############ Step1-2: Quote retweets interaction ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "ct_tw2.rdata"))
ct_tw2$cluster <- as.factor(ct_tw2$cluster)  ## convert to factor

## write a table 
write.csv(table(ct_tw2$category, ct_tw2$cluster), file=paste0(figures.folder, "Table 1-2 qt_cluster.csv"))

## spread data to create a plot
ct_cluster2 <- ct_tw2[,c("category","cluster")] %>% group_by(category, cluster) %>%
  summarise(count=n())
## convert to factor and set names of factor levels
ct_cluster2$cluster <- as.factor(ct_cluster2$cluster)
levels(ct_cluster2$cluster) <- c("Liberal*", "Conservatives*", "Netto uyoku*", "Media*", "Matome*")

pdf(paste0(figures.folder, "Figure 5-2 qt-interactions.pdf"), width = 5, height = 5)
chordDiagram(ct_cluster2, transparency = 0.5, grid.col = grid.col2, big.gap = 45)
dev.off()



############ Step1-3: Replies interaction ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "mt_tw2.rdata"))
mt_tw2$cluster <- as.factor(mt_tw2$cluster)  ## convert to factor

## write a table
write.csv(table(mt_tw2$category, mt_tw2$cluster), file=paste0(figures.folder, "Table 1-3 rp_cluster.csv"))

## spread data to create a plot
mt_cluster2 <- mt_tw2[,c("category","cluster")] %>% group_by(category, cluster) %>%
  summarise(count=n())
## convert to factor and set names of factor levels
mt_cluster2$cluster <- as.factor(mt_cluster2$cluster)
levels(mt_cluster2$cluster) <- c("Liberal*", "Conservatives*", "Netto uyoku*", "Media*", "Matome*")

pdf(paste0(figures.folder, "Figure 5-3 rp-interactions.pdf"), width = 5, height = 5)
chordDiagram(mt_cluster2, transparency = 0.5, grid.col = grid.col2, big.gap = 45)
dev.off()



############ Step2: Quoted URLs ############

## extract row numbers of quote URLs
rows <- setdiff(grep("https://", tw2$expanded_url), grep("https://twitter.com/", tw2$expanded_url))

## add domain names of quoted URLs
tw2$cite_others <- rep(NA, nrow(tw2))
tw2$cite_others[rows] <- sapply(strsplit(tw2$expanded_url[rows], "/"), "[", 3)

## create a new dataframe which contain only tweets quoting URLs
ct_others2 <- tw2[is.na(tw2$cite_others)==F, c("category","cite_others")]

## create a frequancy table for each category and merge them
ct_others2_c <- list()
n <- length(unique(ct_others2$category))  ## number of categories
for (i in 1:n){
  ## create a frequency table of domains
  tmp <- table(ct_others2$cite_others[ct_others2$category==unique(ct_others2$category)[i]])
  tmp <- tmp[order(tmp, decreasing = T)]  ## order by frequency
  ct_others2_c[[i]] <- tmp[1:20] %>% data.frame(stringsAsFactors = F)
}
ct_others2_c <- do.call(cbind, ct_others2_c) %>% data.frame() 

write.csv(ct_others2_c, paste0(figures.folder, "Table 2 URLtable.csv"))
