#==============================================================================
# 09-Hypothesis3.R
# Purpose: Analyse texts of quoted retweets and replies from Netto uyoku to liberals
# and create table 4 and figure 6
# Author: Keisuke Idemitsu
#==============================================================================

library(quanteda)
library(dplyr)
library(reshape2)
library(ggplot2)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data"  ## designate data store folder


############ Step1: Data preparation ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "ct_tw2.rdata"))  ## quote retweets
load(paste0(data.folder, "mt_tw2.rdata"))  ## replies

## choose target data, which are 1) from "Netto uyoku" to "Netto uyoku",
## 2) from "Netto uyoku" to "Liberals" and 3) from "Liberals" to "Liberals"
targettxt2 <- rbind(mt_tw2[mt_tw2$category == "Netto uyoku" & mt_tw2$cluster %in% c(1,3) |
                             mt_tw2$category == "Liberals" & mt_tw2$cluster == 1 ,c(1,3,4)],
                    ct_tw2[ct_tw2$category == "Netto uyoku" & ct_tw2$cluster %in% c(1,3) |
                             ct_tw2$category == "Liberals" & ct_tw2$cluster == 1 ,c(1,3,4)])

## tokenise texts
tokens_target2 <- corpus(targettxt2) %>%
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE) %>%
  tokens_remove(pattern=c("t.co", "https", "rt", "amp", "http", "t.c", "can")) %>%
  tokens_select(min_nchar = 2)

## create a dfm (document frequency matrix) grouped by text category
dfm_target2 <- dfm(tokens_target2) %>%
  dfm_select('^[０-９ァ-ヶー一-龠]+$', valuetype = 'regex') %>%
  dfm_group(groups = paste(docvars(., "category"),
                           "to", docvars(., "cluster")
  ))



############ Step2: Similarity analysis ############

## calculate similarity
tstat_simil_target2 <- textstat_simil(dfm_tfidf(dfm_target2), margin = "documents", method = "cosine")
tstat_simil_target2 <- as.matrix(tstat_simil_target2)  ## convert to matrix
tstat_simil_target2[upper.tri(tstat_simil_target2)] <- NA  ## omit upper triangle
tstat_simil_target2 <- melt(tstat_simil_target2)  ## melt data to create a plot

## plot
ggplot(tstat_simil_target2, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white") +
  geom_text(aes(fill = tstat_simil_target2$value, label = round(tstat_simil_target2$value, 2))) +
  scale_fill_gradient2(low = "white", high = "#009E73") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  scale_x_discrete(name="", labels=c("(1) from Netto uyoku followers\nto Netto uyoku opinion leaders",
                                     "(2) from Netto uyoku followers\nto liberal opinion leaders",
                                     "(3) from Liberal followers\nto liberal opinion leaders")) +
  scale_y_discrete(name="",labels=c("(1)","(2)","(3)")) +
  coord_fixed()
ggsave(paste0(figures.folder, "Figure 6 similarity.pdf"), width = 5, height = 5)



############ Step3: Term frequency analysis ############

## calculate term frequency by group
tstat_freq_target2 <- textstat_frequency(dfm_target2, groups = docnames(dfm_target))

## create a matrix of term frequency
freq_target2 <- list()
n <- length(unique(tstat_freq_target2$group))
for (i in 1:n){
  freq_target2[[i]] <- tstat_freq_target2$feature[tstat_freq_target2$group == unique(tstat_freq_target2$group)[i]][1:20]
}
freq_target2 <- do.call(cbind, freq_target2) %>% data.frame()
colnames(freq_target2) <- unique(tstat_freq_target2$group)

write.csv(freq_target2, paste0(figures.folder, "Table 4 term-frequency.csv"))

