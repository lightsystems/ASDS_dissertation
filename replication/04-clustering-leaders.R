#==============================================================================
# 04-clustering-leaders.R
# Purpose: Clustering opinion leaders into five categories
# and create figures 2-1 and 2-2
# Author: Keisuke Idemitsu
#==============================================================================

library(DBI)
library(dplyr)
library(ggplot2)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data"  ## designate data store folder


############ Step1: Classify based on ideology estimation ############

load(paste0(data.folder,"freq_accounts.rdata"))


freq_accounts$cluster <- rep(NA, nrow(freq_accounts))  ## prepare variable
freq_accounts$cluster[freq_accounts$immDim1 < 0] <- 1  ## liberals
freq_accounts$cluster[freq_accounts$immDim1 > 0] <- 2  ## conservatives

## pick up tweets
SQL.path <- paste0(close.folder, "tl_leaders.sqlite")
db <- dbConnect(RSQLite::SQLite(), SQL.path)
tw <- dbGetQuery(db, "SELECT text, screen_name FROM tl_leaders")
dbDisconnect(db)

## select keywords which are frequently used by right-wing
keywords <- grep("売国|左翼|サヨク|パヨク|反日|中共", tw$text)
keywords_table <- table(tw$screen_name[keywords], dnn="screen_name") %>% data.frame()
## select accounts who use keywords more than the median
rw_accounts <- as.character(keywords_table$screen_name[keywords_table$Freq > median(keywords_table$Freq)])

freq_accounts$cluster[freq_accounts$immDim1 > 0 &
                        tolower(freq_accounts$screen_name) %in% tolower(rw_accounts)] <- 3


## media accounts
freq_accounts$cluster[freq_accounts$immDim1 < -0.8 & freq_accounts$immDim2 < -0.9] <- 4

## matome sites
matome <- c("moeruasia","jijinewspress","hoshusokuhou","anonymous201504","seijichishin")
freq_accounts$cluster[freq_accounts$screen_name %in% matome] <- 5

save(freq_accounts, file=paste0(data.folder,"freq_accounts.rdata"))
write.csv(freq_accounts, paste0(figures.folder, "App Table 1 freq_accounts.csv"))



############ Step2: Create Figures 2-1 and 2-2 ############

## two dimension scatter plot
ggplot(freq_accounts, aes(immDim1, immDim2)) + 
  geom_point(aes(color=as.factor(cluster), size=followers_count)) +
  labs(x="Dimension 1",y="Dimension 2") +
  scale_color_manual(labels = c("Liberals", "Conservatives", "Netto-uyoku", "Media", "Matome"),
                     values = c("#00CED1", "Orange", "#FF1493", "#9ACD32", "#32CD32")) +
  theme_bw() +
  guides(color=guide_legend("Cluster"))
ggsave(paste0(figures.folder, "Figure 2-1 OLestimation.pdf"), width = 7, height = 4)

## pie chart
cluster_table <- table(freq_accounts$cluster, dnn = "Cluster") %>%
  data.frame(stringsAsFactors = F)  ## create frequency table
cluster_table$Percentage <- cluster_table$Freq / sum(cluster_table$Freq)  ## add percentage

pdf(paste0(figures.folder, "Figure 2-2 OLpercentage.pdf"), width = 7, height = 4)
pie(cluster_table$Freq, clockwise = TRUE,
    labels = c("Liberals 46% (43)", "Conservatives 13% (12)",
               "Netto-uyoku 27% (25)", "Media 9% (8)", "Matome 5% (5)"),
    col = c("#00CED1", "Orange", "#FF1493", "#9ACD32", "#32CD32"))
dev.off()

