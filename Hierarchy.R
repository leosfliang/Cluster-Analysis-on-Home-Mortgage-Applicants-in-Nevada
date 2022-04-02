rm(list=ls())

library(tidyverse)
library(cluster)
library(ggplot2)

# import cleaned data -----------------------------------------------------

df_sample <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)
df_sample$action_taken_name <- as.factor(df_sample$action_taken_name)

str(df_sample)
summary(df_sample)

# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_sample) != 'action_taken_name')]
df_num <- df_sample %>% select_if(is.numeric)
df_cat <- df_sample %>% select_if(is.factor)

# Check outliers ----------------------------------------------------------
# Mahalanobis
mdist <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
pval <- pchisq(mdist, df=ncol(df_num)-1, lower.tail=FALSE)
alpha <- 0.01
sum(pval < alpha) 

df_num_noout <- df_num[which(pval >= alpha),]

# Use gower distance to derive the distance matrix
gd <- daisy(df_main, metric = 'gower')
min(gd)
median(gd)
max(gd)

# DBSCAN outliers ----------------------------------------------------------
db <- dbscan(gd, eps=0.11, minPts=50)
print(db)

table(db = as.factor(db$cluster), md = as.factor(ifelse(pval < alpha, 0, 1)))
df_main$md <- as.factor(ifelse(pval < alpha, 0, 1))
df_main$db <- db$cluster
df_num$md <- df_main$md
df_num$db <- df_main$db
df_cat$md <- df_main$md
df_cat$db <- df_main$db

# Describe characteristics of outliers

plot_list_num <- list()
plot_list_cat <- list()
for (i in 1:(ncol(df_num)-2)){
    plot_list_num[[i]] <- ggplot(df_num, aes_string(x = "md", 
                                                 y = colnames(df_num)[i], 
                                                 color = "md")) + geom_boxplot()
}
ggpubr::ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)

ggpubr::ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)















