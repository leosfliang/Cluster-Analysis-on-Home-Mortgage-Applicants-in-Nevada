library(tidyverse)
library(cluster)
library(dbscan)
library(ggpubr)

# import cleaned data -----------------------------------------------------

df_sample <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)
df_sample$action_taken_name <- as.factor(df_sample$action_taken_name)

str(df_sample)
summary(df_sample)
# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_sample) != 'action_taken_name')]

df_num <- df_sample %>% select_if(is.numeric)

# Check outliers ----------------------------------------------------------
# Mahalanobis
mdist <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
pval <- pchisq(mdist, df=ncol(df_num)-1, lower.tail=FALSE)
alpha <- 0.01
sum(pval < alpha) 

df_num_noout <- df_num[which(pval >= alpha),]

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

# Describe characteristics of outliers




# compare with action taken -----------------------------------------------
km5 <- kmeans(gd,2)
kmed5 <- pam(gd,2,diss = TRUE)

table(df_sample$action_taken_name,km5$cluster)
table(df_sample$action_taken_name,kmed5$cluster)

(km_eutab <- table(km_eu$cluster,df_sample$action_taken_name))
(kmed_eutab <- table(kmed_eu$cluster,df_sample$action_taken_name))

sum(diag(km_eutab))/sum(km_eutab) 
sum(diag(kmed_eutab))/sum(kmed_eutab)
