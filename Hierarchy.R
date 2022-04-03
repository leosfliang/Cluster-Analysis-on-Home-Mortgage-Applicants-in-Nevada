rm(list=ls())

library(tidyverse)
library(cluster)
library(ggplot2)
library(dbscan)

# import cleaned data -----------------------------------------------------

df_sample <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)
df_sample$action_taken_name <- as.factor(df_sample$action_taken_name)

str(df_sample)
summary(df_sample)

# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_sample) != 'action_taken_name')]
df_num <- df_main %>% select_if(is.numeric)
df_cat <- df_main %>% select_if(is.factor)

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
df_main$db <- as.factor(db$cluster)
df_num$md <- df_main$md
df_num$db <- df_main$db
df_cat$md <- df_main$md
df_cat$db <- df_main$db
str(df_main)
str(df_num)
str(df_cat)

# Describe characteristics of DBSCAN outliers

plot_list_num <- list()
plot_list_cat <- list()
for (i in 1:(ncol(df_num)-2)){
    plot_list_num[[i]] <- ggplot(df_num, aes_string(x = df_num$db, 
                                                 y = colnames(df_num)[i], 
                                                 color = df_num$db)) + geom_boxplot()
}
ggpubr::ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)

for (i in 1:(ncol(df_cat)-2)){
  plot_list_cat[[i]] <- ggplot(df_cat, aes_string(x = df_cat$db, 
                                                 fill = colnames(df_cat)[i])) + geom_bar(stat='count')
  table(df_cat[,i], df_cat$db)
}

ggpubr::ggarrange(plotlist = plot_list_cat, ncol = 4, nrow = 3)

# As we can observe from the boxplots above, outliers (DB=0) have relatively lower loan amount and higher 
# income. 

table(df_sample$action_taken_name, df_main$db) 
# Meanwhile the frequency table suggests that outliers are more likely to be rejected
# applications (42/75) rather than (636/1925), which contradicts to the common sense 
# that Higher income and lower loan, more likely the application is approved. 
# Thus, removing the DBSCAN outliers can reduce the impact of the corner cases. 

# Remove DBSCAN outliers.
df_work <- df_sample[which(df_main$db == 1),]
group <- df_work$action_taken_name

df_work <- df_work[,-c(6,16,17)]

# Hierarchy Clustering







