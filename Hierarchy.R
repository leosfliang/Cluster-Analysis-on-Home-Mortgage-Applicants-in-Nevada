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

# Hierarchy Clustering

gd_work <- daisy(df_work[,-c(6)], metric = 'gower')

# choose optimal linkages and corresponding k
linkages <- c("single", "complete", "average", "ward.D2")

hc <- list()
for (i in 1:length(linkages)){
  hc[[i]] <- hclust(gd_work, method = linkages[i])
}


sw_list1 <- numeric()
ch_list1 <- numeric()
for (i in 2:10){
  sw_list1 <- c(sw_list1, mean(silhouette(cutree(hc[[1]], i), gd_work)[,3]))
  ch_list1 <- c(ch_list1, fpc::calinhara(gd_work, cutree(hc[[1]], i)))
}

sw_list1 
ch_list1 

sw_list2 <- numeric()
ch_list2 <- numeric()
for (i in 2:10){
  sw_list2 <- c(sw_list2, mean(silhouette(cutree(hc[[2]], i), gd_work)[,3]))
  ch_list2 <- c(ch_list2, fpc::calinhara(gd_work, cutree(hc[[2]], i)))
}

sw_list2 
ch_list2 

sw_list3 <- numeric()
ch_list3 <- numeric()
for (i in 2:10){
  sw_list3 <- c(sw_list3, mean(silhouette(cutree(hc[[3]], i), gd_work)[,3]))
  ch_list3 <- c(ch_list3, fpc::calinhara(gd_work, cutree(hc[[3]], i)))
}

sw_list3 
ch_list3

sw_list4 <- numeric()
ch_list4 <- numeric()
for (i in 2:10){
  sw_list4 <- c(sw_list4, mean(silhouette(cutree(hc[[4]], i), gd_work)[,3]))
  ch_list4 <- c(ch_list4, fpc::calinhara(gd_work, cutree(hc[[4]], i)))
}

sw_list4
ch_list4

# The optimal linkage: complete, k = 2

table(cutree(hc[[2]], 2))
df_work$hc2<-as.factor(cutree(hc[[2]], 2))
df_work$hc3<-as.factor(cutree(hc[[2]], 3))
table(df_work$hc2, df_work$hc3)

# Characteristics of HC2 
table(group = df_work$action_taken_name, cluster = df_work$hc2)
table(group = df_work$action_taken_name, cluster = df_work$hc3)

df_work_num <- df_work %>% select_if(is.numeric)
df_work_cat <- df_work %>% select_if(is.factor)

plot_list_num <- list()
plot_list_cat <- list()
for (i in 1:(ncol(df_work_num))){
  plot_list_num[[i]] <- ggplot(df_work_num, aes_string(x = df_work$hc2, 
                                                  y = colnames(df_work_num)[i], 
                                                  color = df_work$hc2)) + geom_boxplot()
}
ggpubr::ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)

for (i in 1:(ncol(df_work_cat)-2)){
  plot_list_cat[[i]] <- ggplot(df_work_cat, aes_string(x = df_work$hc2, 
                                                  fill = colnames(df_work_cat)[i])) + geom_bar(stat='count')
}

ggpubr::ggarrange(plotlist = plot_list_cat, ncol = 4, nrow = 3)

# Characteristics of HC3

plot_list_num <- list()
plot_list_cat <- list()
for (i in 1:(ncol(df_work_num))){
  plot_list_num[[i]] <- ggplot(df_work_num, aes_string(x = df_work$hc3, 
                                                       y = colnames(df_work_num)[i], 
                                                       color = df_work$hc3)) + geom_boxplot()
}
ggpubr::ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)

for (i in 1:(ncol(df_work_cat)-2)){
  plot_list_cat[[i]] <- ggplot(df_work_cat, aes_string(x = df_work$hc3, 
                                                       fill = colnames(df_work_cat)[i])) + geom_bar(stat='count')
}

ggpubr::ggarrange(plotlist = plot_list_cat, ncol = 4, nrow = 3)