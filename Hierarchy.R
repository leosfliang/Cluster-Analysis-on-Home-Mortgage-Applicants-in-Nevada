rm(list=ls())

library(tidyverse)
library(cluster)
library(ggplot2)
library(dbscan)
library(ggpubr)
library(rpart)

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

library(factoextra)
fviz_cluster(db, gd, ellipse = TRUE, geom = "point")

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
plot <- ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)
annotate_figure(plot, top = text_grob("DBSCAN - numerical", 
                                      color = "red", face = "bold", size = 14))

for (i in 1:(ncol(df_cat)-2)){
  plot_list_cat[[i]] <- ggplot(df_cat, aes_string(x = df_cat$db, fill = colnames(df_cat)[i])) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = colnames(df_cat)[i]) +
  ylab('') +
  theme(plot.title = element_text(size=8),legend.text=element_text(size=7)) +
  guides(fill=guide_legend(title=""))
}

plot <- ggarrange(plotlist = plot_list_cat, ncol = 3, nrow = 3)
annotate_figure(plot, top = text_grob("DBSCAN - categorical", 
                                      color = "red", face = "bold", size = 14))
# As we can observe from the boxplots above, outliers (DB=0) have relatively lower loan amount and higher 
# income. 

table(group = df_sample$action_taken_name, dbs = df_main$db) 
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

#
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

# The optimal linkage: complete, k = 2 or ward.d2, k = 4

table(cutree(hc[[2]], 2))
df_work$hc2<-as.factor(cutree(hc[[2]], 2))

# Characteristics of HC2 
prop.table(table(group = df_work$action_taken_name, cluster = df_work$hc2),2)

df_work_num <- df_work %>% select_if(is.numeric)
df_work_cat <- df_work %>% select_if(is.factor)

plot_list_num <- list()
plot_list_cat <- list()
for (i in 1:(ncol(df_work_num))){
  plot_list_num[[i]] <- ggplot(df_work_num, aes_string(x = df_work$hc2, 
                                                  y = colnames(df_work_num)[i], 
                                                  color = df_work$hc2)) + geom_boxplot()
}
plot <- ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2) 
annotate_figure(plot, top = text_grob("Hierarchical(COMPLETE, k = 2) - numerical", 
                                      color = "red", face = "bold", size = 14))



for (i in 1:(ncol(df_work_cat)-1)){
  plot_list_cat[[i]] <- ggplot(df_work_cat, aes_string(x = df_work$hc2, fill = colnames(df_work_cat)[i])) + 
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = colnames(df_work_cat)[i]) +
    ylab('') +
    theme(plot.title = element_text(size=8),legend.text=element_text(size=7)) +
    guides(fill=guide_legend(title=""))
}

plot <- ggarrange(plotlist = plot_list_cat, ncol = 3, nrow = 4)
annotate_figure(plot, top = text_grob("Hierarchical(COMPLETE, k = 2) - categorical", 
                                      color = "red", face = "bold", size = 14))

tree12 <- rpart(df_work$hc2  ~ ., data = df_work[, -c(6)])
tree12$variable.importance
library(rpart.plot)
rpart.plot(tree12, yesno = TRUE)

prop.table(table(df_work$action_taken_name, df_work$loan_type_name),2)

# Increasing one more cluster 
df_work$hc3<-as.factor(cutree(hc[[2]], 3))
table(df_work$hc2, df_work$hc3)

df_work_2 <- df_work[df_work$hc2 == 1,]
df_work_2$hc3 <- droplevels(df_work_2$hc3)
prop.table(table(group = df_work_2$action_taken_name, cluster = df_work_2$hc3),2)
df_work_2_num <- df_work_2 %>% select_if(is.numeric)
df_work_2_cat <- df_work_2 %>% select_if(is.factor)

plot_list_num <- list()
plot_list_cat <- list()
for (i in 1:(ncol(df_work_2_num))){
  plot_list_num[[i]] <- ggplot(df_work_2_num, aes_string(x = df_work_2$hc3, 
                                                       y = colnames(df_work_2_num)[i], 
                                                       color = df_work_2$hc3)) + geom_boxplot()
}
plot <- ggarrange(plotlist = plot_list_num, ncol = 4, nrow = 2)
annotate_figure(plot, top = text_grob("Hierarchical(COMPLETE, k = 3) - numerical", 
                                      color = "red", face = "bold", size = 14))

for (i in 1:(ncol(df_work_2_cat)-2)){
  plot_list_cat[[i]] <- ggplot(df_work_2_cat, aes_string(x = df_work_2$hc3, fill = colnames(df_work_2_cat)[i])) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = colnames(df_work_cat)[i]) +
    ylab('') +
    theme(plot.title = element_text(size=8),legend.text=element_text(size=7)) +
    guides(fill=guide_legend(title=""))
}

plot <- ggarrange(plotlist = plot_list_cat, ncol = 3, nrow = 4)
annotate_figure(plot, top = text_grob("Hierarchical(COMPLETE, k = 3) - categorical", 
                                      color = "red", face = "bold", size = 14))

tree23 <- rpart(df_work$hc3[df_work$hc2 == 1]  ~ ., data = df_work[df_work$hc2 == 1, -c(6)])
tree23$variable.importance
rpart.plot(tree23, yesno = TRUE)

prop.table(table(group = df_work$action_taken_name, cluster = df_work$hc3),2)


df_work$hc4 <- as.factor(cutree(hc[[2]], 4))
df_work$hc5 <- as.factor(cutree(hc[[2]], 5))
df_work$hc6 <- as.factor(cutree(hc[[2]], 6))

table(df_work$hc3, df_work$hc4)
tree34 <- rpart(df_work$hc4[df_work$hc3 == 3]  ~ ., data = df_work[df_work$hc3 == 3, c(1:5,7:17)])
tree34$variable.importance

table(df_work$hc4, df_work$hc5)
tree45 <- rpart(df_work$hc5[df_work$hc4 == 3]  ~ ., data = df_work[df_work$hc4 == 3, c(1:5,7:17)])
tree45$variable.importance

table(df_work$hc5, df_work$hc6)
tree56 <- rpart(df_work$hc6[df_work$hc5 == 1]  ~ ., data = df_work[df_work$hc5 == 1, c(1:5,7:17)])
tree56$variable.importance
