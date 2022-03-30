library(tidyverse)
library(cluster)
library(ggpubr)

# import cleaned data -----------------------------------------------------

df_sample <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)
df_sample$action_taken_name <- as.factor(df_sample$action_taken_name)

summary(df_sample)
# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_sample) != 'action_taken_name')]

df_num <- df_sample %>% select_if(is.numeric)


# Check outliers ----------------------------------------------------------

mdist <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
pval <- pchisq(mdist, df=ncol(df_num)-1, lower.tail=FALSE)
alpha <- 0.05
sum(pval < alpha) #5634 outliers

df_num_noout <- df_num_noout[which(pval >= alpha),]

# optimal k ----------------------------------------------------------
gd <- daisy(df_main, metric = 'gower')

#sillouette score
sw<-function(km, d)
{
  s<-cluster::silhouette(km$cluster, d)
  s<-mean(s[,3])
  return(s)
}

#get best k
set.seed(1234)
optkmean <- numeric()
optkmed <- numeric()
for (i in 1:5) {
  swscoreMean <- numeric()
  swscoreMed <- numeric()
  for (k in 2:5) {
    km <- kmeans(gd,k)
    kmed <- pam(gd,k,diss = TRUE)
    
    swscoreMean <- c(swscoreMean,sw(km,gd))
    swscoreMed <- c(swscoreMed,sw(kmed,gd))
  }
  optkmean <- c(optkmean, which.max(swscoreMean) +1)
  optkmed <- c(optkmed, which.max(swscoreMed) +1)
}

table(kmean = optkmean) # k = 2
table(kmed = optkmed) # k =2

km <- kmeans(gd,2)
kmed <- pam(gd,2,diss = TRUE)

df_km <- df_main
df_km$CLUSTER <- as.factor(km$cluster)

plot_listnum <- list()
plot_listcat <- list()
var_name <- names(df_km)
p1 = 1
p2 = 1
for (i in 1:(ncol(df_km)-1)){
  current_col <- colnames(df_km)[i]
  if (is.numeric(df_km[,i])){
    plot_listnum[[p1]] <- ggplot(df_km, aes_string('CLUSTER', current_col,group = 'CLUSTER')) +
      geom_boxplot()  +
      labs(title = current_col) +
      ylab('')
    
    p1 = p1 + 1
  }else{
    plot_listcat[[p2]]<- ggplot(df_km, aes_string( fill = current_col,x = 'CLUSTER')) +
      geom_bar(position = "dodge")+
      labs(title = current_col) +
      ylab('')
    
    p2 = p2 + 1
  }
}

ggarrange(plotlist=plot_listnum, widths = c(2,4))

ggarrange(plotlist=plot_listcat, widths = c(3,3))

# numerical data ----------------------------------------------------------
eu <- dist(scale(df_num))

set.seed(1234)
optkmean_eu <- numeric()
optkmed_eu <- numeric()
for (i in 1:5) {
  swscoreMean <- numeric()
  swscoreMed <- numeric()
  for (k in 2:5) {
    km <- kmeans(eu,k)
    kmed <- pam(eu,k,diss = TRUE)
    
    swscoreMean <- c(swscoreMean,sw(km,eu))
    swscoreMed <- c(swscoreMed,sw(kmed,eu))
  }
  optkmean_eu <- c(optkmean_eu, which.max(swscoreMean) +1)
  optkmed_eu <- c(optkmed_eu, which.max(swscoreMed) +1)
}

table(kmean = optkmean_eu) # k = 2
table(kmed = optkmed_eu) # k =2

km_eu <- kmeans(eu,2)
kmed_eu <- pam(eu,2,diss = TRUE)

 #### plot kmeu clusters
df_km_eu <- df_num
df_km_eu$CLUSTER <- as.factor(km_eu$cluster)

plot_listnum_km <- list()
for (i in 1:(ncol(df_km_eu)-1)){
  current_col <- colnames(df_km_eu)[i]
  plot_listnum_km[[i]] <- ggplot(df_km_eu, 
                                 aes_string('CLUSTER',
                                            current_col,
                                            group = 'CLUSTER',
                                            color = 'CLUSTER')) +
    geom_boxplot()  +
    labs(title = current_col) +
    ylab('')
}

ggarrange(plotlist=plot_listnum)


# compare with action taken -----------------------------------------------
km5 <- kmeans(gd,2)
kmed5 <- pam(gd,2,diss = TRUE)

table(df_sample$action_taken_name,km5$cluster)
table(df_sample$action_taken_name,kmed5$cluster)

(km_eutab <- table(km_eu$cluster,df_sample$action_taken_name))
(kmed_eutab <- table(kmed_eu$cluster,df_sample$action_taken_name))

sum(diag(km_eutab))/sum(km_eutab) 
sum(diag(kmed_eutab))/sum(kmed_eutab)
