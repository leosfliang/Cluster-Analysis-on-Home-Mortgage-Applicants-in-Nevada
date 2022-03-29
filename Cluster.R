library(tidyverse)
library(cluster)
library(ggpubr)

# import cleaned data -----------------------------------------------------

df_clean <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)


# take a sample of the data -----------------------------------------------

sample_size = 1000

set.seed(1234)
df_sample <- df_clean[sample(1:nrow(df_clean),size = sample_size),]

summary(df_sample)
# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_clean) != 'action_taken_name')]


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
# compare with action taken -----------------------------------------------
km5 <- kmeans(gd,2)
kmed5 <- pam(gd,2,diss = TRUE)

table(df_sample$action_taken_name,km5$cluster)
table(df_sample$action_taken_name,kmed5$cluster)
