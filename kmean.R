library(tidyverse)
library(cluster)
library(ggpubr)
library(factoextra)

source('function.R')
# import cleaned data -----------------------------------------------------

df_sample <- read.csv('hmda_2017_nevada_cleaned.csv')

df_sample[which(
  df_sample$property_type_name == 'One-to-four family dwelling (other than manufactured housing)'
), ]$property_type_name <- 'One-to-four family dwelling'

df_sample <- df_sample %>%
  mutate(across(where(is.character), as.factor))

df_sample$action_taken_name <- as.factor(df_sample$action_taken_name)
summary(df_sample)
# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_sample) != 'action_taken_name')]

df_num <- df_sample %>% select_if(is.numeric)


# Check outliers ----------------------------------------------------------

mdist <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
pval <- pchisq(mdist, df=ncol(df_num)-1, lower.tail=FALSE)
alpha <- 0.05
sum(pval < alpha) 

df_action_noout <- df_sample[which(pval >= alpha),'action_taken_name']
df_num_noout <- df_num[which(pval >= alpha),]
df_main_noout <- df_main[which(pval >= alpha),]

# optimal k ----------------------------------------------------------
gd_kmean <- daisy(df_main, metric = 'gower')

getOptK(dist = gd_kmean, type = 'kmean',n = 15)

#swscore -> 2
#chscore -> 3
set.seed(1234)
km <- kmeans(gd_kmean,2,nstart = 25)


df_km <- df_main
df_km$CLUSTER <- as.factor(km$cluster)
table(kmean_gd = km$cluster)

#### plot kmeans gd clusters ####
plot_listnum <- list()

plot_listcat <- list()

var_name <- names(df_km)
p1 = 1
p2 = 1
for (i in 1:(ncol(df_km)-1)){
  current_col <- colnames(df_km)[i]
  if (is.numeric(df_km[,i])){
    plot_listnum[[p1]] <- ggplot(df_km, aes_string('CLUSTER', 
                                                   current_col,
                                                   group = 'CLUSTER',
                                                   color = 'CLUSTER')) +
      geom_boxplot()  +
      labs(title = current_col) +
      ylab('') + 
      theme(plot.title = element_text(size=10),legend.position="none")
    
    p1 = p1 + 1
  }else{
    plot_listcat[[p2]]<- ggplot(df_km, aes_string( fill = current_col,x = 'CLUSTER')) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = current_col) +
      ylab('') +
      theme(plot.title = element_text(size=8),legend.text=element_text(size=7))+ 
      guides(fill=guide_legend(title=""))
    
    p2 = p2 + 1
  }
}

annotate_figure(ggarrange(plotlist=plot_listnum), 
                text_grob("Kmeans(gd) - Numerical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

annotate_figure(ggarrange(plotlist=plot_listcat), 
                text_grob("Kmeans(gd) - Categorical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

# numerical data ----------------------------------------------------------
eu <- dist(scale(df_num_noout))

getOptK(dist = eu , type = 'kmean')
#swscore -> 2
#chscore -> 2
set.seed(1234)
km_eu <- kmeans(eu,2,nstart = 25)

df_km_eu <- df_main_noout
df_km_eu$CLUSTER <- as.factor(km_eu$cluster)
table(kmeans_eu = km_eu$cluster)

 #### plot kmeu clusters ###
plot_listnum_eu <- list()
plot_listcat_eu <- list()

p1_eu = 1
p2_eu = 1
for (i in 1:(ncol(df_km_eu)-1)){
  current_col <- colnames(df_km_eu)[i]
  if (is.numeric(df_km_eu[,i])){
    plot_listnum_eu[[p1_eu]] <- ggplot(df_km_eu, aes_string('CLUSTER', 
                                                            current_col,
                                                            group = 'CLUSTER',
                                                            color = 'CLUSTER')) +
      geom_boxplot()  +
      labs(title = current_col) +
      ylab('') + 
      theme(plot.title = element_text(size=8),legend.position="none")
    
    p1_eu = p1_eu + 1
  }else{
    plot_listcat_eu[[p2_eu]]<- ggplot(df_km_eu, aes_string( fill = current_col,x = 'CLUSTER')) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = current_col) +
      ylab('')+ 
      theme(plot.title = element_text(size=8),legend.text=element_text(size=7))+ 
      guides(fill=guide_legend(title=""))
    
    p2_eu = p2_eu + 1
  }
}

annotate_figure(ggarrange(plotlist=plot_listnum_eu), 
                text_grob("Kmeans(eu) - Numerical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

annotate_figure(ggarrange(plotlist=plot_listcat_eu), 
                text_grob("Kmeans(eu) - Categorical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))
# Build cluster on top of largest cluster (cluster 1)
km_eu3 <- kmeans(eu,3)
table(km_eu3$cluster)
table(km_eu$cluster,km_eu3$cluster)

# compare with action taken -----------------------------------------------

## gd ##
(km_gdtab <- as.matrix(table(km$cluster,df_action_noout)))
d <- diag(1, nrow(km_gdtab))
(adjusted_km_gdtab <- pMatrix.min(km_gdtab,d))

#get accuracy
sum(diag(adjusted_km_gdtab))/sum(adjusted_km_gdtab) 

## eu ##
(km_eutab <- as.matrix(table(km_eu$cluster,df_action_noout)))
d <- diag(1, nrow(km_eutab))
(adjusted_km_eutab <- pMatrix.min(km_eutab,d))

#get accuracy
sum(diag(adjusted_km_eutab))/sum(adjusted_km_eutab) 

