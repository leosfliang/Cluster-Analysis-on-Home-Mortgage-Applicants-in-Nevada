library(tidyverse)
library(cluster)
library(ggpubr)

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

# all variable ------------------------------------------------------------

gd_kmed <- daisy(df_main, metric = 'gower')

getOptK(dist = gd_kmed, type = 'pam')
#sw -> k = 5

kmed <- pam(gd_kmed,5,diss = TRUE)

df_kmed <- df_main
df_kmed$CLUSTER <- as.factor(kmed$cluster)
table(kmed$cluster)

#### plot kmeans gd clusters ####
plot_listnum_kmed <- list()
plot_listcat_kmed <- list()
var_name <- names(df_kmed)
p1 = 1
p2 = 1
for (i in 1:(ncol(df_kmed)-1)){
  current_col <- colnames(df_kmed)[i]
  if (is.numeric(df_kmed[,i])){
    plot_listnum_kmed[[p1]] <- ggplot(df_kmed, aes_string('CLUSTER', 
                                                   current_col,
                                                   group = 'CLUSTER',
                                                   color = 'CLUSTER')) +
      geom_boxplot()  +
      labs(title = current_col) +
      ylab('') + 
      theme(plot.title = element_text(size=10),legend.position="none")
    
    p1 = p1 + 1
  }else{
    plot_listcat_kmed[[p2]]<- ggplot(df_kmed, aes_string( fill = current_col,x = 'CLUSTER')) +
      geom_bar(position = "dodge")+
      labs(title = current_col) +
      ylab('') +
      theme(plot.title = element_text(size=8),legend.text=element_text(size=7))+ 
      guides(fill=guide_legend(title=""))
    
    p2 = p2 + 1
  }
}

annotate_figure(ggarrange(plotlist=plot_listnum_kmed), 
                text_grob("K-medoids(gd) - Numerical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

annotate_figure(ggarrange(plotlist=plot_listcat_kmed), 
                text_grob("K-medoids(gd) - Categorical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

# Numerical variables with eu ---------------------------------------------

eu_kmed <- dist(scale(df_num))

getOptK(dist = eu_kmed , type = 'kmean')
#k =2

kmed_eu <- pam(eu_kmed,2,diss = TRUE)

df_kmed_eu <- df_main
df_kmed_eu$CLUSTER <- as.factor(kmed_eu$cluster)
table(kmed_eu$cluster)

#### plot kmeans gd clusters ####
plot_listnum_kmed_eu <- list()
plot_listcat_kmed_eu <- list()
var_name <- names(df_kmed_eu)
p1 = 1
p2 = 1
for (i in 1:(ncol(df_kmed_eu)-1)){
  current_col <- colnames(df_kmed_eu)[i]
  if (is.numeric(df_kmed_eu[,i])){
    plot_listnum_kmed_eu[[p1]] <- ggplot(df_kmed_eu, aes_string('CLUSTER', 
                                                        current_col,
                                                        group = 'CLUSTER',
                                                        color = 'CLUSTER')) +
      geom_boxplot()  +
      labs(title = current_col) +
      ylab('') + 
      theme(plot.title = element_text(size=10),legend.position="none")
    
    p1 = p1 + 1
  }else{
    plot_listcat_kmed_eu[[p2]]<- ggplot(df_kmed_eu, aes_string( fill = current_col,x = 'CLUSTER')) +
      geom_bar(position = "dodge")+
      labs(title = current_col) +
      ylab('') +
      theme(plot.title = element_text(size=8),legend.text=element_text(size=7))+ 
      guides(fill=guide_legend(title=""))
    
    p2 = p2 + 1
  }
}

annotate_figure(ggarrange(plotlist=plot_listnum_kmed_eu), 
                text_grob("K-medoids(gd) - Numerical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

annotate_figure(ggarrange(plotlist=plot_listcat_kmed_eu), 
                text_grob("K-medoids(gd) - Categorical Data", 
                          color = "red",
                          face = "bold", 
                          size = 14))

# compare with action taken -----------------------------------------------
## gd ##
(kmed_gdtab <- as.matrix(table(kmed$cluster,df_sample$action_taken_name)))
d <- diag(1, nrow(kmed_gdtab))
(adjusted_kmed_gdtab <- pMatrix.min(kmed_gdtab,d))

#get accuracy
sum(diag(adjusted_km_gdtab))/sum(adjusted_km_gdtab) 


## eu ##
(kmed_eutab <- as.matrix(table(kmed_eu$cluster,df_sample$action_taken_name)))
d <- diag(1, nrow(kmed_eutab))
(adjusted_kmed_eutab <- pMatrix.min(kmed_eutab,d))

sum(diag(adjusted_kmed_eutab))/sum(adjusted_kmed_eutab) 

