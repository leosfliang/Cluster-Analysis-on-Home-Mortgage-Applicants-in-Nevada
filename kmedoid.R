library(tidyverse)
library(cluster)
library(ggpubr)

source('function.R')
# import cleaned data -----------------------------------------------------

df_sample <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)
df_sample$action_taken_name <- as.factor(df_sample$action_taken_name)

summary(df_sample)
# remove 'action_taken' for now -------------------------------------------

df_main <- df_sample[,which(names(df_sample) != 'action_taken_name')]

df_num <- df_sample %>% select_if(is.numeric)

# all variable ------------------------------------------------------------

gd_kmed <- daisy(df_main, metric = 'gower')

getOptK(dist = gd_kmed, type = 'pam')
#sw -> k = 2

kmed <- pam(gd_kmed,2,diss = TRUE)

df_kmed <- df_main
df_kmed$CLUSTER <- as.factor(kmed$cluster)

# Numerical variables with eu ---------------------------------------------

eu_kmed <- dist(scale(df_num))

getOptK(dist = eu_kmed , type = 'kmean')
#k =2

kmed_eu <- pam(eu_kmed,2,diss = TRUE)

df_kmed_eu <- df_num
df_kmed_eu$CLUSTER <- as.factor(kmed_eu$cluster)

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

