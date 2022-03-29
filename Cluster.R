library(tidyverse)
library(cluster)

# import cleaned data -----------------------------------------------------

df_clean <- read.csv('hmda_2017_nevada_cleaned.csv',stringsAsFactors = T)


# remove 'action_taken' for now -------------------------------------------

df_main <- df_clean[,which(names(df_clean) != 'action_taken_name')]

gd <- daisy(df_main, metric = 'gower')
