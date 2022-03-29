library(tidyverse)

# import data -------------------------------------------------------------
data_file <- 'hmda_2017_nv_first-lien-owner-occupied-1-4-family-records_labels.csv'

df_orig <- read.csv(
  data_file,
  na = c(
    '',
    'Information not provided by applicant in mail, Internet, or telephone application',
    'Not applicable'
  ),
  stringsAsFactors = T
)
