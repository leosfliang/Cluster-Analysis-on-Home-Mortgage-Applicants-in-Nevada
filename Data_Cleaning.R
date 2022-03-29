library(tidyverse)
library(cluster)
# import data -------------------------------------------------------------
data_file <- 'hmda_2017_nv_all-records_labels.csv'

df_orig <- read.csv(
  data_file,
  na = c(
    '',
    'Information not provided by applicant in mail, Internet, or telephone application',
    'Not applicable'
  ),
  stringsAsFactors = T
)


# Check and remove duplicate rows ---------------------------------------------------------

df_orig <- df_orig[which(!duplicated(df_orig)),]

# remove columns ----------------------------------------------------------
remove <- c(
  'as_of_year',
  'agency_name',
  'respondent_id',
  'agency_code',
  'loan_type',
  'property_type',
  'loan_purpose',
  'owner_occupancy',
  'preapproval',
  'action_taken',
  'msamd',
  'state_name',
  'state_abbr',
  'state_code',
  'county_code',
  'applicant_ethnicity',
  'co_applicant_ethnicity',
  'applicant_race_1',
  'applicant_race_name_2',
  'applicant_race_2',
  'applicant_race_name_3',
  'applicant_race_3',
  'applicant_race_name_4',
  'applicant_race_4',
  'applicant_race_name_5',
  'applicant_race_5',
  'co_applicant_race_1',
  'co_applicant_race_name_2',
  'co_applicant_race_2',
  'co_applicant_race_name_3',
  'co_applicant_race_3',
  'co_applicant_race_name_4',
  'co_applicant_race_4',
  'co_applicant_race_name_5',
  'co_applicant_race_5',
  'applicant_sex',
  'co_applicant_sex',
  'purchaser_type',
  'denial_reason_name_1',
  'denial_reason_1',
  'denial_reason_name_2',
  'denial_reason_2',
  'denial_reason_name_3',
  'denial_reason_3',
  'rate_spread',
  'hoepa_status_name',
  'hoepa_status',
  'lien_status',
  'edit_status_name',
  'edit_status',
  'sequence_number',
  'application_date_indicator'
)

df_sub <- df_orig[,which(!names(df_orig) %in% remove)]


# Check missing values per column -----------------------------------------

vecMissCol <- numeric()
for (i in 1:ncol(df_sub)) {
  numMissC = sum(is.na(df_sub[,i]))
  propMissC = numMissC/nrow(df_sub)
  vecMissCol[i] <- round(propMissC *100,2)
}

dfColMiss <- data.frame(Variables = names(df_sub), MissingPerc = vecMissCol)
View(dfColMiss)

#variable with missing value proportion > 25%
hi_miss_vari <- dfColMiss[which(dfColMiss$MissingPerc > 25),'Variables']

df_sub_2 <- df_sub[,which(!names(df_sub) %in% hi_miss_vari)]
df_sub_complete <- df_sub_2[which(complete.cases(df_sub_2)),]

# Check outlier -----------------------------------------------------------

df_num <- df_sub_complete %>% select_if(is.numeric)

mdist <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
pval <- pchisq(mdist, df=ncol(df_num)-1, lower.tail=FALSE)
alpha <- 0.05
sum(pval < alpha) #5634 outliers

df_sub_complete_noout <- df_sub_complete[which(pval >= alpha),]


# Write cleaned data to csv -----------------------------------------------

write.csv(df_sub_complete_noout,'hmda_2017_nevada_cleaned.csv')


