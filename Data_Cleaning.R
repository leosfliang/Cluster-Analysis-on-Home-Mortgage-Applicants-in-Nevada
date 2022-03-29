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

# create column indicating whether co-applicant exist ---------------------

df_orig$co_applicant <-
  as.factor(ifelse(df_orig$co_applicant_sex_name == 'No co-applicant',
                   'no',
                   'yes'))

# Check and remove duplicate rows -----------------------------------------

df_orig <- df_orig[which(!duplicated(df_orig)),]

rownames(df_orig)=NULL

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
  'application_date_indicator',
  'property_type_name',
  'co_applicant_ethnicity_name',
  'co_applicant_race_name_1',
  'co_applicant_sex_name',
  'msamd_name',
  'lien_status_name',
  'owner_occupancy_name',
  'hud_median_family_income',
  'census_tract_number'
)

df_sub <- df_orig[,which(!names(df_orig) %in% remove)]

rownames(df_sub)=NULL

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

#remove variables with hi missing value prop
df_sub_2 <- df_sub[,which(!names(df_sub) %in% hi_miss_vari)]

rownames(df_sub_2)=NULL

#keep only complete rows
df_sub_complete <- df_sub_2[which(complete.cases(df_sub_2)),]

rownames(df_sub_complete)=NULL

# Check outlier -----------------------------------------------------------

df_num <- df_sub_complete %>% select_if(is.numeric)

mdist <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
pval <- pchisq(mdist, df=ncol(df_num)-1, lower.tail=FALSE)
alpha <- 0.05
sum(pval < alpha) #5634 outliers

df_sub_complete_noout <- df_sub_complete[which(pval >= alpha),]

# looking at 'action_taken' -----------------------------------------------

table(action_taken = df_sub_complete_noout$action_taken_name)

  # remove 'Preapproval request denied by financial institution'

df_sub_complete_noout <-
  df_sub_complete_noout[which(
    df_sub_complete_noout$action_taken_name !=
      'Preapproval request denied by financial institution'
  ),]
# Check Cat variable ------------------------------------------------------
#reset factor
df_sub_complete_noout <- df_sub_complete_noout %>% 
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), as.factor))

summary(df_sub_complete_noout)
# Write cleaned data to csv -----------------------------------------------

rownames(df_sub_complete_noout)=NULL
write.csv(df_sub_complete_noout,'hmda_2017_nevada_cleaned.csv',row.names=F)


