# Original DataSet

original data set can be downloaded [HERE](https://files.consumerfinance.gov/hmda-historic-loan-data/hmda_2017_nv_all-records_labels.zip)

The data set contains home loan information for the state of Nevada in 2017

# Data Cleaning 

### **Data_Cleaning.R** 
contains the the data cleaning code

use the original data for running this file

the script contains: 
  - checking for duplicates
  - creating a binary variable indicating whether co-applicant exist
  - removing unwanted variables
  - Check missing values per column
  - keeping only rows with no outliers
  - checking and removing numerical outliers
  - converting action_taken to binary
  - reseting the factors


### **hmda_2017_nevada_cleaned.csv** 
the data set created after running **Data_Cleaning.R** 
