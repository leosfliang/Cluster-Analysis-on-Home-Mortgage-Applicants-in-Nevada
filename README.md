# Original DataSet

original data set can be downloaded [HERE](https://files.consumerfinance.gov/hmda-historic-loan-data/hmda_2017_nv_all-records_labels.zip)

The data set contains home loan information for the state of Nevada in 2017

# R files

## **Data_Cleaning.R** 
  contains the the data cleaning code

  use the original data for running this file

  the script contains: 
    - checking for duplicates
    - creating a binary variable indicating whether co-applicant exist
    - removing unwanted variables
    - Check missing values per column
    - checking and removing numerical outliers
    - converting action_taken to binary
    - Taking a sample of the data
    - reseting the factors


### **hmda_2017_nevada_cleaned.csv** 
  the data set created after running **Data_Cleaning.R** 
  
## **function.R**
  - contains necessary functions

## **kmean.R**

## **kmedoid.R**

# Plots
  -save plots in plots folder
