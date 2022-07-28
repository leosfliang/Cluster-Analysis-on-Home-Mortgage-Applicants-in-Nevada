# Project Overview

This project applied different clustering methods to conduct clustering analysis on home loan applicants based on their quantitative and qualitative features, in order to help mortgage specialist to make their decision on loan applications.

First, we applied the clustering methods KMeans and KMedoid on quantitative features only, and then implemented DBSCAN and Hierarchy clustering methods on all featured.

Next, we compared all different approaches and come to a conclusion.

For the mortgage specialists, they can probably look into qualitative criteria before evaluating the applicant’s ability to repay, such as loan type, principal residence, and other criteria such as gender, race, age. We could apply the hierarchical clustering until all qualitative variables are well-split, then apply partition clustering within each cluster. In this way, we might see different selection criteria for repayment ability for different types of mortgage.

See details in report: [Report](https://github.com/leosfliang/Cluster-Analysis-on-Home-Mortgage-Applicants-in-Nevada/blob/main/Documents/%20Dana%204840%20Project%20Report.pdf)

# Documents
The project proposal, presentation ppt slides and the project report can be found in the Documents folder

# Original DataSet

original data set can be downloaded [HERE](https://files.consumerfinance.gov/hmda-historic-loan-data/hmda_2017_nv_all-records_labels.zip)

The data set contains home loan information for the state of Nevada in 2017

# R files

### **Data_Cleaning.R** 
  contains the the data cleaning code

  use the original data for running this file

  the script contains:  
- checking for duplicates  
- creating a binary variable indicating whether co-applicant exist  
- removing unwanted variables  
- Check missing values per column  
- checking and removing numerical outliers  
- converting action_taken to binary  
- taking a sample of the data  
- reseting the factors  


### **hmda_2017_nevada_cleaned.csv** 
  - the data set created after running **Data_Cleaning.R** 
  
### **function.R**
  - contains necessary functions

### **kmean.R**
- Kmeans method analysis

### **kmedoid.R**
- kmedoid method analysis

### **DBSCAN.R**
- Dbscan method analysis

### **Hierarchy.R**
- Hierarchical method analysis

## Plots
  - plots folder contains visualization of the analysis
