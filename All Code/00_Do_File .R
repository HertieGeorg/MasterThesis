# Do File: brings all code together and is the main file of the project 


# Load the required packages

source("01_install_packages.R")

# Load and clean Data 

source("02_load_and_clean_data.R")

source("03_Load_Legislator_Database.R")

source("04_Load_and_Clean_Election_Dates_Data.R")
# in: Election_Dates_196slers1967to2016_20180908.rdata
# out: election_data, list_of_biggest_general_election_dates, list_of_all_general_election_dates

source("05_scrape_edit_dates.R")
# Data is loaded and saved as "main_dataframe_dates.Rdata" it is not recommended to load the data each time as scraping takes up to 30min 
# in: mturk_df
# out: main_dataframe_dates 



# Descriptive Analysis of Data

source("06_Creating_Main_Dataset.R") 
# main_dataframe_dates contains all classified edits of MPs proflies from IP-adressses from Congress and the dates of the respective edits
# this analysis mainly focuses on the distribution of different types of edits over time 
# in: main_dataframe_dates 
# out: several visualizations 

source("09a_Figures_EditsOverTime_DescriptiveStatistics.R") 

source("09b_Difference_Political_Maintenance_Edits.R") 

source("09c_Analysis_Edits_by_Category.R") 

source("10a_Analysis_Skewedness_Edits.R") 

source("11a_Exp_ZINB_Analysis_All_Edits.R")
# calls on 12_(Table1&2)_Exponentiated_Coefficients_All_Edits.R as well

source("11d_Exp_ZINB_Analysis_Positive_Political_Edits.R")
# calls on 12_(Table3&4)_Exponentiated_Coefficients_BP_Edits.R as well 








