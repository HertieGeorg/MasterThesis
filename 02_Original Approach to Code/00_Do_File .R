# Do File: brings all code together and is  the main file of the project 

setwd("~/Documents/GitHub/MasterThesis")

# Load the required packages

source("01_install_packages.R")



# Load and clean Data 

source("02_load_and_clean_data.R")
# in: "Data/Batch_200780_batch_results.csv" (mturk classification data), "Data/codebook_edits_classification.xlsx" (variable names)
# out: mturk_df 

#source("03_Load_Legislator_Database.R")
# Data is loaded and saved in order "LegislatoR_Data"; it is not recommended to load the data each time as files are up to 200mb big 
# in: /
# out: LegislatoR data for US house and senate (core, history, political, profession, traffic)

#source("04_Load_and_Clean_Election_Dates_Data.R")
# in: Election_Dates_196slers1967to2016_20180908.rdata
# out: election_data, list_of_biggest_general_election_dates, list_of_all_general_election_dates

#source("05_scrape_edit_dates.R")
# Data is loaded and saved as "main_dataframe_dates.Rdata" it is not recommended to load the data each time as scraping takes up to 30min 
# in: mturk_df
# out: main_dataframe_dates 


# Descriptive Analysis of Data

source("06_descriptive_Analysis_of_edits_over_time.R") 
# main_dataframe_dates contains all classified edits of MPs proflies from IP-adressses from Congress and the dates of the respective edits
# this analysis mainly focuses on the distribution of different types of edits over time 
# in: main_dataframe_dates 
# out: several visualizations 


# Merge LegislatoR and MTurk 

# To do next: create the following file (maybe use 0?_Merge_Legislator_Mturk_table)
source("07_descriptive_Analysis_of_edits_based_on_MP_characteristics.R") #doesnt exist yet 
# this analysis maily focuses on the distribution of different types of edits based on the characteristica of MPs 
# in: main_dataframe_dates merged with LegistlatorR data -> still needs to be done


# Analysis of edits data as preparation for Regression Model Choice 


source("08_creating_MoC_Main_Dataset.R") 
# using LegislatoR data to create a dataset that contain all MoC that served in session 109-114 and  their characteristics
# characteristics: pageied, name, sex, ethnicity, religion, birth, death, 
# & party, chamber, days_served, state, competitevness of electoral district, total edits of their Wik-profiles, 
# in: LegislatoR data (core + political), legislatoR historical (to count overall Wik-edits), Harvard-Election dataset (for competitiveness)
# out: BothChambers_Data_MoCs 


source("09_Counting_Congress_Edits.R") 
# adding the number of edits coming from within Congress to main dataset that contains information about all MoCs (serving in 109-114)
# in: BothChambers_Data_MoCs, main_dataframe_dates
# out: main_dataframe_core, Characteristics_Data_MoCs 
#(also out: Inside_Congress_Edits_Politically_PositiveNegative -> list of edits from Congress with Dummy for positive and negative political motivation)
#(also out: Inside_Congress_Edits_Politically -> list of edits from within Congress wiht Dummy for politically motivation)



source("10_analysis_distribution_of_edits.R") 
# this analysis looks into the distribution and variance of edits data
# e.g. is edit data negatively skewed? 
# and other test-statistics 
# in: 
# out:  Figure showing distribution of number of edits per profile; Lists with MoCs that see edits and that do not see edits (separatley)



