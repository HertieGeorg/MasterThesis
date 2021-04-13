# Include Internet Access Data


# Observed Time Frame of Congress Edits: 2005-2016

# Using Internet Access data from 2011, which consitutes the middle of the observed time frame 
# Data taken from United States Census Bureau
# Link to Source: https://www.census.gov/topics/population/computer-internet/data/tables.All.html

library(readxl)
US_Census_Bureau_Computer_and_Internet_Access_in_the_United_States_2011 <- read_excel("Input_Data on Election Dates and Vote Margins /Internet Access/US_Census_Bureau_Computer and Internet Access in the United States- 2011.xls")

data_Internet <- US_Census_Bureau_Computer_and_Internet_Access_in_the_United_States_2011 
data_Internet <- data_Internet %>%  drop_na()

#Choose only columns that contain percentage values
data_Internet <- data_Internet %>% dplyr::select(-c("...2","...3","...5"))
names(data_Internet)[names(data_Internet) == "candidatevotes"] <- "candidatesvotes_first_max"

#rename columns
colnames(data_Internet) <- c("State", "Individual accesses the Internet from some location (Percent)", "Individual lives in household with Internet use (Percent)") 

#Using column 3: "Individual lives in household with Internet use (Percent)" as independ variable for the model 
data_Internet <- data_Internet %>% dplyr::select(-c("Individual accesses the Internet from some location (Percent)"))
data_Internet <- data_Internet [-c(1), ] # Drop row for all of US

# Next: merge with Characterisics-MoCs Dataset 


# Get Column for popularity of MoCs from Page Views 


#Question: Are only edits included that were made during the time an MoC was serving? 
# No, edits are from the whole observed time frame

# Vorgehen: 
# Get traffic data for house and senate 
# Filter for time frame 
# Aggregate by pageid, (check whether MoC that served in both chambers have entries in both lists for the same day)
# Merge Senate and House 
# Create 10 categories and sort MoC into it from most popular to least popular 
# Merge with main data


# load and attach legislatoR and dplyr
library(legislatoR)
library(dplyr)

 # assign entire Core table for the German Bundestag into the environment


deu_politicians <- get_core(legislature = "deu")



install.packages("legislatoR")

  
devtools::install_github("saschagobel/legislatoR")

R.Version()














