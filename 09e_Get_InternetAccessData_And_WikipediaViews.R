# Include Internet Access Data


# Observed Time Frame of Congress Edits: 2005-2016

# Using Internet Access data from 2011, which consitutes the middle of the observed time frame 
# Data taken from United States Census Bureau
# Link to Source: https://www.census.gov/topics/population/computer-internet/data/tables.All.html
# Does not include data for 8 MoCs from Puerto Rico, American Samoa, Guam, Virgin Islands, and Nothern Mariana Islands


#-------------------------------------------------------------------------------------------------------------------
#------------------------------ Getting data on Internet Access for all States -------------------------------------
#-------------------------------------------------------------------------------------------------------------------


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

names(data_Internet)[names(data_Internet) == "State"] <- "STATE"

# Transform State-names into upper case to merge it later with Main Dataset
data_Internet <- mutate_each(data_Internet , funs(toupper))


#-------------------------------------------------------------------------------------------------------------------
#----------------------------------------------         END          -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#----------------------- Getting data on Popularity (Sum Views over time frame on Wiki-Profile) --------------------
#-------------------------------------------------------------------------------------------------------------------



# Get Column for popularity of MoCs from Page Views 


#Question: Are only edits included that were made during the time an MoC was serving? 
# No, edits are from the whole observed time frame



# Some MoCs are included in both the senate and house data set (with the same values for pageid and traffic for the same days)
# Therefore, I will filter out the Senators pageids from the House-Dataset and then merge both 
# House-Data starts in 2009, therefore all page views between 2009 and 2016 will be aggregated and used as variable (which is not ideal but still does the job)
# As the data does not in 2005 (but in 2009) and Views are not filtered for whether MoC was serving during that time, the variable will be not very precise
# To address this problem I will sort View Data into 5 Categories from most popular to least popular
# There was no other way to just get views during tenure (as there would be no reasonable values for MoC that only served before 2009)
# An ideal varialbe would include the time frame 2005-2016 and would only count views during the time a MoC served and divide by days served


# another way to do it: 

a <- house_us_traffic
b <- senate_us_traffic
ab <- rbind(a, b)

# Reduce dataset to pageids that are in main data set = Characteristics_Data_MoCs
list_unique_pageid_CharacteristcsDataSet <- unique(Characteristics_Data_MoCs$pageid)
abc <- subset(ab, pageid  %in% list_unique_pageid_CharacteristcsDataSet)
length(unique(abc$pageid)) # = 981

abcd <- unique(abc) # dropping rows that are doubles (due to MoCs serving in both chambers)
length(unique(abcd$pageid))

# Creating Year-Column
abcd$date <- as.character(abcd$date)
abcd$Year <- stringr::str_extract(abcd$date  , "^\\d{4}") 
abcd$Year <- as.double(abcd$Year)

# Filter for timeframe (but as data for house starts in 2009, it will be shifted back to 2009-2018)
abcde <- abcd %>%  filter(Year >= 2009) %>%  filter(Year <= 2018)
length(unique(abcde$pageid))

# Aggregate View Data by pageid
abcdef <- abcd  %>% dplyr::select(c(pageid, traffic))
abcdef <-  aggregate(abcdef[, 2], list(abcdef$pageid), sum)

names(abcdef  )[names(abcdef ) == "Group.1"] <- "pageid"
names(abcdef  )[names(abcdef  ) == "x"] <- "ProfileViewsSum09_to_16"

length(unique(abcdef$pageid))

# Using Summary Statistics to Compute Categories for High of Number of Views (1-5)
summary(abcdef$ProfileViewsSum09_to_16)

# Categories: 1. lower 25%, 2. 26%-50%, 3. 51%-75%, 4. 76%-95% , 5. 96%-100%

abcdef$ViewCategory = 0

for(i in 1:length(abcdef$ProfileViewsSum09_to_16)) {
  if (abcdef$ProfileViewsSum09_to_16[i] <= 177684) {
    abcdef$ViewCategory[i] = 1  #firtst quartile
  }
  if (abcdef$ProfileViewsSum09_to_16[i] > 177684 && abcdef$ProfileViewsSum09_to_16[i] <= 302908 ) {
    abcdef$ViewCategory[i] = 2  # second quartile
  }
  if (abcdef$ProfileViewsSum09_to_16[i] > 302908 && abcdef$ProfileViewsSum09_to_16[i] <= 757801 ) {
    abcdef$ViewCategory[i] = 3  # third quartile
  }
}

# Get last 5% - threshold: 
ShortLived_df <- abcdef%>% filter( ViewCategory == 0) #contains the last quartile -> taking the last two decils (25%*0,1*2 = 5%)
quantile(ShortLived_df$ProfileViewsSum09_to_16, prob = seq(0, 1, length = 11), type = 5) # 5% = bigger than 3604214

# Use 5% threshold to compute last 2 categories: 

for(i in 1:length(abcdef$ProfileViewsSum09_to_16)) {
  if (abcdef$ProfileViewsSum09_to_16[i] > 757801 && abcdef$ProfileViewsSum09_to_16[i] <= 3604214 ) {
    abcdef$ViewCategory[i] = 4   #forth quartile minus the top 5%
  }
  if (abcdef$ProfileViewsSum09_to_16[i] > 3604214) {
    abcdef$ViewCategory[i] = 5  # last 5% 
  }
}



#-------------------------------------------------------------------------------------------------------------------
#----------------------------------------------         END          -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#---------------------  Merging Internet and View Data with Characertistics_Data_Mocs   ----------------------------
#-------------------------------------------------------------------------------------------------------------------


Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, abcdef  , by ="pageid")
Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, data_Internet , by ="STATE")


#Out: Characteristics_Data_MoCs
# Saving Dataframe 
save(Characteristics_Data_MoCs , file = "Characteristics_Data_MoCs.Rdata")




#-------------------------------------------------------------------------------------------------------------------
#----------------------------------------------         END          -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------










###### -------------------------- Ideenspeicher--------------------



# Former Approach to Aggregate View Data: 

# Aggretating House Views
house_us_traffic_tf <- house_us_traffic

# Filter for pageids included in main dataset (to reduce size of house_us_traffic_tf and make it easier to work with the data)
list_unique_pageid_CharacteristcsDataSet <- unique(Characteristics_Data_MoCs$pageid)
house_us_traffic_tf <- subset(house_us_traffic_tf, pageid  %in% list_unique_pageid_CharacteristcsDataSet)

house_us_traffic_tf$date <- as.character(house_us_traffic_tf$date)

house_us_traffic_tf$Year <- stringr::str_extract(house_us_traffic_tf$date  , "^\\d{4}") 

house_us_traffic_tf$Year <- as.double(house_us_traffic_tf$Year)

# The following commentaries are not aktuell anymore, as I decided to takle the problem differntly 
# Create new column pageid_Year which can be used for merging later
# All MoH that are in Characteristics_Data_MoC are also in this data set included with respective pageid
# But ca. 120 from those in Characteristics_Data_MoC served before 2009, and therefore there is no view data during tenure for them
#house_us_traffic_tf <- house_us_traffic_tf  %>%  unite(pageid_Year, pageid, Year, sep = "_", remove = FALSE)

# Filter for time frame (but as data for house starts in 2009, it will be 2009-2016 )
house_us_traffic_tf <- house_us_traffic_tf %>%  filter(Year >= 2009) %>%  filter(Year <= 2016)

house_us_traffic_tf <- house_us_traffic_tf %>% dplyr::select(c(pageid, traffic))
house_us_traffic_tf <-  aggregate(house_us_traffic_tf[, 2], list(house_us_traffic_tf$pageid), sum)

names(house_us_traffic_tf  )[names(house_us_traffic_tf  ) == "Group.1"] <- "pageid"
names(house_us_traffic_tf  )[names(house_us_traffic_tf  ) == "x"] <- "ProfileViewsSum09_to_16"




# Aggregating Senate Views
senate_us_traffic_tf <- senate_us_traffic

# Filter for pageids included in main dataset (to reduce size of house_us_traffic_tf and make it easier to work with the data)
senate_us_traffic_tf <- subset(senate_us_traffic_tf, pageid  %in% list_unique_pageid_CharacteristcsDataSet)


senate_us_traffic_tf$date <- as.character(senate_us_traffic_tf$date)

senate_us_traffic_tf$Year <- stringr::str_extract(senate_us_traffic_tf$date  , "^\\d{4}") 

senate_us_traffic_tf$Year <- as.double(senate_us_traffic_tf$Year)

# Filter for timeframe (but as data for house starts in 2009, it will be 2009-2016 )
# Can not start in 2007 because Senator would have systematic more views compared to MoHs
# 2 Senator that are in Characeristics_Data_MoC are not included in senate_us_traffic (Jack Reed, George Allen)
senate_us_traffic_tf <- senate_us_traffic_tf %>%  filter(Year >= 2009) %>%  filter(Year <= 2016)

senate_us_traffic_tf <- senate_us_traffic_tf %>% dplyr::select(c(pageid, traffic))
senate_us_traffic_tf <-  aggregate(senate_us_traffic_tf[, 2], list(senate_us_traffic_tf$pageid), sum)

names(senate_us_traffic_tf  )[names(senate_us_traffic_tf  ) == "Group.1"] <- "pageid"
names(senate_us_traffic_tf  )[names(senate_us_traffic_tf  ) == "x"] <- "ProfileViewsSum09_to_16"




# Filter for MoCs that are both in datasets (because they served in both chambers)

list_Senators <- unique(senate_us_traffic_tf$pageid)

`%notin%` <- Negate(`%in%`) # own definition of negated %in%
house_us_traffic_tf <- subset(house_us_traffic_tf, pageid  %notin% list_Senators)


# Join both datasets (house and senate)

Views_Both_Chambers <- rbind(house_us_traffic_tf, senate_us_traffic_tf)

length(unique(Views_Both_Chambers$pageid))  # no doubles in pageid


VV <- BothChambers_Data_MoCs %>%  filter(country == "USA-S")








