#09_creating_Main_Dataset.R

# What data is needed for this analysis? 
# A dataset, the Main-Dataset that contains:
# 1. count data, i.e. how many edits each MoC has seen over the observed time frame
# 1a. count data divided by number of sessions served out of the sessions included 
  #Therefore: decide on what MoCs are included, and aggregate edits 
  # Add category of politically motivated edits and create a dummy 
# 2. characteristics data, i.e. age at the time of edit, Senate/House Dummy, race, sex, dummy for competitive districts 
  #Therefore: need to define competitiveness, e.g. based on how close a race was 


#pageid, name, sex, race, date of birth, year of birth, date of death, House-Dummy, Senate-Dummy, Dummy for competitive district, race, count edits, 





#-------------------------------------------------------------------------------------------------------------------
#------------- Merging LegislatoR-Data, Filtering for 109th-114th Sessions, Creating Dummies--------------------------
#-------------------------------------------------------------------------------------------------------------------

#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
Merged_House_Data <- merge(house_us_core, house_us_political, by="pageid")
Merged_Senate_Data <- merge(senate_us_core, senate_us_political, by="pageid")

# Filter for Sessions we want to observe: 109th - 114th Session
House_Session_109_to_114 <- Merged_House_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
Senate_Session_109_to_114 <- Merged_Senate_Data %>%  filter(session <= 114) %>%  filter(session >= 109)

# Only keep the first 17 variables (other not needed)
House_Session_109_to_114_Short <- House_Session_109_to_114  %>% dplyr::select(1:18)
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114 %>% dplyr::select(1:18)

# Getting rid of 2 more variables that are not needed 
House_Session_109_to_114_Short <- House_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, deathplace))
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, deathplace))

#Adding Chamber Column (House or Senate) before merging
House_Session_109_to_114_Short$Chamber <- "House"
Senate_Session_109_to_114_Short$Chamber <- "Senate"

#Checking whether both dataframes hold the same set of variables:
colnames(Senate_Session_109_to_114_Short)
colnames(House_Session_109_to_114_Short) #looks good! 

#Merge House and Senate data from Legislator iinto one data frame
BothChambers_Data <- rbind(House_Session_109_to_114_Short, Senate_Session_109_to_114_Short)

# Creating Chamber Dummies
BothChambers_Data <- BothChambers_Data %>% fastDummies::dummy_cols(select_columns = "Chamber")
# Create Sex Dummy
BothChambers_Data <- BothChambers_Data %>% fastDummies::dummy_cols(select_columns = "sex",  remove_first_dummy = TRUE)
# Create Ethnicity Dummies
BothChambers_Data <- BothChambers_Data %>% fastDummies::dummy_cols(select_columns = "ethnicity")
# Create Party Dummies
BothChambers_Data <- BothChambers_Data %>% fastDummies::dummy_cols(select_columns = "party")

# Creating variable that indicates how many days a MoCs has served during the 6 sessions observed 
# variable "service" indicats the period of service in days during the respective session of a legislator
DaysService_Count <- BothChambers_Data
DaysService_Count =  DaysService_Count %>% dplyr::select(c(pageid, service))
DaysService_Count  <- aggregate(DaysService_Count$service, by=list(pageid= DaysService_Count$pageid), FUN=sum)
names(DaysService_Count )[names(DaysService_Count) == "x"] <- "Days_Served"
#Merge with Main Data Set
BothChambers_Data <- merge(BothChambers_Data, DaysService_Count, by = "pageid")

# How many unique MoCs are contained in our dataset? -> 981
length(unique(BothChambers_Data$pageid)) 

# Getting rid of variables that are not longer needed and that prevent sorting for unique MoCs
#BothChambers_Data  <- BothChambers_Data  %>% dplyr::select(-c(session, session_end, session_start, service, country))

# If I want to reduce the dataset to unique MoCs, there are still some MoCs that are included several times
# BothChambers_Data_Unique <- unique(BothChambers_Data) #1129 entries, also 148 mehr 
# this is due to changes in Chamber and Electoral district 
# Ohne constituency: 1018 entries (37 mehr), which can be accounted to a change from House to Senate by a MoC
# I have to take out constituency, as MoCs do change the districts they are running in (this happens around 110 times in the observed time frame)
# But as I want to have the competitiveness of a district as variable in my analysis, I have to deal with that first 

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------









#-------------------------------------------------------------------------------------------------------------------
#----------------------- Voting Data - House: Creating Vote Margin Variable and Merging-ID -------------------------
#-------------------------------------------------------------------------------------------------------------------


# Getting Vote Margin Data 
X1976_2018_house3 <- read_csv("Data on Election Dates and Vote Margins /Vote Margins/Dataverse Harvard1976-2018-house/1976-2018-house3.csv")
vote_data_house <- X1976_2018_house3  #data does not include Oversee Terretories like Guam, Puerto Rico, Washington D.C. etc.

# Creating an id to be able to merge both datasets: id of vote margin = State - District - Election Year + 1
vote_data_house$session_start <- (vote_data_house$year + 1) # sessions stars one year after election
vote_data_house$Chamber <- "House"
vote_data_house <- vote_data_house %>%  unite(district_id, state, district, session_start, Chamber, sep = "_", remove = FALSE)
#### district: **Description**: district number ****Note****:	At-large districts are coded as 0 (zero).
# at-large districts are districts that contain a whole state: namely: Alaska, Delaware, Montana,North Dakota,South Dakota,Vermont,Wyoming

# Filter for the time frame that will be analysed 
vote_data_house <- vote_data_house %>%  filter(year >= 2004) %>%  filter(year <= 2016)
# Filter out primary elections (only keep general elections)
 # But First: dealing with NAs

for (i in 1:9819) {
  if(is.na(vote_data_house$stage[i])== T) {
    vote_data_house$stage[i] = "gen" }}

#Getting the relative vote margin in vote_data_house: -> vote_data_house$vote_maxdiff_relative
vote_data_house_group <- as.data.table(vote_data_house)
vote_data_house_group <- vote_data_house_group %>% dplyr::select(c(district_id, candidatevotes))
vote_data_house_group_max <- vote_data_house_group[vote_data_house_group[, .I[candidatevotes == max(candidatevotes)], by=district_id]$V1]
names(vote_data_house_group_max)[names(vote_data_house_group_max) == "candidatevotes"] <- "candidatesvotes_first_max"
# setting all max candidatevotes to 0
vote_data_house_group$candidatevotes[vote_data_house_group[, .I[candidatevotes == max(candidatevotes)], by=district_id]$V1] <- 00
vote_data_house_group_max_2 <- vote_data_house_group[vote_data_house_group[, .I[candidatevotes == max(candidatevotes)], by=district_id]$V1]
names(vote_data_house_group_max_2)[names(vote_data_house_group_max_2) == "candidatevotes"] <- "candidatesvotes_second_max"
# Merged the two columns contain the highest and second highest vote result
vote_data_house_group_max_1_2 <- merge(vote_data_house_group_max, vote_data_house_group_max_2, by = "district_id")
vote_data_house_group_max_1_2 <- as.data.frame(vote_data_house_group_max_1_2)
#Merge max vote data columns into vote_data_house dataset
vote_data_house <- merge(vote_data_house , vote_data_house_group_max_1_2, by = "district_id")
# Creating Cloumn with total difference of votes between winner and second best
vote_data_house$vote_maxdiff_absolute <- (vote_data_house$candidatesvotes_first_max - vote_data_house$candidatesvotes_second_max)
# Creating Cloumn with difference of votes between winner and second best relative to total votes 
vote_data_house$vote_maxdiff_relative <- (vote_data_house$vote_maxdiff_absolute / vote_data_house$totalvotes)

# Prepare for merging with main data
vote_data_house_merge <- vote_data_house %>% dplyr::select(c(district_id, vote_maxdiff_relative, vote_maxdiff_absolute))
vote_data_house_merge <- unique(vote_data_house_merge)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------









#-------------------------------------------------------------------------------------------------------------------
#--------------------------------- LegislatoR Data: creating a Merging ID for House Vote Margins--------------------
#-------------------------------------------------------------------------------------------------------------------


#Creating an id for main data: state_short + district_number + session_start_year + Chamber 
# Creating varialbe session_start_year
BothChambers_Data$session_start_year <- stringr::str_extract(BothChambers_Data$session_start, "\\d+") 
# Creating variable state_short and district_number 
BothChambers_Data$state_short <- BothChambers_Data$constituency
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "congressional district", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "at-large", "0")
BothChambers_Data$district_number  <- stringr::str_extract(BothChambers_Data$state_short, "\\d+") 
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "\\d+nd", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "\\d+st", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "\\d+th", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "\\d+rd", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "'s", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "\\d+", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, " $", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, " $", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, " $", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "^ ", "")
BothChambers_Data$state_short <- stringr::str_replace_all(BothChambers_Data$state_short, "^ ", "")

#Transform state names into uppercase 
for (i in 1:3319) {
  string <- BothChambers_Data$state_short[i] 
  STRING <- toupper(string)
  BothChambers_Data$state_short[i] <- STRING }

#Create ID
BothChambers_Data <- BothChambers_Data %>%  unite(district_id, state_short, district_number, session_start_year, Chamber, sep = "_", remove = FALSE)

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------ End of SubChapter -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------







#-------------------------------------------------------------------------------------------------------------------
#--------------------------------- Merge Vote Margins (House) with LegislatoR-Data ---------------------------------
#-------------------------------------------------------------------------------------------------------------------

# Merge Vote Margins from House elections with Main-Data set
BothChambers_Data_VoteResults <- left_join(BothChambers_Data, vote_data_house_merge, by = "district_id")
# As already said, there is only voting data for the 50. regular US states not Guam, D.C., Puerto Rico etc.

#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------ End of SubChapter -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------






#For Senate we need a different ID as elections only take place every 6 years: 

BothChambers_Data_VoteResults_Senate <-  BothChambers_Data_VoteResults %>% filter(country == "USA-S") 
#Transform candidate names into uppercase 
BothChambers_Data_VoteResults_Senate$Last_Name <- BothChambers_Data_VoteResults_Senate$name
for (i in 1:621) {
  string <- BothChambers_Data_VoteResults_Senate$Last_Name[i] 
  STRING <- toupper(string)
  BothChambers_Data_VoteResults_Senate$Last_Name[i] <- STRING
}

#Extracting the last name 
BothChambers_Data_VoteResults_Senate$Last_Name <- stringr::str_extract(BothChambers_Data_VoteResults_Senate$Last_Name, "[[:alpha:]]+$")

BothChambers_Data_VoteResults_Senate <- BothChambers_Data_VoteResults_Senate %>%  unite(district_id, state_short, district_number, session_start_year, Chamber, Last_Name, sep = "_", remove = FALSE)


#Now the same for the vote margins of the SENATE races

# Problem: in LegislatoR hat jeder Senator für jede Session eine Row (also für alle 2 Jahre), aber tritt nur alle 6 Jahre zur Wahl an 
# Allerdings findet alle 2 Jahre eine Wahl zum Senat statt wo ein Drittel der Senatoren gewählt wird 
#Solution: New id: "ARKANSAS_2_2013_House_NANCY PELOSI" 


# Getting Vote Margin Data FOR THE SENATE and Merge it with Main-Data Set
X1976_2020_senate <- read_csv("Data on Election Dates and Vote Margins /Vote Margins/Dataverse Harvard1976-2020-senate/1976-2020-senate.csv")
vote_data_senate <- X1976_2020_senate 

# Creating an id to be able to merge both datasets: id of vote margin = State - District which is always NA for the Senate - Election Year + 1
vote_data_senate$session_start <- (vote_data_senate$year + 1) # sessions stars one year after election
vote_data_senate$Chamber <- "Senate"
vote_data_senate$NaS <- "NA"
# Including whether a election was a special election (due to a Senator that ended his term early) into to the district_id
vote_data_senate$Special <- vote_data_senate$special 
vote_data_senate$Special<- stringr::str_replace_all(vote_data_senate$Special, "TRUE", "Special")
vote_data_senate$Special<- stringr::str_replace_all(vote_data_senate$Special, "FALSE", "")

# Creating a column containing the last name of the legislator and use it for matching 
vote_data_senate$Last_Name <- vote_data_senate$candidate  
vote_data_senate$Last_Name  <- stringr::str_extract(vote_data_senate$Last_Name , "[[:alpha:]]+$")


# Creating district_id
vote_data_senate <- vote_data_senate %>%  unite(district_id, state, NaS, session_start, Chamber, Last_Name, Special, sep = "_", remove = FALSE)
vote_data_senate$district_id <- stringr::str_replace_all(vote_data_senate$district_id, "_$", "")


# Filter for the time frame that will be analysed 
vote_data_senate <- vote_data_senate %>%  filter(year >= 2004) %>%  filter(year <= 2016)
# Filter out primarie elections (only keep general elections)
vote_data_senate <- vote_data_senate %>%  filter(stage == "gen") 


#Getting the relative vote margin in vote_data_senate: -> vote_data_senate$vote_maxdiff_relative
vote_data_senate_group <- as.data.table(vote_data_senate)
vote_data_senate_group <- vote_data_senate_group %>% dplyr::select(c(district_id, candidatevotes))
vote_data_senate_group_max <- vote_data_senate_group[vote_data_senate_group[, .I[candidatevotes == max(candidatevotes)], by=district_id]$V1]
names(vote_data_senate_group_max)[names(vote_data_senate_group_max) == "candidatevotes"] <- "candidatesvotes_first_max"
# setting all max candidatevotes to 0
vote_data_senate_group$candidatevotes[vote_data_senate_group[, .I[candidatevotes == max(candidatevotes)], by=district_id]$V1] <- 00
vote_data_senate_group_max_2 <- vote_data_senate_group[vote_data_senate_group[, .I[candidatevotes == max(candidatevotes)], by=district_id]$V1]
names(vote_data_senate_group_max_2)[names(vote_data_senate_group_max_2) == "candidatevotes"] <- "candidatesvotes_second_max"
# Merged the two columns contain the highest and second highest vote result
vote_data_senate_group_max_1_2 <- merge(vote_data_senate_group_max, vote_data_senate_group_max_2, by = "district_id")
vote_data_senate_group_max_1_2 <- as.data.frame(vote_data_senate_group_max_1_2)
#Merge max vote data columns into vote_data_senate dataset
vote_data_senate <- merge(vote_data_senate , vote_data_senate_group_max_1_2, by = "district_id")
# Creating Cloumn with total difference of votes between winner and second best
vote_data_senate$vote_maxdiff_absolute_Senate <- (vote_data_senate$candidatesvotes_first_max - vote_data_senate$candidatesvotes_second_max)
# Creating Cloumn with difference of votes between winner and second best relative to total votes 
vote_data_senate$vote_maxdiff_relative_Senate  <- (vote_data_senate$vote_maxdiff_absolute_Senate  / vote_data_senate$totalvotes)
# Filter out special elections (are needed later to add them manually to main dataset)
vote_data_senate_special <- vote_data_senate %>% filter(special == TRUE)
# Prepare for mergin with main data
vote_data_senate_merge <- vote_data_senate %>% dplyr::select(c(district_id, vote_maxdiff_relative_Senate , vote_maxdiff_absolute_Senate ))
vote_data_senate_merge <- unique(vote_data_senate_merge)

# Before merging with BothChambers_Data, I need to take care of the special elections in the dataset with are marked in the 
# district_id in vote_data_senate_merge but NOT in the district_id of BothChambers_Data
vote_data_senate_special

# Merge Vote Margins from Senate elections with Main-Data set
BothChambers_Data_VoteResults_Both_Chambers <- left_join(BothChambers_Data_VoteResults, vote_data_senate_merge, by = "district_id")






#####--------------------- Testing Zone -------------


# To Do: look at merged Senate data and make a list of last names that do not see any entires
# -> compare those last names with last names in Harvard Dataset and Change last names accordingly in the Harvard set
# create IDs again with new last names and merge again 


# Same but with the Senate-ID based on Last_Names
BothChambers_Data_VoteResults_Both_Chambers_New_ID <- left_join(BothChambers_Data_VoteResults_Senate, vote_data_senate_merge, by = "district_id")


length(unique(BothChambers_Data_VoteResults_Both_Chambers_New_ID$Last_Name)) # 174 Senators included 

Table_of_Senators_Where_Matching_Worked  <- BothChambers_Data_VoteResults_Both_Chambers_New_ID %>% filter(vote_maxdiff_absolute_Senate >= 0)
length(unique(Table_of_Senators_Where_Matching_Worked$Last_Name)) #worked for 139 Senators -> 35 missing 


Table_of_all_Senators <- as.data.frame(unique(BothChambers_Data_VoteResults_Both_Chambers_New_ID$Last_Name)) 
names(Table_of_all_Senators )[names(Table_of_all_Senators) == "unique(BothChambers_Data_VoteResults_Both_Chambers_New_ID$Last_Name)"] <- "Name"


List_of_Senators_Where_Matching_Worked <- unique(Table_of_Senators_Where_Matching_Worked$Last_Name) #worked for 139 Senators -> 35 missing 


Table_of_all_Senators$Matching_Worked <- 0 

for (i in 1:174) {
  string <- Table_of_all_Senators$Name[i] 
  bool <- (string %in% List_of_Senators_Where_Matching_Worked)
  Table_of_all_Senators$Matching_Worked[i] <- bool
}

# show me Senator where matching did not work: 
Check_Last_Name <- Table_of_all_Senators %>% filter(Matching_Worked == 0)







# Check whether dimensions are chaning in a predicted way 
dim(BothChambers_Data)
dim(BothChambers_Data_VoteResults) # adds 2 columns, no changes in rows 
dim(BothChambers_Data_VoteResults_Both_Chambers)  # adds 2 columns, no changes in rows 












# Dropping duplicates (at this moment there is a row for each sessions a MoCs has served)
# Goal: having a dataset that contains each MoCs just once 


length(unique(BothChambers_Data$pageid)) # 981

BothChambers_Data_Unique <- unique(BothChambers_Data)
# Some MoCs seem to have why did this not work? because some MoCs were running in different districts at different times (and won them)
# Some MoCs seem to have served more than 6 sessions out of 6 sessinos: -> that's because they served in both House and Senate 
# e.g. Bob_Menendez, Ed_Markey, Roger_Wicker, Mark_Kirk 

Bob_Menendez -> Senate (all terms)
Ed_Markey ->  House
Roger_Wicker -> Senate (all termns)
Mark_Kirk -> Senate (all terms)

What do I need? time served of the observed time frame -> use service 
  

# Include overall edtis over time frame (edits not done from within Congress) based on LegislatoR -> History 





#------------------------- Ideenspeicher -----------------------




#################################
Names_Legislator <- as.data.frame(unique(BothChambers_Data_VoteResults_Both_Chambers_New_ID$Last_Name))
Names_Legislator_List <- unique(BothChambers_Data_VoteResults_Both_Chambers_New_ID$Last_Name)
Names_Havard <- as.data.frame(unique(vote_data_senate$Last_Name))
Names_Havard_List <- unique(vote_data_senate$Last_Name)
vote_data_senate$Last_Name <- stringr::str_replace_all(vote_data_senate$Last_Name, "_$", "")
Names_Legislator$Last_Name_Bool <- 0
for (i in 1:174) {
  Names_Legislator$Last_Name_Bool[i] <-  isTRUE(Names_Legislator$unique(vote_data_senate$Last_Name)[i] %in% Names_Havard_List)
}

