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
#------------- House: Merging LegislatoR-Data, Filtering for 109th-114th Sessions, Creating Dummies--------------------------
#-------------------------------------------------------------------------------------------------------------------

#Data used: 
load("~/Documents/GitHub/LegislatoR_Data/house_core_LegislatoR.Rdata")
load("~/Documents/GitHub/LegislatoR_Data/house_history_LegislatoR.Rdata")
load("~/Documents/GitHub/LegislatoR_Data/house_political_LegislatoR.Rdata")
load("~/Documents/GitHub/LegislatoR_Data/senate_core_LegislatoR.Rdata")
load("~/Documents/GitHub/LegislatoR_Data/senate_history_LegislatoR.Rdata")
load("~/Documents/GitHub/LegislatoR_Data/senate_political_LegislatoR.Rdata")



#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
Merged_House_Data <- merge(house_us_core, house_us_political, by="pageid")

# Filter for Sessions we want to observe: 109th - 114th Session
House_Session_109_to_114 <- Merged_House_Data %>%  filter(session <= 114) %>%  filter(session >= 109)

# Only keep the first 17 variables (other not needed)
House_Session_109_to_114_Short <- House_Session_109_to_114  %>% dplyr::select(1:18)

# Getting rid of 2 more variables that are not needed 
House_Session_109_to_114_Short <- House_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, deathplace))

#Adding Chamber Column (House or Senate) before merging
House_Session_109_to_114_Short$Chamber <- "House"

House_Data <- House_Session_109_to_114_Short


# Creating variable that indicates how many days a MoCs has served during the 6 sessions observed 
# variable "service" indicats the period of service in days during the respective session of a legislator
DaysService_Count <- House_Data
DaysService_Count =  DaysService_Count %>% dplyr::select(c(pageid, service))
DaysService_Count  <- aggregate(DaysService_Count$service, by=list(pageid= DaysService_Count$pageid), FUN=sum)
names(DaysService_Count )[names(DaysService_Count) == "x"] <- "Days_Served"
#Merge with Main Data Set
House_Data <- merge(House_Data, DaysService_Count, by = "pageid")

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
House_Data$session_start_year <- stringr::str_extract(House_Data$session_start, "\\d+") 
# Creating variable state_short and district_number 
House_Data$state_short <- House_Data$constituency
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "congressional district", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "at-large", "0")
House_Data$district_number  <- stringr::str_extract(House_Data$state_short, "\\d+") 
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "\\d+nd", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "\\d+st", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "\\d+th", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "\\d+rd", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "'s", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "\\d+", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, " $", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, " $", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, " $", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "^ ", "")
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, "^ ", "")

#Transform state names into uppercase 
for (i in 1:2698) {
  string <- House_Data$state_short[i] 
  STRING <- toupper(string)
  House_Data$state_short[i] <- STRING }


#Create ID
House_Data <- House_Data %>%  unite(district_id, state_short, district_number, session_start_year, Chamber, sep = "_", remove = FALSE)

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------ End of SubChapter -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------






#-------------------------------------------------------------------------------------------------------------------
#--------------------------------- Merge Vote Margins (House) with LegislatoR-Data ---------------------------------
#-------------------------------------------------------------------------------------------------------------------

# Merge Vote Margins from House elections with Main-Data set
House_Data_VoteResults <- left_join(House_Data, vote_data_house_merge, by = "district_id")
# As already said, there is only voting data for the 50. regular US states not Guam, D.C., Puerto Rico etc.

#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------ End of SubChapter -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#--------------------------- Create Mean of Vote Margin for House Dataset ------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

#Deleting unused columns
House_Data_MoCs <- House_Data_VoteResults %>%  
  dplyr::select(-c(session, session_start ,session_end  ,service, district_id, district_number, session_start_year, constituency))


# Prepare for mergin with LegislatoR Senate Dataset
House_Data_VoteMargin_Means <- House_Data_MoCs %>% dplyr::select(c(pageid, vote_maxdiff_relative , vote_maxdiff_absolute ))

# Take the mean of vote margins for each MoC
House_Data_VoteMargin_Means  <- aggregate(House_Data_VoteMargin_Means[, 2:3], list(House_Data_VoteMargin_Means$pageid), mean)
names(House_Data_VoteMargin_Means)[names(House_Data_VoteMargin_Means) == "Group.1"] <- "pageid"
names(House_Data_VoteMargin_Means)[names(House_Data_VoteMargin_Means) == "vote_maxdiff_relative"] <- "Mean_vote_maxdiff_relative"
names(House_Data_VoteMargin_Means)[names(House_Data_VoteMargin_Means) == "vote_maxdiff_absolute"] <- "Mean_vote_maxdiff_absolute"

# Mergin Mean Votemargins with House DataSet
House_Data_MoCs  <- left_join(House_Data_MoCs, House_Data_VoteMargin_Means, by = "pageid")

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#--------------------------- Reduce House Dataset to one row per MoC  ---------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

length(unique(House_Data_MoCs$pageid)) #830

House_Data_MoCs <- House_Data_MoCs %>% dplyr::select(-c(vote_maxdiff_absolute, vote_maxdiff_relative))

# Some pageid can be found twice, the reason if often a change of party
House_Data_MoCs$party[House_Data_MoCs$pageid== "11313563"] <- "PNP/D"
House_Data_MoCs$party[House_Data_MoCs$pageid== "20224544"] <- "D/Independent"
House_Data_MoCs$party[House_Data_MoCs$pageid== "2216593"] <- "D/DFL"
House_Data_MoCs$state_short[House_Data_MoCs$pageid== "309634"] <- "VIRGIN ISLANDS"
House_Data_MoCs$party[House_Data_MoCs$pageid== "404905"] <- "D/DFL"
House_Data_MoCs$party[House_Data_MoCs$pageid== "493581"] <- "D/DFL"
House_Data_MoCs$party[House_Data_MoCs$pageid== "493616"] <- "D/DFL"
House_Data_MoCs$party[House_Data_MoCs$pageid== "5437748"] <- "D/DFL"

House_Data_MoCs <- unique(House_Data_MoCs)

# Analytics 
#Which_PageID_Are_Double <- as.data.frame(table(House_Data_MoCs$pageid))
#length(unique(House_Data_MoCs$name)) #828 -> there are 2 pairs of MoCs that have the same name (however pageid and wikititle are distinct)
#length(unique(House_Data_MoCs$wikititle)) #830

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#------------- Senate: Merging LegislatoR-Data, Filtering for 109th-114th Sessions, Creating Dummies--------------------------
#-------------------------------------------------------------------------------------------------------------------

#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
Merged_Senate_Data <- merge(senate_us_core, senate_us_political, by="pageid")

# Filter for Sessions we want to observe: 109th - 114th Session
Senate_Session_109_to_114 <- Merged_Senate_Data %>%  filter(session <= 114) %>%  filter(session >= 109)

# Only keep the first 17 variables (other not needed)
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114 %>% dplyr::select(1:18)

# Getting rid of 2 more variables that are not needed 
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, deathplace))

#Adding Chamber Column (House or Senate) before merging
Senate_Session_109_to_114_Short$Chamber <- "Senate"

Senate_Data <- Senate_Session_109_to_114_Short



# Creating variable that indicates how many days a MoCs has served during the 6 sessions observed 
# variable "service" indicats the period of service in days during the respective session of a legislator
DaysService_Count <- Senate_Data
DaysService_Count =  DaysService_Count %>% dplyr::select(c(pageid, service))
DaysService_Count  <- aggregate(DaysService_Count$service, by=list(pageid= DaysService_Count$pageid), FUN=sum)
names(DaysService_Count )[names(DaysService_Count) == "x"] <- "Days_Served"
#Merge with Main Data Set
Senate_Data <- merge(Senate_Data, DaysService_Count, by = "pageid")


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#----------------------- Voting Data - Senate: Creating Vote Margin Variable and Merging-ID -------------------------
#-------------------------------------------------------------------------------------------------------------------


X1976_2020_senate <- read_csv("Data on Election Dates and Vote Margins /Vote Margins/Dataverse Harvard1976-2020-senate/1976-2020-senate.csv")
vote_data_senate <- X1976_2020_senate 

# Choose time frame
vote_data_senate <- vote_data_senate %>%  filter(year >= 2004) %>%  filter(year <= 2016)
# Keep only general elections
vote_data_senate <- vote_data_senate %>%  filter(stage == "gen") 


#Create ID: state-year-special 
    #Including marker for special elections (due to a Senator that ended his term early) 
vote_data_senate$Special <- vote_data_senate$special 
vote_data_senate$Special<- stringr::str_replace_all(vote_data_senate$Special, "TRUE", "Special")
vote_data_senate$Special<- stringr::str_replace_all(vote_data_senate$Special, "FALSE", "")
    #Creating Election_id
vote_data_senate <- vote_data_senate %>%  unite(Election_id, state, year, Special, sep = "_", remove = FALSE)
    #Cleaning Election_id
vote_data_senate$Election_id <- stringr::str_replace_all(vote_data_senate$Election_id, "_$", "")


#Getting the relative vote margin in vote_data_senate: -> vote_data_senate$vote_maxdiff_relative
vote_data_senate_group <- as.data.table(vote_data_senate)
vote_data_senate_group <- vote_data_senate_group %>% dplyr::select(c(Election_id, candidatevotes))
vote_data_senate_group_max <- vote_data_senate_group[vote_data_senate_group[, .I[candidatevotes == max(candidatevotes)], by=Election_id]$V1]
names(vote_data_senate_group_max)[names(vote_data_senate_group_max) == "candidatevotes"] <- "candidatesvotes_first_max"

# setting all max candidatevotes to 0
vote_data_senate_group$candidatevotes[vote_data_senate_group[, .I[candidatevotes == max(candidatevotes)], by=Election_id]$V1] <- 0
vote_data_senate_group_max_2 <- vote_data_senate_group[vote_data_senate_group[, .I[candidatevotes == max(candidatevotes)], by=Election_id]$V1]
names(vote_data_senate_group_max_2)[names(vote_data_senate_group_max_2) == "candidatevotes"] <- "candidatesvotes_second_max"

# Merged the two columns contain the highest and second highest vote result
vote_data_senate_group_max_1_2 <- merge(vote_data_senate_group_max, vote_data_senate_group_max_2, by = "Election_id")
vote_data_senate_group_max_1_2 <- as.data.frame(vote_data_senate_group_max_1_2)

#Merge max vote data columns into vote_data_senate dataset
vote_data_senate <- merge(vote_data_senate , vote_data_senate_group_max_1_2, by = "Election_id")

# Creating Cloumn with total difference of votes between winner and second best
vote_data_senate$vote_maxdiff_absolute_Senate <- (vote_data_senate$candidatesvotes_first_max - vote_data_senate$candidatesvotes_second_max)

# Creating Cloumn with difference of votes between winner and second best relative to total votes 
vote_data_senate$vote_maxdiff_relative_Senate  <- (vote_data_senate$vote_maxdiff_absolute_Senate  / vote_data_senate$totalvotes)

# Filter out special elections (are needed later to add them manually to main dataset)
vote_data_senate_special <- vote_data_senate %>% filter(special == TRUE)


# Prepare for mergin with LegislatoR Senate Dataset
vote_data_senate_merge <- vote_data_senate %>% dplyr::select(c(Election_id, state, vote_maxdiff_relative_Senate , vote_maxdiff_absolute_Senate ))
vote_data_senate_merge <- unique(vote_data_senate_merge)
    # Take the mean for each of the 50 States
Senate_MeanVoteMargins_by_State <- aggregate(vote_data_senate_merge[, 3:4], list(vote_data_senate_merge$state), mean)


# Merge mean vote margins dataset with LegislatoR Senate Data
    #Create "STATE" cloumn for merging 
names(Senate_MeanVoteMargins_by_State)[names(Senate_MeanVoteMargins_by_State) == "Group.1"] <- "STATE"
    #Transform constituency cloumn into uppercase STATE-cloumn
for (i in 1:621) {
  string <- Senate_Data$constituency[i] 
  STRING <- toupper(string)
  Senate_Data$STATE[i] <- STRING }
    #The MERGE
Senate_Data_VoteResults  <- left_join(Senate_Data, Senate_MeanVoteMargins_by_State, by = "STATE")

# For the Senate not individual vote results of the Senorts in their own races are used for measuring the competitiveness, instead
# the mean of all elections in the Senator's state is taken and used for competitiveness 
# The real reason is: it was impossible to compute otherwise
# The pled reason is: Senate elections are so seldom (only 6 years) that for some Senators only one result might apply
# to get a more solid number, more elections are taken into account 


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#--------------------------- Reduce Senate Dataset to one Row per MoC  ---------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

#Deleting unused columns
Senate_Data_VoteResults <- Senate_Data_VoteResults %>%  dplyr::select(-c(session, session_start ,session_end  ,service))

length(unique(Senate_Data_VoteResults$pageid)) #180
length(unique(Senate_Data_VoteResults$name)) #180

# Dealing with Joe Liebermann who went from democratic to independent
Senate_Data_VoteResults$party[Senate_Data_VoteResults$name== "Joe Lieberman"] <- "D_Independent"

Senate_Data_MoCs <- unique(Senate_Data_VoteResults)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------
#--------------------------------- Merging House and Senate MoC Data -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

colnames(Senate_Data_MoCs)
colnames(House_Data_MoCs)

#House
names(House_Data_MoCs)[names(House_Data_MoCs) == "state_short"] <- "STATE"

#Senate
names(Senate_Data_MoCs)[names(Senate_Data_MoCs) == "vote_maxdiff_relative_Senate"] <- "Mean_vote_maxdiff_relative"
names(Senate_Data_MoCs)[names(Senate_Data_MoCs) == "vote_maxdiff_absolute_Senate"] <- "Mean_vote_maxdiff_absolute"
Senate_Data_MoCs <- Senate_Data_MoCs %>% dplyr::select(-c(constituency))

BothChambers_Data_MoCs <- rbind(House_Data_MoCs, Senate_Data_MoCs)

#Analytics
dim(Senate_Data_MoCs)
dim(House_Data_MoCs)
dim(BothChambers_Data_MoCs)


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- Creaing Year-of-Birth Column -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

BothChambers_Data_MoCs$YearBirth <- stringr::str_extract(BothChambers_Data_MoCs$birth , "^\\d{4}") 
House_Data$state_short <- stringr::str_replace_all(House_Data$state_short, " ", "")
BothChambers_Data_MoCs$YearBirth <- as.double(BothChambers_Data_MoCs$YearBirth)



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- Creating Dummies -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

#Dummies:
# Create Ethnicity Dummies
BothChambers_Data_MoCs <- BothChambers_Data_MoCs  %>% fastDummies::dummy_cols(select_columns = "ethnicity")
# Creating Chamber Dummies
BothChambers_Data_MoCs  <- BothChambers_Data_MoCs  %>% fastDummies::dummy_cols(select_columns = "Chamber")
# Create Sex Dummy
BothChambers_Data_MoCs <- BothChambers_Data_MoCs  %>% fastDummies::dummy_cols(select_columns = "sex",  remove_first_dummy = TRUE)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#---------------- Including Total Number of Edits (not only from Congress) in Time Frame ---------------------------
#-------------------------------------------------------------------------------------------------------------------

# Extracting year of edit
house_us_history$year <- stringr::str_extract(house_us_history$timestamp , "^\\d{4}") 
house_us_history$year <- stringr::str_replace_all(house_us_history$year, " ", "")
house_us_history$year<- as.double(house_us_history$year)

house_us_history_short <- house_us_history  %>%  filter(year >= 2005) %>%  filter(year <= 2016)
house_us_history_short$count <- 1
house_us_history_short <- house_us_history_short %>%  unite(pageid_year, pageid, year, sep = "_", remove = FALSE)

Sum_Edits_Per_MoC <- aggregate(house_us_history_short[, 11], list(house_us_history_short$pageid_year), sum)
names(Sum_Edits_Per_MoC)[names(Sum_Edits_Per_MoC) == "Group.1"] <- "pageid_year"
names(Sum_Edits_Per_MoC)[names(Sum_Edits_Per_MoC) == "x"] <- "All_Edits_perYear_MoC"

Sum_Edits_Per_MoC$pageid <- stringr::str_extract(Sum_Edits_Per_MoC$pageid_year, "^\\d+(?=[[:punct:]])") 

Sum_Edits_Per_MoC_Total <- aggregate(Sum_Edits_Per_MoC[, 2], list(Sum_Edits_Per_MoC$pageid), sum)
names(Sum_Edits_Per_MoC_Total)[names(Sum_Edits_Per_MoC_Total) == "Group.1"] <- "pageid"
names(Sum_Edits_Per_MoC_Total)[names(Sum_Edits_Per_MoC_Total) == "x"] <- "All_Edits_2004to16_MoC"

House_All_Edits_Total_perYear_byMoC <- left_join(Sum_Edits_Per_MoC, Sum_Edits_Per_MoC_Total, by = "pageid")

House_2004to2016_Edits_MoC <- House_All_Edits_Total_perYear_byMoC %>% dplyr::select(c(pageid,All_Edits_2004to16_MoC))
House_2004to2016_Edits_MoC <- unique(House_2004to2016_Edits_MoC)

#Merge with LegislatoR-Data 
BothChambers_Data_MoCs <-  left_join(BothChambers_Data_MoCs , House_2004to2016_Edits_MoC , by = "pageid")

length(unique(BothChambers_Data_MoCs$wikititle)) #all MoC that appear twice have been in Senate and House


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------
#-------------------------- Same shit but this time only for the years an MOC has severd ---------------------------
#-------------------------------------------------------------------------------------------------------------------

#House
Merged_House_Data <- merge(house_us_core, house_us_political, by="pageid")
House_Session_109_to_114 <- Merged_House_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
House_Sessions_MoCs <- House_Session_109_to_114 %>%  dplyr::select(c(pageid, session))
House_Sessions_MoCs <- House_Sessions_MoCs %>%  unite(Pageid_Session, pageid, session, sep = "_", remove = FALSE)

#Senate
Merged_Senate_Data <- merge(senate_us_core, senate_us_political, by="pageid")
Senate_Session_109_to_114 <- Merged_Senate_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
Senate_Sessions_MoCs <- Senate_Session_109_to_114 %>%  dplyr::select(c(pageid, session))
Senate_Sessions_MoCs <- Senate_Sessions_MoCs %>%  unite(Pageid_Session, pageid, session, sep = "_", remove = FALSE)


# Continue Aggreate Edits based former partly aggregated data (i.e. Sum_Edits_Per_MoC)
Sum_Edits_Per_MoC$Pageid_Session <- Sum_Edits_Per_MoC$pageid_year

Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2005", "109")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2006", "109")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2007", "110")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2008", "110")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2009", "111")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2010", "111")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2011", "112")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2012", "112")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2013", "113")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2014", "113")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2015", "114")
Sum_Edits_Per_MoC$Pageid_Session <- stringr::str_replace_all(Sum_Edits_Per_MoC$Pageid_Session, "2016", "114")


Edits_Per_Sessions_MoC <- aggregate(Sum_Edits_Per_MoC[, 2], list(Sum_Edits_Per_MoC$Pageid_Session), sum)
names(Edits_Per_Sessions_MoC)[names(Edits_Per_Sessions_MoC) == "Group.1"] <- "Pageid_Session"
names(Edits_Per_Sessions_MoC)[names(Edits_Per_Sessions_MoC) == "x"] <- "Overall_Edits"

# Join edit count data with the MoCs and their Sessions they have served 
Total_Edits_by_Session_House  <-  left_join(House_Sessions_MoCs, Edits_Per_Sessions_MoC  , by = "Pageid_Session")
Total_Edits_by_Session_Senate  <-  left_join(Senate_Sessions_MoCs, Edits_Per_Sessions_MoC  , by = "Pageid_Session")

# Summing up by pageid for House
Total_Edits_by_Tenure_House <-  aggregate(Total_Edits_by_Session_House[, 4], list(Total_Edits_by_Session_House$pageid), sum)
names(Total_Edits_by_Tenure_House)[names(Total_Edits_by_Tenure_House) == "Group.1"] <- "pageid"
names(Total_Edits_by_Tenure_House)[names(Total_Edits_by_Tenure_House) == "x"] <- "Overall_Edits_During_Tenure"

# Summing up by pageid for Senate
Total_Edits_by_Tenure_Senate <-  aggregate(Total_Edits_by_Session_Senate[, 4], list(Total_Edits_by_Session_Senate$pageid), sum)
names(Total_Edits_by_Tenure_Senate)[names(Total_Edits_by_Tenure_Senate) == "Group.1"] <- "pageid"
names(Total_Edits_by_Tenure_Senate)[names(Total_Edits_by_Tenure_Senate) == "x"] <- "Overall_Edits_During_Tenure"


# Merge edits-counts with Main Dataset (LegislatoR)
# But: some MoCs are twice in the data, as they served both Senate and House in the observed timeframe 
# Therefore we need a Chamber-sensitive way to join the data

# Creating pageid_chamber for merging 
Total_Edits_by_Tenure_Senate$chamber <- "Senate"
Total_Edits_by_Tenure_Senate <- Total_Edits_by_Tenure_Senate %>%  unite(pageid_chamber, pageid, chamber, sep = "_", remove = FALSE)
Total_Edits_by_Tenure_Senate <- Total_Edits_by_Tenure_Senate %>% dplyr::select(-c(chamber))

Total_Edits_by_Tenure_House$chamber <- "House"
Total_Edits_by_Tenure_House <- Total_Edits_by_Tenure_House %>%  unite(pageid_chamber, pageid, chamber, sep = "_", remove = FALSE)
Total_Edits_by_Tenure_House <- Total_Edits_by_Tenure_House %>% dplyr::select(-c(chamber))

BothChambers_Data_MoCs <- BothChambers_Data_MoCs  %>%  unite(pageid_chamber, pageid, Chamber, sep = "_", remove = FALSE)

Total_Edits_by_Tenure_BothChambers <- rbind(Total_Edits_by_Tenure_House, Total_Edits_by_Tenure_Senate)

#Set NAs to Zero 
#sum(is.na(Total_Edits_by_Tenure_BothChambers$Overall_Edits_During_Tenure))
Total_Edits_by_Tenure_BothChambers$Overall_Edits_During_Tenure[is.na(Total_Edits_by_Tenure_BothChambers$Overall_Edits_During_Tenure)] <- 0
Total_Edits_by_Tenure_BothChambers <- Total_Edits_by_Tenure_BothChambers %>% dplyr::select(-c(pageid))

BothChambers_Data_MoCs <-  left_join(BothChambers_Data_MoCs, Total_Edits_by_Tenure_BothChambers , by = "pageid_chamber")

# OUT:  Saving Dataframe BothChambers_Data_MoCs
save(BothChambers_Data_MoCs , file = "BothChambers_Data_MoCs.Rdata")


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




















#------------------------- Ideenspeicher -----------------------


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

