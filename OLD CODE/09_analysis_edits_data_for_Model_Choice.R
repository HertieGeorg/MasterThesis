#08_analysis_edits_data_for_Model_Choice


# How many MoCs do not see edits of their profiles coming from within Congress in the observed time frame? 

# Get main_dataframe_dates.Rdata
load("~/Documents/GitHub/MasterThesis/main_dataframe_dates.Rdata")


#Time frame of edits: 2004-07-16 to 2017-12-07
# but only 7 observations in 2004
# the 109th United States Congress startet at January 4, 2005 
# the 114th Congress ended in January 3, 2017
# ca. 150 obervations after January 3, 2017

#Conclusion: 
#This analysis is looking at edits made on profiles of MoCs serving in Sessions 109 to 114 during those sessions (i.e. January 4, 2005 until January 3, 2017)

# Subsetting main_dataframe_dates into the defined time frame 

main_dataframe_core <- main_dataframe_dates %>% filter(date_LegislatoR <= "2017-01-03")
main_dataframe_core <- main_dataframe_core %>% filter(date_LegislatoR >= "2005-01-04")


length(unique(main_dataframe_core$wikititle)) 
# 638 different MoCs see edits in the observered time frame (January 4, 2005 until January 3, 2017)

# How many MoCs served in this time frame (109th-114th Congress)? 

#Load LegislatoR data
load("~/Documents/GitHub/MasterThesis/LegislatoR_Data/house_political_LegislatoR.Rdata")
load("~/Documents/GitHub/MasterThesis/LegislatoR_Data/house_core_LegislatoR.Rdata")
load("~/Documents/GitHub/MasterThesis/LegislatoR_Data/senate_core_LegislatoR.Rdata")
load("~/Documents/GitHub/MasterThesis/LegislatoR_Data/senate_political_LegislatoR.Rdata")

#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
# 1. 
Merged_House_Data <- merge(house_us_core, house_us_political, by="pageid")
# 2. 
Merged_Senate_Data <- merge(senate_us_core, senate_us_political, by="pageid")

# Filter for Sessions we want to observe 109 - 114
#House
House_Session_109_to_114 <- Merged_House_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
#Senate
Senate_Session_109_to_114 <- Merged_Senate_Data %>%  filter(session <= 114) %>%  filter(session >= 109)

# List and Count unique MoCs in observed time frame 

List_of_MoH_109th_to_114th <- unique(House_Session_109_to_114$name)
length(List_of_MoH_109th_to_114th) #828

List_of_MoS_109th_to_114th <- unique(Senate_Session_109_to_114$name)
length(List_of_MoS_109th_to_114th) #180

# Combing House and Senate, in total 828 + 180 = 1008 MoCs served in the observed time frame 
# In our dataset, however, we see only 638 MoCs that see edits to their profiles in the observed time frame 
# This gives a ratio of 638/1008 = 63.3% of MoCs affected 
# This means on the other side that 36.7% of MoCs must be counted as zeros
# The question is now, is this an exessive number of zeros? 


# Checking whether data is skewed: visualized probability distribution of numbers of edits seen by each MoC

edit_distribution <- main_dataframe_core %>% select( c(pageid))
edit_distribution$count = 1

edit_distribution <- aggregate(edit_distribution$count, by=list(pageid=edit_distribution$pageid), FUN=sum)
  
# Add entry for number of MoCs that see 0 edits of their profiles
df<-data.frame(0,370)
names(df)<-c("pageid","x")

for (i in 1:370) {
    df[i,1] <- "MoC_Without_Edits"
    df[i,2] <-  0
    }

# Combine both dataframes
edit_distribution <- rbind(edit_distribution, df)


# Visualize Distribution 
ggplot(edit_distribution, aes(x = x)) +
  geom_histogram(bins = 30) +
  xlab("Number of Edits per Profile") 


# Visualize Distribution without outliers

edit_distribution_without_outliers <- edit_distribution %>%  filter(x < 20)
#this leaves out 10 observations out of 943

ggplot(edit_distribution_without_outliers, aes(x = x)) +
  geom_histogram(bins = 20) +
  xlab("Number of Edits per Profile") 

# distributions is highly skewed 



#Sorting MoCs into lists based on whether their profiles see edits or not 

MoCs_with_Edits <- unique(main_dataframe_core$Name_clean)

#Preparation for MoCs_withOUT_Edits
House_Session_109_to_114$chamber <- "House"
House_Session_109_to_114 <- House_Session_109_to_114 %>% select(c(name, chamber))
MoCs_House_Session_109_to_114 <- unique(House_Session_109_to_114)

Senate_Session_109_to_114$chamber <- "Senate"
Senate_Session_109_to_114 <- Senate_Session_109_to_114 %>% select(c(name, chamber))
MoCs_Senate_Session_109_to_114 <- unique(Senate_Session_109_to_114)

MoCs_BothChambers_109_114 <- rbind(MoCs_Senate_Session_109_to_114 , MoCs_House_Session_109_to_114)

# MoCs_withOUT_Edits:
`%!in%` = Negate(`%in%`) #create own function real quick 
MoCs_withOUT_Edits <- MoCs_BothChambers_109_114 %>% filter(name %!in% MoCs_with_Edits)



# Which MoCs are actually included into the edits dataset? Did they really all serve in the 109-114th Congress? 

# First: merge House and Senate data from Legislator Into one dataframe
Merged_House_Data_Short <- Merged_House_Data %>% select(1:18)
Merged_Senate_Data_Short <- Merged_Senate_Data %>% select(1:18)
Merged_BothChambers_Data_Short <- rbind(Merged_Senate_Data_Short , Merged_House_Data_Short )

# Second: merged edit data and LegislatoR into one dataframe (based on edit data)

MoCs_with_Edits_Pageid <- unique(main_dataframe_core$pageid)

LegislatoR_BothChambers_MoCs_With_Edits <- Merged_BothChambers_Data_Short %>% filter(pageid %in% MoCs_with_Edits_Pageid)
#unique(LegislatoR_BothChambers_MoCs_With_Edits$pageid)

length(unique(LegislatoR_BothChambers_MoCs_With_Edits$session))

ggplot(LegislatoR_BothChambers_MoCs_With_Edits, aes(x = session)) +
  geom_histogram(bins = 34)


looking_for_right_sessions <- LegislatoR_BothChambers_MoCs_With_Edits %>% filter(session <= 114) %>% filter(session >= 109)
#out of the 637 Mocs that have edits, 547 have served in Sessions 109-114 (but only 573 have served in sessions 80 to 120)

length(unique(looking_for_right_sessions$pageid))

# So what? -> only include edits that were done during the 114 and 109 Congress on profiles of MoCs who served during that time


