#10_analysis_distribution_of_edits

# NEED A BIG OVERHAUL! 

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


