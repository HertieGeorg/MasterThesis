#10_Counting_Congress_Edits

#in:
# BothChambers_Data_MoCs.Rdata 
# main_dataframe_dates.Rdata


#-------------------------------------------------------------------------------------------------------------------
#----------- Subsetting datasets with Congress edits to the observed timeframe (109th-114th session)----------------
#-------------------------------------------------------------------------------------------------------------------


# Subsetting main_dataframe_dates into the defined time frame (Sessions 109-114)
main_dataframe_core <- main_dataframe_dates %>% filter(date_LegislatoR <= "2017-01-03")  %>% filter(date_LegislatoR >= "2005-01-04")

# 638 different MoCs see edits in the observered time frame (January 4, 2005 until January 3, 2017)
length(unique(main_dataframe_core$wikititle)) 

# 981 different MoC served in Sessions 109-114 (MoCs that served in both chambers are counted just once)
length(unique(BothChambers_Data_MoCs$wikititle))
# Due to MoCs that served in both chamber, the dataset contains 1010 entries, each entry has a unique pageid_chamber variable 

# Ratio of MoCs that see Wikipedia-Edits coming from within Congress during their tenure: 
# 638/981 = 65%   #This means on the other side that 35% of MoCs must be counted as zeros
# So, is that an exessive number of zeros? 

#Out: main_dataframe_core
# Saving Dataframe 
#save(main_dataframe_core, file = "main_dataframe_core.Rdata")


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------
#------Getting a column that shows average overall edits on Wikipedia profile per day served in Congress  ----------
#-------------------------------------------------------------------------------------------------------------------

Characteristics_Data_MoCs <- BothChambers_Data_MoCs 

# Sum up day_served by MoC (there are still some Legislators twice in dataset as they served in both chambers)
Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Days_Served))
# Summing up by pageid for Senate
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- "Day_Served_Sum"
Merge_Data_Days_Served <- Merge_Data

# Sum up Overall_Edits_During_Tenure by MoC (there are still some Legislators twice in dataset as they served in both chambers)
Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Overall_Edits_During_Tenure))
# Summing up by pageid for Senate
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- "Overall_Edits_During_Tenure_Sum"
Merge_Data_Overall_Edits_During_Tenure <- Merge_Data

# Join all summed up values into one dataframe: Merge_Data_1_2_3 
Merge_Data_1_2_3<- left_join(Merge_Data_Days_Served, Merge_Data_Overall_Edits_During_Tenure , by ="pageid")
#colnames(Merge_Data_1_2_3)

# Get average number of overall edits on profile per day served 
Merge_Data_1_2_3$OverallEdits_inTenure_perDayServed <- Merge_Data_1_2_3$Overall_Edits_During_Tenure_Sum / Merge_Data_1_2_3$Day_Served_Sum

#Merge back with main data set
Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, Merge_Data_1_2_3 , by ="pageid")

length(unique(Characteristics_Data_MoCs $pageid)) #981
length(Characteristics_Data_MoCs $pageid) #1010



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#------Getting rid of the differenciation between the chambers for one MoC that served in both  --------------------
#-------------------------------------------------------------------------------------------------------------------

# This means reducing the length(BothChambers_Data_MoCs) = 1010 to a length(Characteristics_Data_MoCs) = 981 dataset


Characteristics_Data_MoCs$Weigthed_Mean_vote_maxdiff_relative = Characteristics_Data_MoCs$Mean_vote_maxdiff_relative* Characteristics_Data_MoCs$Days_Served
Characteristics_Data_MoCs$Weigthed_Mean_vote_maxdiff_absolute = Characteristics_Data_MoCs$Mean_vote_maxdiff_absolute* Characteristics_Data_MoCs$Days_Served

#Drop columns that are chamber specific: 
Characteristics_Data_MoCs = Characteristics_Data_MoCs %>% dplyr::select(-c("Chamber_House", "Chamber_Senate", "Days_Served", "Mean_vote_maxdiff_relative", "country", "Mean_vote_maxdiff_absolute", "Chamber","pageid_chamber", "religion",  "Overall_Edits_During_Tenure"))



# Get columns for Combined_Mean_vote_maxdiff_relative for each Legislator (getting rid of 2 values for different chambers)
DaysServedSum <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Day_Served_Sum)) #needed later
Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Weigthed_Mean_vote_maxdiff_relative))
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- "Combined_Weigthed_Mean_vote_maxdiff_relative"
# Join with DaysServedSum 
Merge_Data <- left_join(Merge_Data, DaysServedSum  , by ="pageid")
Merge_Data <- unique(Merge_Data)
Merge_Data$Combined_Mean_vote_maxdiff_relative <- Merge_Data$Combined_Weigthed_Mean_vote_maxdiff_relative /  Merge_Data$Day_Served_Sum
Merge_DataA <- Merge_Data %>% dplyr::select(-c(Day_Served_Sum, Combined_Weigthed_Mean_vote_maxdiff_relative))


# Same for absolute values: Combined_Mean_vote_maxdiff_absolute for each Legislator (getting rid of 2 values for different chambers)
DaysServedSum <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Day_Served_Sum)) #needed later
Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Weigthed_Mean_vote_maxdiff_absolute))
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- "Combined_Weigthed_Mean_vote_maxdiff_absolute"
# Join with DaysServedSum 
Merge_Data <- left_join(Merge_Data, DaysServedSum  , by ="pageid")
Merge_Data <- unique(Merge_Data)
Merge_Data$Combined_Mean_vote_maxdiff_absolute <- Merge_Data$Combined_Weigthed_Mean_vote_maxdiff_absolute /  Merge_Data$Day_Served_Sum
Merge_DataB <- Merge_Data %>% dplyr::select(-c(Day_Served_Sum, Combined_Weigthed_Mean_vote_maxdiff_absolute))

# Joing both sub-datasets:
Merge_DataAB <- left_join(Merge_DataA, Merge_DataB , by ="pageid")

# Re-Joing with maindata 
Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, Merge_DataAB , by ="pageid")

# Getting Rid of chamber specific columns
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>% dplyr::select(-c(Weigthed_Mean_vote_maxdiff_relative, Weigthed_Mean_vote_maxdiff_absolute))

# Test statistics
length(Characteristics_Data_MoCs) #1010
length(unique(Characteristics_Data_MoCs$pageid)) #981
 
# Delete MoC that appear twice (keep just one entry per MoC)
Characteristics_Data_MoCs <- unique(Characteristics_Data_MoCs) # 986 -> 5 zu viel! 

# Filter out rows that are duplicates and still in dataset: (due to two different versions for the names in former Senate row and House row)
Characteristics_Data_MoCs$pageid[duplicated(Characteristics_Data_MoCs$pageid)]
#Pageids: 361176" "412103" "414532" "417439" "440819"
# 361176
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>%  filter(name != "Bernard Sanders")
"412103"
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>%  filter(name != "Mark Steven Kirk")
"414532"
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>%  filter(name != "Ben Cardin")
"417439" 
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>%  filter(name != "Roger Wicker")
"440819"
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>%  filter(name != "Ed Markey")

dim(Characteristics_Data_MoCs) # 981 -> worked! 

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#-------------------------- Include Edits counts for edits coming from within Congress -----------------------------
#-------------------------------------------------------------------------------------------------------------------


# All edits from within Congress:
Inside_Congress_Edits <- main_dataframe_core
Inside_Congress_Edits$count <- 1
Inside_Congress_Edits <- Inside_Congress_Edits   %>% dplyr::select(c(pageid, count))
# Aggregate 
Inside_Congress_Edits_Aggregate <- aggregate(Inside_Congress_Edits[, 2], list(Inside_Congress_Edits$pageid), sum)
names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "Group.1"] <- "pageid"
names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "count"] <- "AllCongressEdits_Per_MoC"

#Out: Inside_Congress_Edits_Aggregate


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#-------------------------- Include Politically Edits counts for edits coming from within Congress ----------------
#-------------------------------------------------------------------------------------------------------------------


# Politically motivated edits from within Congress: 
#     For better insights the column should be entangled in different columns 

Inside_Congress_Edits_Politically <- main_dataframe_core

# Create Dummy for political motivation of edit and set it to 0 
Inside_Congress_Edits_Politically$politically_motivated <- 0

# Politically Motivated Edits: (see definition in Fließtext Masterabeit)
pattern_1 = "insults_mockery"
pattern_2 = "praise_adulation"

pattern_3 = "rather_beneficial"
pattern_4 = "mostly_beneficial"
pattern_5 = "rather_harmful"
pattern_6 = "mostly_harmful"


# Filetring column Answer.characteristics for insults and praise
for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.characteristics[i], pattern_1, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.characteristics[i], pattern_2, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

colnames(Inside_Congress_Edits_Politically)

# Filtering Column: Answer.content_added
for(i in 1:2601) {                                      
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_added[i], pattern_3, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_added[i], pattern_4, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_added[i], pattern_5, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_added[i], pattern_6, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

# Filtering Column: Answer.content_removed
for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_removed[i], pattern_3, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_removed[i], pattern_4, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_removed[i], pattern_5, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically$Answer.content_removed[i], pattern_6, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically$politically_motivated[i] = 1 
  }
}

table(Inside_Congress_Edits_Politically$politically_motivated) # 0 -> 1140  ,  1 -> 1461 

#Out: Inside_Congress_Edits_Politically -> list of edits from within Congress wiht Dummy for politically motivation for each edit


# Aggregation Politically Edits from within Congress per MoC: 
Inside_Congress_Edits_Politically$count <- 1
Inside_Congress_Edits_Politically1 <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)
Inside_Congress_Edits_Politically1 <- Inside_Congress_Edits_Politically1  %>% dplyr::select(c(pageid, count))

# Aggregate 
Inside_Congress_Edits_Politically1_Aggregate <- aggregate(Inside_Congress_Edits_Politically1[, 2], list(Inside_Congress_Edits_Politically1$pageid), sum)
names(Inside_Congress_Edits_Politically1_Aggregate)[names(Inside_Congress_Edits_Politically1_Aggregate) == "Group.1"] <- "pageid"
names(Inside_Congress_Edits_Politically1_Aggregate)[names(Inside_Congress_Edits_Politically1_Aggregate) == "count"] <- "All_Politically_CongressEdits_Per_MoC"


# Merge Aggregated Edit-Columns back with Characteristics_Data_MoCs

Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, Inside_Congress_Edits_Aggregate , by ="pageid")
Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, Inside_Congress_Edits_Politically1_Aggregate , by ="pageid")

#Out: Characteristics_Data_MoCs 


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------






#-------------------------------------------------------------------------------------------------------------------
#---------------------- Dividing Congress Edits into postivie politically and negative politically  ----------------
#-------------------------------------------------------------------------------------------------------------------

# As some politically motivated edits to a profile are of a destructive and some of a beneficial nature
# We can assume that the destructive ones are made by opponents, while the beneficial ones are done by the MoC or his/her staffers themselves
# Therefore we should have differenct columns to distinguish between those two kinds of edits 

# To Do: if separation is done, produce also descriptive statistics for positive, negative and non-political edits 




# Politically motivated edits from within Congress: 
Inside_Congress_Edits_Politically_PositiveNegative <- main_dataframe_core

# Create Dummy for political motivation of edit and set it to 0 
Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive <- 0
Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative <- 0


# Politically Motivated Edits: (see definition in Fließtext Masterabeit)

pattern_3 = "rather_beneficial"
pattern_4 = "mostly_beneficial"
pattern_5 = "rather_harmful"
pattern_6 = "mostly_harmful"




# Regarding "insults_mockery", in one case a very insulting sentence was removed from a profile by someone within Congress; 
# This is definitley a positive political motivation but still listed under "insults_mockery"
# Therefore all edits under "insults_mockery" cannot be label as political negative motivation 
# if content_added = Most_harmful + contentc removed = Mostybenefical -> overall harmful 
# if content_added = Mosty_beneficial + contentc removed = Mostybenefical -> overall beneficial


#------------- Exploring the data to find out what constitutes a postive or negativ politically motivated edit


# Set dummy for each edit in the columns politically_motivated_positive and politically_motivated_negative

# Filtering Column: Answer.content_added
for(i in 1:2601) {                                      
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_3, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_4, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_5, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_6, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1 
  }
}

# Filtering Column: Answer.content_removed
for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_3, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_4, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_5, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1 
  }
}

for(i in 1:2601) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_6, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1
  }
}

table(Inside_Congress_Edits_Politically$politically_motivated) # 0 -> 1140  ,  1 -> 1461 
table(Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive) 
table(Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative) 

# Check whether some posts are indicated to be both positive and negative politically motivated
Both <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1) %>% filter(politically_motivated_negative == 1) 

# Posts that have a marker for both positive and negatvie are filtered again and insults are marked as negative and praise as positive (rest = neutral)

pattern_10 = "insults_mockery"
pattern_11 = "praise_adulation"

Politically_Both <- Inside_Congress_Edits_Politically_PositiveNegative %>% 
  filter(politically_motivated_positive == 1) %>% 
  filter(politically_motivated_negative == 1) 

#"insults_mockery"
  for(i in 1:137) {
    if (sjmisc::str_contains(Politically_Both$Answer.characteristics[i], pattern_10, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
      Politically_Both$politically_motivated_positive[i] = 0
    }
  } 

#"praise_adulation"
for(i in 1:137) {
  if (sjmisc::str_contains(Politically_Both$Answer.characteristics[i], pattern_11, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Politically_Both$politically_motivated_negative[i] = 0
  }
} 

# neither "praise_adulation" nor "insults_mockery" -> set to 0
for(i in 1:137) {
  if (Politically_Both$politically_motivated_positive[i] == 1 && Politically_Both$politically_motivated_negative[i] == 1) { 
    Politically_Both$politically_motivated_positive[i] = 0
    Politically_Both$politically_motivated_negative[i] = 0
  }
} 


# Changed values of Politically_Both are transfered to main data set (-> Inside_Congress_Edits_Politically_PositiveNegative)
length(Inside_Congress_Edits_Politically_PositiveNegative$pageid)

for(i in 1:2601) {
  for (j in 1:137) {
    if (Inside_Congress_Edits_Politically_PositiveNegative$revid[i] == Politically_Both$revid[j]) { 
      Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = Politically_Both$politically_motivated_negative[j]
      Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = Politically_Both$politically_motivated_positive[j]
    }
  }
} 


#Out: Inside_Congress_Edits_Politically_PositiveNegative -> list of edits from within Congress with Dummy for positive and negative political motivation for each edit


# Aggregation positive Politically Edits from within Congress per MoC: 
Inside_Congress_Edits_Politically_PositiveNegative$count <- 1
Inside_Congress_Edits_Politically_Positive <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1)
Inside_Congress_Edits_Politically_Positive <- Inside_Congress_Edits_Politically_Positive  %>% dplyr::select(c(pageid, count))

# Aggregation negative Politically Edits from within Congress per MoC: 
Inside_Congress_Edits_Politically_Negative <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_negative == 1)
Inside_Congress_Edits_Politically_Negative <- Inside_Congress_Edits_Politically_Negative  %>% dplyr::select(c(pageid, count))


# Aggregate 
Inside_Congress_Edits_Politically_Positive_Aggregate <- aggregate(Inside_Congress_Edits_Politically_Positive[, 2], list(Inside_Congress_Edits_Politically_Positive$pageid), sum)
names(Inside_Congress_Edits_Politically_Positive_Aggregate)[names(Inside_Congress_Edits_Politically_Positive_Aggregate) == "Group.1"] <- "pageid"
names(Inside_Congress_Edits_Politically_Positive_Aggregate)[names(Inside_Congress_Edits_Politically_Positive_Aggregate) == "count"] <- "All_Positive_Politically_CongressEdits_Per_MoC"

# Aggregate 
Inside_Congress_Edits_Politically_Negative_Aggregate <- aggregate(Inside_Congress_Edits_Politically_Negative[, 2], list(Inside_Congress_Edits_Politically_Negative$pageid), sum)
names(Inside_Congress_Edits_Politically_Negative_Aggregate)[names(Inside_Congress_Edits_Politically_Negative_Aggregate) == "Group.1"] <- "pageid"
names(Inside_Congress_Edits_Politically_Negative_Aggregate)[names(Inside_Congress_Edits_Politically_Negative_Aggregate) == "count"] <- "All_Negative_Politically_CongressEdits_Per_MoC"


# Merge Aggregated Edit-Columns back with Characteristics_Data_MoCs

Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, Inside_Congress_Edits_Politically_Negative_Aggregate  , by ="pageid")
Characteristics_Data_MoCs <- left_join(Characteristics_Data_MoCs, Inside_Congress_Edits_Politically_Positive_Aggregate  , by ="pageid")

# For some edits it is clear that there is a political motivation but it is unclear whether this motivation is predominantly positve or negative
# That is why some MoCs see more politically motivated edits in general than the sum of negative and postive political edits 
# Reason: some edits see the removal of positive content and the adding of other positive content at the same time, therefore it is not possible to identify the motivation 
#cont. in a automated manner 


#Out: Characteristics_Data_MoCs
# Saving Dataframe 
#save(Characteristics_Data_MoCs , file = "Characteristics_Data_MoCs.Rdata")










# Ideenspeicher----------------------------------------------------------------------------------------------------

# Code for extracting edits by pageid and year at the same time (wrote it before I decided to merge data of legislators that served in both chambers)

#Inside_Congress_Edits <- main_dataframe_core
#Inside_Congress_Edits$count <- 1
#Inside_Congress_Edits <- Inside_Congress_Edits   %>% dplyr::select(c(pageid, date_LegislatoR, count))
#   Extracting year of edit
#Inside_Congress_Edits$year <- stringr::str_extract(Inside_Congress_Edits$date_LegislatoR , "^\\d{4}") 
#Inside_Congress_Edits$year <- stringr::str_replace_all(Inside_Congress_Edits$year, " ", "")
#Inside_Congress_Edits$year <- as.double(Inside_Congress_Edits$year)
#   Create a pageid_year column to aggretate by pageid and by year the same time
#Inside_Congress_Edits <- Inside_Congress_Edits  %>%  unite(pageid_year, pageid, year, sep = "_", remove = FALSE)
#   Aggregate 
#Inside_Congress_Edits_Aggregate <- aggregate(Inside_Congress_Edits[, 4], list(Inside_Congress_Edits$pageid_year), sum)
#names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "Group.1"] <- "pageid_year"
#names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "count"] <- "CongressEdits_Per_YearMoC"
#   Recreate year column that got lost in aggregation-process
#Inside_Congress_Edits_Aggregate$year <- stringr::str_extract(Inside_Congress_Edits_Aggregate$pageid_year, "[[:punct:]]\\d+$")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "[[:punct:]]", "")
#   Change year column into Sessions column
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2005", "109")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2006", "109")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2007", "110")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2008", "110")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2009", "111")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2010", "111")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2011", "112")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2012", "112")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2013", "113")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2014", "113")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2015", "114")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2016", "114")
#Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2017", "114")
#names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "year"] <- "session"



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




