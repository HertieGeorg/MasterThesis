# 06_Creating_Main_Dataset.R

# This file does the following:
# Creates count variables for edits for each MoC in each Session
# Creates variables: 
#- Edit counts 
#- External edits on Wikipedia profile (per Session)
#- Popularity as Views on Page 
#- Competitivness (per Session)

# This File creates follwing output: 
# 1. Inside_Congress_Edits_Politically_PositiveNegative.Rdata
# 2. Inside_Congress_Edits_Politically.Rdata
# 3. BothChambers_Session_109_to_114_Short.Rdata
# 4. main_dataframe_true.Rdata

# And takes following input: 
# 1. house_us_core, house_us_political, house_us_traffic, house_us_history
# 2. senate_us_core, senate_us_political, senate_us_traffic, senate_us_history
# 3. main_dataframe_dates
# 4. Voting data for Senate and House 



# Getting House MoCs 109th - 114th session
#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
Merged_House_Data <- merge(house_us_core, house_us_political, by="pageid")
# Filter for Sessions we want to observe: 109th - 114th Session
House_Session_109_to_114 <- Merged_House_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
# Only keep the first 17 variables (other not needed)
House_Session_109_to_114_Short <- House_Session_109_to_114  %>% dplyr::select(1:18)
# Getting rid of 2 more variables that are not needed 
House_Session_109_to_114_Short <- House_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, wikidataid, deathplace, ethnicity, religion, death, session_start, session_end))


# Getting Senate MoCs 109th - 114th session
Merged_Senate_Data <- merge(senate_us_core, senate_us_political, by="pageid")
# Filter for Sessions we want to observe: 109th - 114th Session
Senate_Session_109_to_114 <- Merged_Senate_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
# Only keep the first 17 variables (other not needed)
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114  %>% dplyr::select(1:18)
# Getting rid of 2 more variables that are not needed 
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, wikidataid, deathplace, ethnicity, religion, death, session_start, session_end))


# Combine both Chambergs
BothChambers_Session_109_to_114_Short <- rbind(House_Session_109_to_114_Short , Senate_Session_109_to_114_Short)
# Create birthyear column
BothChambers_Session_109_to_114_Short$birthyear <- stringr::str_extract(BothChambers_Session_109_to_114_Short$birth , "^\\d{4}") 
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>% dplyr::select(-c(birth))

#Create pageid_session variable for merging later on 

BothChambers_Session_109_to_114_Short$pageid2 <- BothChambers_Session_109_to_114_Short$pageid 
BothChambers_Session_109_to_114_Short$session2 <- BothChambers_Session_109_to_114_Short$session
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>%  unite(pageid_session, pageid2, session2)



# Subsetting main_dataframe_dates into the defined time frame (Sessions 109-114)
main_dataframe_core <- main_dataframe_dates %>% filter(date_LegislatoR <= "2017-01-03")  %>% filter(date_LegislatoR >= "2005-01-04")
# Out: main_dataframe_core



#-------------------------------------------------------------------------------------------------------------------
# Creating a list of edits in the observed time frame that are done on profiles of MoCs that do serve anymore at the time of the edit

# Getting the data 
main_dataframe_core$year_edit <- stringr::str_extract(main_dataframe_core$date_LegislatoR , "^\\d{4}") 

# Creating a column indicating in which session an edit was made
for(i in 1:length(main_dataframe_core$year_edit)) {
  if (main_dataframe_core$year_edit[i] ==  2005) {
    main_dataframe_core$year_edit[i] = 109
  } 
  else if (main_dataframe_core$year_edit[i] ==  2006) {
    main_dataframe_core$year_edit[i] = 109
  }
  else if (main_dataframe_core$year_edit[i] ==  2007) {
    main_dataframe_core$year_edit[i] = 110
  } 
  else if (main_dataframe_core$year_edit[i] ==  2008) {
    main_dataframe_core$year_edit[i] = 110
  }
  else if (main_dataframe_core$year_edit[i] ==  2009) {
    main_dataframe_core$year_edit[i] = 111
  }
  else if (main_dataframe_core$year_edit[i] ==  2010) {
    main_dataframe_core$year_edit[i] = 111
  }
  else if (main_dataframe_core$year_edit[i] ==  2011) {
    main_dataframe_core$year_edit[i] = 112
  }
  else if (main_dataframe_core$year_edit[i] ==  2012) {
    main_dataframe_core$year_edit[i] = 112
  }
  else if (main_dataframe_core$year_edit[i] ==  2013) {
    main_dataframe_core$year_edit[i] = 113
  }
  else if (main_dataframe_core$year_edit[i] ==  2014) {
    main_dataframe_core$year_edit[i] = 113
  }
  else if (main_dataframe_core$year_edit[i] ==  2015) {
    main_dataframe_core$year_edit[i] = 114
  }
  else if (main_dataframe_core$year_edit[i] ==  2016) {
    main_dataframe_core$year_edit[i] = 114
  }
  else if (main_dataframe_core$year_edit[i] ==  2017) {
    main_dataframe_core$year_edit[i] = 114
  }
  else {
    main_dataframe_core$year_edit[i] = main_dataframe_core$year_edit[i]
  }
}

# Create a merge-id
main_dataframe_core$pageid2 <- main_dataframe_core$pageid 
main_dataframe_core <- main_dataframe_core %>%  unite(pageid_year, pageid2, year_edit)

#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
Merged_House_Data <- merge(house_us_core, house_us_political, by="pageid")
# Filter for Sessions we want to observe: 109th - 114th Session
House_Session_109_to_114 <- Merged_House_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
# Only keep the first 17 variables (other not needed)
House_Session_109_to_114_Short <- House_Session_109_to_114  %>% dplyr::select(1:18)
# Getting rid of 2 more variables that are not needed 
House_Session_109_to_114_Short <- House_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, deathplace))

#Create Merge-id
House_Session_109_to_114_Short$pageid2 <- House_Session_109_to_114_Short$pageid 
House_Session_109_to_114_Short$session2 <- House_Session_109_to_114_Short$session
House_Session_109_to_114_Short <- House_Session_109_to_114_Short %>%  unite(pageid_year, pageid2, session2)
length(unique(House_Session_109_to_114_Short$pageid))
length(unique(main_dataframe_core$pageid))

# Create data set that contains all "right" edits for the House
House_Session_109_to_114_Edits <- left_join(House_Session_109_to_114_Short, main_dataframe_core, by = "pageid_year")


# Same for Senate
#Merge Core and Political Data Set for 1. House, 2. Senate by page-id
Merged_Senate_Data <- merge(senate_us_core, senate_us_political, by="pageid")
# Filter for Sessions we want to observe: 109th - 114th Session
Senate_Session_109_to_114 <- Merged_Senate_Data %>%  filter(session <= 114) %>%  filter(session >= 109)
# Only keep the first 17 variables (other not needed)
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114  %>% dplyr::select(1:18)
# Getting rid of 2 more variables that are not needed 
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114_Short %>% dplyr::select(-c(birthplace, deathplace))

# Create Merge-id
Senate_Session_109_to_114_Short$pageid2 <- Senate_Session_109_to_114_Short$pageid 
Senate_Session_109_to_114_Short$session2 <- Senate_Session_109_to_114_Short$session
Senate_Session_109_to_114_Short <- Senate_Session_109_to_114_Short %>%  unite(pageid_year, pageid2, session2)
length(unique(Senate_Session_109_to_114_Short$pageid))

# Create data set that contains all "right" edits for the Senate
Senate_Session_109_to_114_Edits <- left_join(Senate_Session_109_to_114_Short, main_dataframe_core, by = "pageid_year")

# Combine both Chambers 
BothChambers_Session_109_to_114_Edits <- rbind(House_Session_109_to_114_Edits , Senate_Session_109_to_114_Edits)
table(is.na(BothChambers_Session_109_to_114_Edits$date_LegislatoR) == T)

# Get list of edits that should not be included in the data 
list_revid <- BothChambers_Session_109_to_114_Edits$revid

# Dataframe
false_edits_df <- main_dataframe_core %>% filter((revid %in% list_revid) == F)
# List
false_edits_list <- false_edits_df$revid

# Change name of pagedid_year into pagedid_session
main_dataframe_core <- rename(main_dataframe_core, pageid_session =  pageid_year)

#Filter false edits out from_dataframe_core
main_dataframe_true <- main_dataframe_core %>% filter((revid %in% false_edits_list) == F)
# Reduces number of edits from 2601 to 2448

# Out: main_dataframe_tru
# Saving Dataframe 
#save(main_dataframe_true , file = "main_dataframe_true.Rdata")


#-------------------------------------------------------------------------------------------------------------------
#-------------------------- Include Edits counts for edits coming from within Congress -----------------------------

# All edits from within Congress:
Inside_Congress_Edits <- main_dataframe_true
Inside_Congress_Edits$count <- 1
Inside_Congress_Edits <- Inside_Congress_Edits   %>% dplyr::select(c(pageid_session, count))
# Aggregate 
Inside_Congress_Edits_Aggregate <- aggregate(Inside_Congress_Edits[, 2], list(Inside_Congress_Edits$pageid_session), sum)
names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "Group.1"] <- "pageid_session"
names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "count"] <- "AllCongressEdits_Per_MoC_Session"

#Out: Inside_Congress_Edits_Aggregate



#-------------------------------------------------------------------------------------------------------------------
#-------------------------- Include Politically Edits counts for edits coming from within Congress ----------------

# Politically motivated edits from within Congress: For better insights the column should be entangled in different columns 

Inside_Congress_Edits_Politically <- main_dataframe_true

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

table(Inside_Congress_Edits_Politically$politically_motivated) # 0 -> 1075  ,  1 -> 1373

#Out: Inside_Congress_Edits_Politically -> list of edits from within Congress with Dummy for politically motivation for each edit
# Saving Dataframe 
#save(Inside_Congress_Edits_Politically , file = "Inside_Congress_Edits_Politically.Rdata")

# Aggregation Politically Edits from within Congress per MoC: 
Inside_Congress_Edits_Politically$count <- 1
Inside_Congress_Edits_Politically1 <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)
Inside_Congress_Edits_Politically1 <- Inside_Congress_Edits_Politically1  %>% dplyr::select(c(pageid_session, count))

# Aggregate 
Inside_Congress_Edits_Politically1_Aggregate <- aggregate(Inside_Congress_Edits_Politically1[, 2], list(Inside_Congress_Edits_Politically1$pageid_session), sum)
names(Inside_Congress_Edits_Politically1_Aggregate)[names(Inside_Congress_Edits_Politically1_Aggregate) == "Group.1"] <- "pageid_session"
names(Inside_Congress_Edits_Politically1_Aggregate)[names(Inside_Congress_Edits_Politically1_Aggregate) == "count"] <- "All_Politically_CongressEdits_Per_MoC_Session"


# Merge Aggregated Edit-Columns back with Characteristics_Data_MoCs
BothChambers_Session_109_to_114_Short <- left_join(BothChambers_Session_109_to_114_Short, Inside_Congress_Edits_Aggregate , by ="pageid_session")
BothChambers_Session_109_to_114_Short <- left_join(BothChambers_Session_109_to_114_Short, Inside_Congress_Edits_Politically1_Aggregate , by ="pageid_session")
#Out: BothChambers_Session_109_to_114_Short 

#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#---------------------- Dividing Congress Edits into positive politically and negative politically  ----------------

# As some politically motivated edits to a profile are of a destructive and some of a beneficial nature
# We can assume that the destructive ones are made by opponents, while the beneficial ones are done by the MoC or his/her staffers themselves
# Therefore we should have different columns to distinguish between those two kinds of edits 

# Politically motivated edits from within Congress: 
Inside_Congress_Edits_Politically_PositiveNegative <- main_dataframe_true

# Create Dummy for political motivation of edit and set it to 0 
Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive <- 0
Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative <- 0


# Politically Motivated Edits: (see definition in Fließtext Masterabeit)

pattern_3 = "rather_beneficial"
pattern_4 = "mostly_beneficial"
pattern_5 = "rather_harmful"
pattern_6 = "mostly_harmful"



#------------- Exploring the data to find out what constitutes a positive or negative politically motivated edit


# Set dummy for each edit in the columns politically_motivated_positive and politically_motivated_negative

# Filtering Column: Answer.content_added
for(i in 1:2448) {                                      
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_3, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1 
  }
}

for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_4, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1 
  }
}

for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_5, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1 
  }
}

for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_added[i], pattern_6, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1 
  }
}

# Filtering Column: Answer.content_removed
for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_3, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1 
  }
}

for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_4, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = 1
  }
}

for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_5, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1 
  }
}

for(i in 1:2448) {
  if (sjmisc::str_contains(Inside_Congress_Edits_Politically_PositiveNegative$Answer.content_removed[i], pattern_6, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = 1
  }
}

table(Inside_Congress_Edits_Politically$politically_motivated) # 0 -> 1075  ,  1 -> 1373
table(Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)  # positive 1254
table(Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative)  #negative 221

# Check whether some posts are indicated to be both positive and negative politically motivated
Both <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1) %>% filter(politically_motivated_negative == 1) 

# Posts that have a marker for both positive and negative are filtered again and insults are marked as negative and praise as positive (rest = neutral)

pattern_10 = "insults_mockery"
pattern_11 = "praise_adulation"

Politically_Both <- Inside_Congress_Edits_Politically_PositiveNegative %>% 
  filter(politically_motivated_positive == 1) %>% 
  filter(politically_motivated_negative == 1) 

#"insults_mockery"
for(i in 1:126) {
  if (sjmisc::str_contains(Politically_Both$Answer.characteristics[i], pattern_10, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Politically_Both$politically_motivated_positive[i] = 0
  }
} 

#"praise_adulation"
for(i in 1:126) {
  if (sjmisc::str_contains(Politically_Both$Answer.characteristics[i], pattern_11, ignore.case = FALSE, logic = NULL, switch = FALSE)) {
    Politically_Both$politically_motivated_negative[i] = 0
  }
} 

# neither "praise_adulation" nor "insults_mockery" -> set to 0
for(i in 1:126) {
  if (Politically_Both$politically_motivated_positive[i] == 1 && Politically_Both$politically_motivated_negative[i] == 1) { 
    Politically_Both$politically_motivated_positive[i] = 0
    Politically_Both$politically_motivated_negative[i] = 0
  }
} 


# Changed values of Politically_Both are transfered to main data set (-> Inside_Congress_Edits_Politically_PositiveNegative)
length(Inside_Congress_Edits_Politically_PositiveNegative$pageid)

for(i in 1:2448) {
  for (j in 1:126) {
    if (Inside_Congress_Edits_Politically_PositiveNegative$revid[i] == Politically_Both$revid[j]) { 
      Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative[i] = Politically_Both$politically_motivated_negative[j]
      Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive[i] = Politically_Both$politically_motivated_positive[j]
    }
  }
} 


#Out: Inside_Congress_Edits_Politically_PositiveNegative -> list of edits from within Congress with Dummy for positive and negative political motivation for each edit
# Saving Dataframe 
#save(Inside_Congress_Edits_Politically_PositiveNegative , file = "Inside_Congress_Edits_Politically_PositiveNegative.Rdata")


# Aggregation positive Politically Edits from within Congress per MoC: 
Inside_Congress_Edits_Politically_PositiveNegative$count <- 1
Inside_Congress_Edits_Politically_Positive <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1)
Inside_Congress_Edits_Politically_Positive <- Inside_Congress_Edits_Politically_Positive  %>% dplyr::select(c(pageid_session, count))

# Aggregation negative Politically Edits from within Congress per MoC: 
Inside_Congress_Edits_Politically_Negative <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_negative == 1)
Inside_Congress_Edits_Politically_Negative <- Inside_Congress_Edits_Politically_Negative  %>% dplyr::select(c(pageid_session, count))


# Aggregate 
Inside_Congress_Edits_Politically_Positive_Aggregate <- aggregate(Inside_Congress_Edits_Politically_Positive[, 2], list(Inside_Congress_Edits_Politically_Positive$pageid_session), sum)
names(Inside_Congress_Edits_Politically_Positive_Aggregate)[names(Inside_Congress_Edits_Politically_Positive_Aggregate) == "Group.1"] <- "pageid_session"
names(Inside_Congress_Edits_Politically_Positive_Aggregate)[names(Inside_Congress_Edits_Politically_Positive_Aggregate) == "count"] <- "All_Positive_Politically_CongressEdits_Per_MoC_Session"

# Aggregate 
Inside_Congress_Edits_Politically_Negative_Aggregate <- aggregate(Inside_Congress_Edits_Politically_Negative[, 2], list(Inside_Congress_Edits_Politically_Negative$pageid_session), sum)
names(Inside_Congress_Edits_Politically_Negative_Aggregate)[names(Inside_Congress_Edits_Politically_Negative_Aggregate) == "Group.1"] <- "pageid_session"
names(Inside_Congress_Edits_Politically_Negative_Aggregate)[names(Inside_Congress_Edits_Politically_Negative_Aggregate) == "count"] <- "All_Negative_Politically_CongressEdits_Per_MoC_Session"

table(Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative)
table(Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)


# Merge Aggregated Edit-Columns back with Characteristics_Data_MoCs

BothChambers_Session_109_to_114_Short <- left_join(BothChambers_Session_109_to_114_Short, Inside_Congress_Edits_Politically_Negative_Aggregate  , by ="pageid_session")
BothChambers_Session_109_to_114_Short <- left_join(BothChambers_Session_109_to_114_Short, Inside_Congress_Edits_Politically_Positive_Aggregate  , by ="pageid_session")

# For some edits it is clear that there is a political motivation but it is unclear whether this motivation is predominantly positve or negative
# That is why some MoCs see more politically motivated edits in general than the sum of negative and positive political edits 
# Reason: some edits see the removal of positive content and the adding of other positive content at the same time, therefore it is not possible to identify the motivation in a automated manner 


#-------------------------------------------------------------------------------------------------------------------
#---------------------- Fillings NAs for Count Variables, Create Chamber Variable  -----------------------------------

# Create Chamber Variable
BothChambers_Session_109_to_114_Short <- rename(BothChambers_Session_109_to_114_Short, Chamber  = country)
BothChambers_Session_109_to_114_Short$Chamber <- stringr::str_extract(BothChambers_Session_109_to_114_Short$Chamber , ".$") 

#Fillings NAs for Count Variables 
BothChambers_Session_109_to_114_Short$AllCongressEdits_Per_MoC_Session[is.na(BothChambers_Session_109_to_114_Short$AllCongressEdits_Per_MoC_Session)] <- 0
BothChambers_Session_109_to_114_Short$All_Politically_CongressEdits_Per_MoC_Session[is.na(BothChambers_Session_109_to_114_Short$All_Politically_CongressEdits_Per_MoC_Session)] <- 0
BothChambers_Session_109_to_114_Short$All_Positive_Politically_CongressEdits_Per_MoC_Session[is.na(BothChambers_Session_109_to_114_Short$All_Positive_Politically_CongressEdits_Per_MoC_Session)] <- 0
BothChambers_Session_109_to_114_Short$All_Negative_Politically_CongressEdits_Per_MoC_Session[is.na(BothChambers_Session_109_to_114_Short$All_Negative_Politically_CongressEdits_Per_MoC_Session)] <- 0



#-------------------------------------------------------------------------------------------------------------------
#---------------- Including Total Number of Edits (not only from Congress) per Session   ---------------------------

# Extracting year of edit and create Merge-ID: pageid_session 
house_us_history$year <- stringr::str_extract(house_us_history$timestamp , "^\\d{4}") 
house_us_history$year <- stringr::str_replace_all(house_us_history$year, " ", "")
house_us_history$year<- as.double(house_us_history$year)

senate_us_history$year <- stringr::str_extract(senate_us_history$timestamp , "^\\d{4}") 
senate_us_history$year <- stringr::str_replace_all(senate_us_history$year, " ", "")
senate_us_history$year<- as.double(senate_us_history$year)

# Combine both chambers
BothChambers_us_history <- rbind(house_us_history, senate_us_history)
# Filter for examined time frame
BothChambers_us_history <- BothChambers_us_history  %>%  filter(year >= 2005) %>%  filter(year <= 2016)

# Create session column
BothChambers_us_history$session <- BothChambers_us_history$year

BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2005", "109")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2006", "109")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2007", "110")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2008", "110")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2009", "111")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2010", "111")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2011", "112")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2012", "112")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2013", "113")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2014", "113")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2015", "114")
BothChambers_us_history$session <- stringr::str_replace_all(BothChambers_us_history$session, "2016", "114")


# Create merging variable: pageid_session
BothChambers_us_history$count <- 1
BothChambers_us_history <- BothChambers_us_history %>%  unite(pageid_session, pageid, session, sep = "_", remove = FALSE)

# Aggregate Counts of Edits (column-number: 12)
ExternalEdits_Per_MoC_Session <- aggregate(BothChambers_us_history[, 12], list(BothChambers_us_history$pageid_session), sum)
# Change column-names  
names(ExternalEdits_Per_MoC_Session)[names(ExternalEdits_Per_MoC_Session) == "Group.1"] <- "pageid_session"
names(ExternalEdits_Per_MoC_Session)[names(ExternalEdits_Per_MoC_Session) == "x"] <- "ExternalEdits_per_MoC_Session"

#Merge with MoC data: BothChambers_Session_109_to_114_Short
BothChambers_Session_109_to_114_Short <-  left_join(BothChambers_Session_109_to_114_Short , ExternalEdits_Per_MoC_Session , by = "pageid_session")

# Fill NAs (3 out of 3319)
BothChambers_Session_109_to_114_Short$ExternalEdits_per_MoC_Session[is.na(BothChambers_Session_109_to_114_Short$ExternalEdits_per_MoC_Session)] <- 0

#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#----------------         Including Total Number of Views on Wikipedia Profile        ------------------------------


# Get Column for popularity of MoCs from Page Views 
# Problem: House data only ranges from from 2009 to 2016 and can thus probably not be used 

# Get Data
a <- house_us_traffic
b <- senate_us_traffic
ab <- rbind(a, b)


# Reduce dataset to pageids that are in main data set = Characteristics_Data_MoCs
list_unique_pageid <- unique(BothChambers_Session_109_to_114_Short$pageid)
abc <- subset(ab, pageid  %in% list_unique_pageid)
length(unique(abc$pageid)) # = 981

abcd <- unique(abc) # dropping rows that are doubles (due to MoCs serving in both chambers)
length(unique(abcd$pageid))

# Creating Year-Column
abcd$date <- as.character(abcd$date)
abcd$Year <- stringr::str_extract(abcd$date  , "^\\d{4}") 
abcd$Year <- as.double(abcd$Year)

# 1. Views Version One: only includes MoCs that served between 2009 and 2016 (Views per Session)
# Saved in separate Dataframe: Views_per_pageid_session

# Filter for timeframe (but as data for house starts in 2009, it will be shifted back to 2009-2016)
abcde <- abcd %>%  filter(Year >= 2009) %>% filter(Year <= 2016)
length(unique(abcde$pageid))

# Create session column
abcde$session <- abcde$Year

abcde$session <- stringr::str_replace_all(abcde$session, "2009", "111")
abcde$session <- stringr::str_replace_all(abcde$session, "2010", "111")
abcde$session <- stringr::str_replace_all(abcde$session, "2011", "112")
abcde$session <- stringr::str_replace_all(abcde$session, "2012", "112")
abcde$session <- stringr::str_replace_all(abcde$session, "2013", "113")
abcde$session <- stringr::str_replace_all(abcde$session, "2014", "113")
abcde$session <- stringr::str_replace_all(abcde$session, "2015", "114")
abcde$session <- stringr::str_replace_all(abcde$session, "2016", "114")

# Create Merge-id pageid_session
abcde <- abcde %>%  unite(pageid_session, pageid, session, sep = "_", remove = FALSE)

# Aggregate View Data by pageid_session
abcdef <- abcde  %>% dplyr::select(c(pageid_session, traffic))
abcdef <-  aggregate(abcdef[, 2], list(abcdef$pageid_session), sum)

names(abcdef  )[names(abcdef ) == "Group.1"] <- "pageid_session"
names(abcdef  )[names(abcdef  ) == "x"] <- "ProfileViewsPerSession09_to_16"

Views_per_pageid_session <- abcdef

# Out: Views_per_pageid_session
# Saving Dataframe 
save(Views_per_pageid_session , file = "Views_per_pageid_session.Rdata")

# - End of  Views Version One -


# 2. Views Version Two : all MoCs but just data from 2009 and 2016 aggregated over whole timeframe (not per Session)

# Filter for timeframe (but as data for house starts in 2009, it will be shifted back to 2009-2018
abcde <- abcd %>%  filter(Year >= 2009) %>%  filter(Year <= 2018)
length(unique(abcde$pageid))

# Aggregate View Data by pageid
abcdef <- abcde  %>% dplyr::select(c(pageid, traffic))
abcdef <-  aggregate(abcdef[, 2], list(abcdef$pageid), sum)

names(abcdef  )[names(abcdef ) == "Group.1"] <- "pageid"
names(abcdef  )[names(abcdef  ) == "x"] <- "ProfileViewsSum09_to_18"

length(unique(abcdef$pageid))

# Using Summary Statistics to Compute Categories for High of Number of Views (1-5)
summary(abcdef$ProfileViewsSum09_to_18)

# Categories: 1. lower 25%, 2. 26%-50%, 3. 51%-75%, 4. 76%-95% , 5. 96%-100%

abcdef$ViewCategory = 0

for(i in 1:length(abcdef$ProfileViewsSum09_to_18)) {
  if (abcdef$ProfileViewsSum09_to_18[i] <= 177684) {
    abcdef$ViewCategory[i] = 1  #firtst quartile
  }
  if (abcdef$ProfileViewsSum09_to_18[i] > 177684 && abcdef$ProfileViewsSum09_to_18[i] <= 302908 ) {
    abcdef$ViewCategory[i] = 2  # second quartile
  }
  if (abcdef$ProfileViewsSum09_to_18[i] > 302908 && abcdef$ProfileViewsSum09_to_18[i] <= 757801 ) {
    abcdef$ViewCategory[i] = 3  # third quartile
  }
}

# Get last 5% - threshold: 
ShortLived_df <- abcdef%>% filter( ViewCategory == 0) #contains the last quartile -> taking the last two decils (25%*0,1*2 = 5%)
quantile(ShortLived_df$ProfileViewsSum09_to_18, prob = seq(0, 1, length = 11), type = 5) # 5% = bigger than 3604214

# Use 5% threshold to compute last 2 categories: 

for(i in 1:length(abcdef$ProfileViewsSum09_to_18)) {
  if (abcdef$ProfileViewsSum09_to_18[i] > 757801 && abcdef$ProfileViewsSum09_to_18[i] <= 3604214 ) {
    abcdef$ViewCategory[i] = 4   #forth quartile minus the top 5%
  }
  if (abcdef$ProfileViewsSum09_to_18[i] > 3604214) {
    abcdef$ViewCategory[i] = 5  # last 5% 
  }
}

#Merge with MoC data: BothChambers_Session_109_to_114_Short
BothChambers_Session_109_to_114_Short <-  left_join(BothChambers_Session_109_to_114_Short ,  abcdef , by = "pageid")

#-------------------------------------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------------------------------------
#---------------- Creating Competitiveness Variable based on Voting Results   -------------------------------

#--------------Voting Data - House: Creating Vote Margin Variable and Merging-ID -------------------------

# Getting Vote Margin Data 
X1976_2018_house3 <- read_csv("Input_Data on Election Dates and Vote Margins /Vote Margins/Dataverse Harvard1976-2018-house/1976-2018-house3.csv")
vote_data_house <- X1976_2018_house3  #data does not include Oversee Terretories like Guam, Puerto Rico, Washington D.C. etc.

# Creating an id to be able to merge both datasets: id of vote margin = State - District - Session

vote_data_house$session <- vote_data_house$year
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2004", "109")
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2006", "110")
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2008", "111")
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2010", "112")
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2012", "113")
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2014", "114")
vote_data_house$session <- stringr::str_replace_all(vote_data_house$session, "2016", "115")

vote_data_house$Chamber <- "H"
vote_data_house <- vote_data_house %>%  unite(district_id, state, district, session, Chamber, sep = "_", remove = FALSE)
#### district: **Description**: district number ****Note****:	At-large districts are coded as 0 (zero).
# at-large districts are districts that contain a whole state: namely: Alaska, Delaware, Montana,North Dakota,South Dakota,Vermont,Wyoming

# Filter for the time frame that will be analysed 
vote_data_house <- vote_data_house %>%  filter(year >= 2004) %>%  filter(year <= 2015)
# Filter out primary elections (only keep general elections)
# But First: dealing with NAs

length(vote_data_house$stage)
for (i in 1:8379) {
  if(is.na(vote_data_house$stage[i])== T) {
    vote_data_house$stage[i] = "gen" }}

vote_data_house <- vote_data_house %>%  filter(stage == "gen") 

#Getting the relative vote margin in vote_data_house: -> vote_data_house$vote_maxdiff_relative
require(data.table) 
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

# Drop two rows that are still adressing the same MoC in the same session
length(vote_data_house_merge$district_id) #2612
vote_data_house_merge <- vote_data_house_merge[-2284,]
vote_data_house_merge <- vote_data_house_merge[-2277,]

#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#--------------------------------- Creating a Merging ID for BothChambers_Session_109_to_114_Short------------------


# ID: state + district_number + session + Chamber
# e.g.: 	ALABAMA_1_109_H

# Creating variable state_short and district_number 
BothChambers_Session_109_to_114_Short$state_short <- BothChambers_Session_109_to_114_Short$constituency
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "congressional district", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "at-large", "0")
BothChambers_Session_109_to_114_Short$district_number  <- stringr::str_extract(BothChambers_Session_109_to_114_Short$state_short, "\\d+") 
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "\\d+nd", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "\\d+st", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "\\d+th", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "\\d+rd", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "'s", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "\\d+", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, " $", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, " $", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, " $", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "^ ", "")
BothChambers_Session_109_to_114_Short$state_short <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$state_short, "^ ", "")

length(BothChambers_Session_109_to_114_Short$state_short)

#Transform state names into uppercase 
for (i in 1: 3319) {
  string <- BothChambers_Session_109_to_114_Short$state_short[i] 
  STRING <- toupper(string)
  BothChambers_Session_109_to_114_Short$state_short[i] <- STRING }

BothChambers_Session_109_to_114_Short$district_id <- NA

#Create ID
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>%  unite(district_id, state_short, district_number, session, Chamber, sep = "_", remove = FALSE)

#length(BothChambers_Session_109_to_114_Short$Chamber)
for (i in 1: 3319) {
  if (BothChambers_Session_109_to_114_Short$Chamber[i] == "S") {
  BothChambers_Session_109_to_114_Short$district_id[i] = NA
  }
}

#Sicherheitskopie: BothChambers_Session_109_to_114_Short
#Sicherheit <- BothChambers_Session_109_to_114_Short

# Merge Vote Margins from House elections with Main-Data set
BothChambers_Session_109_to_114_Short <- left_join(BothChambers_Session_109_to_114_Short, vote_data_house_merge, by = "district_id")
# As already said, there is only voting data for the 50. regular US states not Guam, D.C., Puerto Rico etc.

#-------------------------------------------------------------------------------------------------------------------






#-------------------------------------------------------------------------------------------------------------------
#----------------------- Voting Data - Senate: Creating Vote Margin Variable and Merging-ID -------------------------
#-------------------------------------------------------------------------------------------------------------------


X1976_2020_senate <- read_csv("Input_Data on Election Dates and Vote Margins /Vote Margins/Dataverse Harvard1976-2020-senate/1976-2020-senate.csv")
vote_data_senate <- X1976_2020_senate 

# Choose time frame
vote_data_senate <- vote_data_senate %>%  filter(year >= 2004) %>%  filter(year <= 2015)
vote_data_senate$session <- vote_data_senate$year

vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2004", "109")
vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2006", "110")
vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2008", "111")
vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2010", "112")
vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2012", "113")
vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2014", "114")
vote_data_senate$session <- stringr::str_replace_all(vote_data_senate$session, "2016", "115")

# Keep only general elections
vote_data_senate <- vote_data_senate %>%  filter(stage == "gen") 


#Create ID: state-year-special 
#Including marker for special elections (due to a Senator that ended his term early) 
vote_data_senate$Special <- vote_data_senate$special 
vote_data_senate$Special<- stringr::str_replace_all(vote_data_senate$Special, "TRUE", "Special")
vote_data_senate$Special<- stringr::str_replace_all(vote_data_senate$Special, "FALSE", "")
#Creating Election_id
vote_data_senate <- vote_data_senate %>%  unite(Election_id, state, session, Special, sep = "_", remove = FALSE)

vote_data_senate$last_winner <-  vote_data_senate$candidate
vote_data_senate$last_winner <- stringr::str_replace_all(vote_data_senate$last_winner, ", JR.", "")
vote_data_senate$last_winner <- stringr::str_replace_all(vote_data_senate$last_winner, " JR.", "")
vote_data_senate$last_winner <- stringr::str_replace_all(vote_data_senate$last_winner, " III", "")
vote_data_senate$last_winner <- stringr::str_replace_all(vote_data_senate$last_winner, " IV", "")
vote_data_senate$last_winner <-  stringr::str_extract(vote_data_senate$last_winner, "[[:alpha:]]+$") 


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

length(vote_data_senate$year)

#Select only the winner rows (with the last name of the winner of each race in the district_id)
vote_data_senate$winner = 0
for (i in 1:1084) {
  if(vote_data_senate$candidatevotes[i] == vote_data_senate$candidatesvotes_first_max[i]) {
    vote_data_senate$winner[i] = 1 }}
vote_data_senate_winner <- vote_data_senate %>%  filter(winner == 1)

# Include Last name of winner into the id
vote_data_senate_winner$Election_id <- stringr::str_replace_all(vote_data_senate_winner$Election_id, "_Special", "")
vote_data_senate_winner <- vote_data_senate_winner %>%  unite(Election_id_Senate, Election_id, last_winner, sep = "_", remove = FALSE)

# Prepare for mergin with LegislatoR Senate Dataset
vote_data_senate_merge <- vote_data_senate_winner %>% dplyr::select(c(Election_id_Senate, vote_maxdiff_relative_Senate , vote_maxdiff_absolute_Senate ))




# Merge Senate Vote Margins with Main Data: BothChambers_Session_109_to_114_Short
BothChambers_Session_109_to_114_Short$last_winner <- BothChambers_Session_109_to_114_Short$name
BothChambers_Session_109_to_114_Short$last_winner <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$last_winner, ", Jr.", "")
BothChambers_Session_109_to_114_Short$last_winner <-  stringr::str_extract(BothChambers_Session_109_to_114_Short$last_winner, "[[:alpha:]]+$") 



#Transform last names into uppercase 
for (i in 1: 3319) {
  string <- BothChambers_Session_109_to_114_Short$last_winner[i] 
  STRING <- toupper(string)
  BothChambers_Session_109_to_114_Short$last_winner[i] <- STRING }

BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>%  unite(Election_id_Senate, state_short, session, last_winner, sep = "_", remove = FALSE)

#length(BothChambers_Session_109_to_114_Short$Chamber)
for (i in 1: 3319) {
  if (BothChambers_Session_109_to_114_Short$Chamber[i] == "H") {
    BothChambers_Session_109_to_114_Short$Election_id_Senate[i] = NA
  }
}

#Merging 
BothChambers_Session_109_to_114_Short  <- left_join(BothChambers_Session_109_to_114_Short, vote_data_senate_merge , by = "Election_id_Senate")


# Filling missing Values in vote_maxdiff_relative_senate by
# 1. Filling it for the session without elections on a MoS-base via "down" and "up"
# 2. For MoSs without any election in the observed time-frame, take mean of the respective State 

AAA <- BothChambers_Session_109_to_114_Short %>% filter(Chamber == "S")

CCC <- AAA[1,] 
CCC[1] <- 0

for (i in 1: 981) {
  string = pageid[i]
  BBB <- AAA %>% filter(pageid == string) 
  BBB <- BBB %>% arrange(session)
  BBB <- BBB %>% fill(vote_maxdiff_relative_Senate, .direction = "down")
  BBB <- BBB %>% fill(vote_maxdiff_relative_Senate, .direction = "up")
  CCC <- rbind(CCC, BBB)
}

CCC <- CCC %>% filter(pageid != 0)

table(is.na(CCC$vote_maxdiff_relative_Senate) ==T) #still 42 with missing values 
# Solution: take State average 

#Take the mean for each of the 50 States
DDD <- CCC %>% dplyr::select(c(state_short, vote_maxdiff_relative_Senate))
DDD <- DDD %>% drop_na()
Senate_MeanVoteMargins_by_State <- aggregate(DDD[, 2], list(DDD$state_short), mean)
Senate_MeanVoteMargins_by_State <- rename(Senate_MeanVoteMargins_by_State, "vote_maxdiff_relative_Senate_MEAN" = x)
Senate_MeanVoteMargins_by_State <- rename(Senate_MeanVoteMargins_by_State, "state_short" = Group.1)

CCC <- left_join(CCC, Senate_MeanVoteMargins_by_State , by = "state_short")

# Replace NA by mean of state in vote_maxdiff_relatvie_Senate in CCC

#length(CCC$pageid)
for (i in 1:621) {
  if(is.na(CCC$vote_maxdiff_relative_Senate[i]) == T) {
    CCC$vote_maxdiff_relative_Senate[i] = CCC$vote_maxdiff_relative_Senate_MEAN[i] }
}


# Merge back with main data: 
CCC <- CCC %>%dplyr::select(c("Election_id_Senate", "vote_maxdiff_relative_Senate"))
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>%  dplyr::select(-c(vote_maxdiff_relative_Senate, vote_maxdiff_absolute_Senate))

BothChambers_Session_109_to_114_Short  <- left_join(BothChambers_Session_109_to_114_Short, CCC  , by = "Election_id_Senate")

# But Vote_Max_Diff_Values into one column
#length(BothChambers_Session_109_to_114_Short$pageid)
for (i in 1:3319) {
  if(is.na(BothChambers_Session_109_to_114_Short$vote_maxdiff_relative[i]) == T) {
    BothChambers_Session_109_to_114_Short$vote_maxdiff_relative[i] = BothChambers_Session_109_to_114_Short$vote_maxdiff_relative_Senate[i] }
}

table((is.na(BothChambers_Session_109_to_114_Short$vote_maxdiff_relative) == T)) #only NAs are now the territories where I do not have voting data 


# Clean data set and delete columns that are not needed
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>%
  dplyr::select(-c(vote_maxdiff_relative_Senate, vote_maxdiff_absolute, last_winner, district_number, district_id, Election_id_Senate ))

# Drop Tim Scott Senate Session 112 as he served only 1 day 
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short[-2860,]


# Sort MoCs from smaller Parties to either D or R in a new column

BothChambers_Session_109_to_114_Short$party_dual <- BothChambers_Session_109_to_114_Short$party
table(BothChambers_Session_109_to_114_Short$party_dual)

BothChambers_Session_109_to_114_Short$party_dual <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$party_dual, "DFL", "D")
BothChambers_Session_109_to_114_Short$party_dual <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$party_dual, "PNP/D", "D")
BothChambers_Session_109_to_114_Short$party_dual <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$party_dual, "PNP/R", "R")
BothChambers_Session_109_to_114_Short$party_dual <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$party_dual, "DFL", "D")
BothChambers_Session_109_to_114_Short$party_dual <- stringr::str_replace_all(BothChambers_Session_109_to_114_Short$party_dual, " ", "")

# Independents: all 5 independents in the data set were caucusing with the Democrats in the observed time frame 
# Bernie_Sanders, Gregorio_Sablan, Joe_Lieberman ,Angus_King, Jim_Jeffords

for (i in 1:length(BothChambers_Session_109_to_114_Short$party_dual)) {
  if(BothChambers_Session_109_to_114_Short$party[i] == "Independent") {
    BothChambers_Session_109_to_114_Short$party_dual[i] = "D" }
}


# Out: BothChambers_Session_109_to_114_Short
# Saving Dataframe 
#save(BothChambers_Session_109_to_114_Short , file = "BothChambers_Session_109_to_114_Short.Rdata")




 




