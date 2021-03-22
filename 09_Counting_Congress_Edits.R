#10_Counting_Congress_Edits

#in:
# BothChambers_Data_MoCs 
# main_dataframe_dates


#-------------------------------------------------------------------------------------------------------------------
#----------- Subsetting datasets with Congress edits to the observed timeframe (109th-114th session)----------------
#-------------------------------------------------------------------------------------------------------------------


# Subsetting main_dataframe_dates into the defined time frame (Sessions 109-114)
main_dataframe_core <- main_dataframe_dates %>% filter(date_LegislatoR <= "2017-01-03")  %>% filter(date_LegislatoR >= "2005-01-04")

# 638 different MoCs see edits in the observered time frame (January 4, 2005 until January 3, 2017)
length(unique(main_dataframe_core$wikititle)) 

# 981 different MoC served in Sessions 109-114 (MoCs that served in both chamber are counted just once)
length(unique(BothChambers_Data_MoCs$wikititle))
# Due to MoCs that served in both chamber, the dataset contains 1010 entries, each entry has a unique pageid_chamber variable 

# Ratio of MoCs that see Wikipedia-Edits coming from within Congress during their tenure: 
# 638/981 = 65%   #This means on the other side that 35% of MoCs must be counted as zeros
# So, is that an exessive number of zeros? 

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------
#---------------------- Merge separate rows for MoC who served in both Chambers into one row per MoC  -------------
#-------------------------------------------------------------------------------------------------------------------

Characteristics_Data_MoCs <- BothChambers_Data_MoCs 

Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Days_Served))
# Summing up by pageid for Senate
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- "Day_Served_Sum"
Merge_Data_Days_Served <- Merge_Data


Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, All_Edits_2004to16_MoC))
# Summing up by pageid for Senate
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- " All_Edits_2004to16_MoC_Sum"
Merge_Data_All_Edits_2004to16_MoC <- Merge_Data

Merge_Data <- Characteristics_Data_MoCs %>% dplyr::select(c(pageid, Overall_Edits_During_Tenure))
# Summing up by pageid for Senate
Merge_Data <-  aggregate(Merge_Data[, 2], list(Merge_Data$pageid), sum)
names(Merge_Data )[names(Merge_Data ) == "Group.1"] <- "pageid"
names(Merge_Data )[names(Merge_Data ) == "x"] <- "Overall_Edits_During_Tenure_Sum"
Merge_Data_Overall_Edits_During_Tenure <- Merge_Data

Merge_Data_1_2 <- left_join(Merge_Data_Day_Served, Merge_Data_All_Edits_2004to16_MoC, by ="pageid")

Merge_Data_1_2_3 <- left_join(Merge_Data_1_2, Merge_Data_Overall_Edits_During_Tenure , by ="pageid")

# Mean durch Gewichtung mit Days Served machen 

#,Mean_vote_maxdiff_relative,  Mean_vote_maxdiff_absolute, All_Edits_2004to16_MoC,  Overall_Edits_During_Tenure))




#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------------------------------------
#-------------------------- Include Edits counts for edits coming from within Congress -----------------------------
#-------------------------------------------------------------------------------------------------------------------

Inside_Congress_Edits <- main_dataframe_core

Inside_Congress_Edits$count <- 1

Inside_Congress_Edits <- Inside_Congress_Edits   %>% dplyr::select(c(pageid, date_LegislatoR, count))

# Extracting year of edit
Inside_Congress_Edits$year <- stringr::str_extract(Inside_Congress_Edits$date_LegislatoR , "^\\d{4}") 
Inside_Congress_Edits$year <- stringr::str_replace_all(Inside_Congress_Edits$year, " ", "")
Inside_Congress_Edits$year <- as.double(Inside_Congress_Edits$year)

Inside_Congress_Edits <- Inside_Congress_Edits  %>%  unite(pageid_year, pageid, year, sep = "_", remove = FALSE)

Inside_Congress_Edits_Aggregate <- aggregate(Inside_Congress_Edits[, 4], list(Inside_Congress_Edits$pageid_year), sum)
names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "Group.1"] <- "pageid_year"
names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "count"] <- "CongressEdits_Per_YearMoC"



Inside_Congress_Edits_Aggregate$year <- stringr::str_extract(Inside_Congress_Edits_Aggregate$pageid_year, "[[:punct:]]\\d+$")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "[[:punct:]]", "")


Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2005", "109")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2006", "109")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2007", "110")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2008", "110")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2009", "111")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2010", "111")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2011", "112")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2012", "112")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2013", "113")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2014", "113")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2015", "114")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2016", "114")
Inside_Congress_Edits_Aggregate$year <- stringr::str_replace_all(Inside_Congress_Edits_Aggregate$year, "2017", "114")

names(Inside_Congress_Edits_Aggregate)[names(Inside_Congress_Edits_Aggregate ) == "year"] <- "session"


colnames(Inside_Congress_Edits)


# Why am I separating House and Senate? 
# no reason: -> I should combine them 


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------




