
#09b_Fiugres_EditsOverTimes_DescriptiveStatistics

#This cript contains code for creating figures:
# 1. All edits from within Congress over time (with markers for elections dates)
# 2. Political edits from within Congress over time (with markers for elections dates)
# 3. All edits and all edtis from within Congress in over time (comparison) in one graph 
# 4. Comparison of distribution of topics between non political edits from within congress and political edits 




#-------------------------------------------------------------------------------------------------------------------
#------------------------ Figures: Edits from within Congress over time (compared to election dates) ---------------
#-------------------------------------------------------------------------------------------------------------------


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)


# Load Edit Data (Sessions 109th-114th)
data <- main_dataframe_core


# 1. All edits from within Congress in observed time frame over time 

#Figure 9b(1)
edits_90days <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=90, alpha=0.8) +
  ggtitle("All edits on MoCs Wikipedia Profiles from within Congress during 109th-114th session (Bin size = 90 Days)") +
  theme_ipsum()
edits_90days 

# Same but with markers for election dates (general elections every two years)
edits_210days_elections  <- data %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_210days_elections  





# 2. Political edits from within Congress over time (with markers for elections dates)
#Load Edit data with markers for politically motivated edits: 

data1 <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)

edits_politically_210days_elections   <- data1 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_politically_210days_elections  





# 3. Beneficial Political edits from within Congress over time (with markers for elections dates)
# Load: Inside_Congress_Edits_Politically_PositiveNegative

data2 <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1)

edits_politically_210days_elections   <- data2 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All beneficial political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_politically_210days_elections  





# 4. Harmful Political edits from within Congress over time (with markers for elections dates)
# Load: Inside_Congress_Edits_Politically_PositiveNegative

data3 <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_negative == 1)

edits_politically_210days_elections   <- data3 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All harmful political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_politically_210days_elections  











# ------------------------------------------------ Ideenspeicher -------------------------------------------------


# 3. All edits and all edits from within Congress in over time (comparison) in one graph 
# -> Tedious and effect might be better visible if looking at correlation between Congress Edits and Overall Edits on MoC-Level

# Choosing observed time frame 
house_us_history_109_114 <- house_us_history  %>%  filter(year >= 2005) %>%  filter(year <= 2016)
# Creating Date Column without exat timestamp
house_us_history_109_114$date <- stringr::str_extract(house_us_history_109_114$timestamp , "^\\d{4}-\\d{2}-\\d{2}") 
house_us_history_109_114$date  <- stringr::str_replace_all(house_us_history_109_114$date , " ", "")  

house_us_history_109_114$date<- as.Date(house_us_history_109_114$date)

typeof(house_us_history_109_114$date)


all_edits_60days_elections <- house_us_history_109_114 %>%
  ggplot( aes(x= date)) +
  geom_histogram(binwidth= 60, alpha=0.9) +
  ggtitle("All political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7)
all_edits_60days_elections


# Display Congress edits and overall edits in the same graph (histogram)

# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

house_us_history_109_114_reduced <- house_us_history_109_114 %>% dplyr::select(c(date, revid))
house_us_history_109_114_reduced$type = "B"

main_dataframe_core_reduced <- main_dataframe_core %>% dplyr::select(c(date_LegislatoR, revid))
names(main_dataframe_core_reduced)[names(main_dataframe_core_reduced) == "date_LegislatoR"] <- "date"
main_dataframe_core_reduced$type = "A"

Edits_Reduced  <- rbind(house_us_history_109_114_reduced, main_dataframe_core_reduced)
# to have results that are actually visible the following line needs to be repeated 100 times: 
Edits_Reduced  <- rbind(Edits_Reduced , main_dataframe_core_reduced)


p <- Edits_Reduced  %>%
  ggplot( aes(x=date, fill=type)) +
  geom_histogram(binwidth= 100,  alpha=0.7, position = 'identity') +
  theme_ipsum() +
  scale_fill_manual(values=c( "#f68060", "#69b3a2")) +
  labs(fill="")
p


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------- End of SubChapter-----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------



