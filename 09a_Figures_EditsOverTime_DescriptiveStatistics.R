
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

data2 <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)

edits_politically_210days_elections   <- data2 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_politically_210days_elections  






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






#-------------------------------------------------------------------------------------------------------------------
#----------------------Figures: Comparsion between groups: political edits and non-political edits) ----------------
#-------------------------------------------------------------------------------------------------------------------
#------------------------- Figures did not turn out that well: will use simple table probably  ---------------------
#-------------------------------------------------------------------------------------------------------------------


# 4. Comparison of distribution of topics between non political edits from within congress and political edits 

# Devide dataset into politically and non-politically 
Edits_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)
Edits_Non_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 0)

# Get number of entires for every topic category (to use it for pie-chart)
table(is.na(Edits_Politically$Answer.topic_personal))
table(is.na(Edits_Politically$Answer.topic_career))
table(is.na(Edits_Politically$Answer.topic_views))
table(is.na(Edits_Politically$Answer.topic_other))

Pie_Data_Topics_Politically <- data.frame (Topic = c("Personal", "Career","Views","Other"), NumberOfEntries = c(520, 1165, 466, 475))

#  For Non-politically 
table(is.na(Edits_Non_Politically$Answer.topic_personal))
table(is.na(Edits_Non_Politically$Answer.topic_career))
table(is.na(Edits_Non_Politically$Answer.topic_views))
table(is.na(Edits_Non_Politically$Answer.topic_other))

Pie_Data_Topics_Non_Politically <- data.frame (Topic = c("Personal", "Career","Views","Other"), NumberOfEntries = c(281, 766, 119, 257))



# Pie Politically
pie(Pie_Data_Topics_Politically$NumberOfEntries , border="white", col=myPalette, labels = c("Personal", "Career","Views","Other") ) 
#Pie Non_Politically
pie(Pie_Data_Topics_Non_Politically$NumberOfEntries , border="white", col=myPalette, labels = c("Personal", "Career","Views","Other") ) 


library(waffle)
library(RColorBrewer)
 
# Political Waffle Chart 
520+466+1165+475 # = 2626
#520/2626 #1165/2626 #466/2626 #475/2626

savings <- c(`Career (44.4%)`=1165, `Personal (19.8%)`=520,
             `Views (17.7%)`=466, `Other (18.1%)`=475)

waffle(savings/10, rows=7, size=0.5, 
       colors=c("#44D2AC", "#E48B8B", "#B67093", "#3A9ABD"), 
       title="Number of Entries per Topic in Political Edits", 
       xlab="1 square == 10 Entries")


#Non-Political Waffle Chart
766 + 281 + 119+  257 # = 1423

savings <- c(`Career (53.8%)`=766, `Personal (19.7%)`=281,
             `Views (8.7%)`=119, `Other (18.1%)`=257)

waffle(savings/5, rows=7, size=0.5, 
       colors=c("#44D2AC", "#E48B8B", "#B67093", "#3A9ABD"), 
       title="Number of Entries per Topic in Non Political Edits", 
       xlab="1 square == 5 Entries")




# Further looking into topic "personal" and subtopics of personal 

Edits_Non_Politically 
Edits_Politically 

# For better insights the column should be entangled in different columns 

table(Edits_Non_Politically$Answer.topic_personal)
Edits_Non_Politically$religion <- Edits_Non_Politically$Answer.topic_personal
Edits_Non_Politically$financial_earnings  <- Edits_Non_Politically$Answer.topic_personal
Edits_Non_Politically$family_current_life <- Edits_Non_Politically$Answer.topic_personal
Edits_Non_Politically$character <- Edits_Non_Politically$Answer.topic_personal
Edits_Non_Politically$early_life<- Edits_Non_Politically$Answer.topic_personal
Edits_Non_Politically$achievements_awards <- Edits_Non_Politically$Answer.topic_personal
Edits_Non_Politically$activities_memberships <- Edits_Non_Politically$Answer.topic_personal


Edits_Non_Politically$religion <- stringr::str_replace_all(Edits_Non_Politically$religion, "(.*)religion(.*)", "1")
Edits_Non_Politically$religion <- stringr::str_replace_all(Edits_Non_Politically$religion, "(.*)[[:alpha:]](.*)", "0")

Edits_Non_Politically$financial_earnings <- stringr::str_replace_all(Edits_Non_Politically$financial_earnings, "(.*)financial_earnings(.*)", "1")
Edits_Non_Politically$financial_earnings <- stringr::str_replace_all(Edits_Non_Politically$financial_earnings, "(.*)[[:alpha:]](.*)", "0")

Edits_Non_Politically$family_current_life <- stringr::str_replace_all(Edits_Non_Politically$family_current_life, "(.*)family_current_life(.*)", "1")
Edits_Non_Politically$family_current_life <- stringr::str_replace_all(Edits_Non_Politically$family_current_life, "(.*)[[:alpha:]](.*)", "0")

Edits_Non_Politically$character <- stringr::str_replace_all(Edits_Non_Politically$character, "(.*)character(.*)", "1")
Edits_Non_Politically$character <- stringr::str_replace_all(Edits_Non_Politically$character, "(.*)[[:alpha:]](.*)", "0")

Edits_Non_Politically$early_life <- stringr::str_replace_all(Edits_Non_Politically$early_life, "(.*)early_life(.*)", "1")
Edits_Non_Politically$early_life <- stringr::str_replace_all(Edits_Non_Politically$early_life, "(.*)[[:alpha:]](.*)", "0")

Edits_Non_Politically$achievements_awards <- stringr::str_replace_all(Edits_Non_Politically$achievements_awards, "(.*)achievements_awards(.*)", "1")
Edits_Non_Politically$achievements_awards <- stringr::str_replace_all(Edits_Non_Politically$achievements_awards, "(.*)[[:alpha:]](.*)", "0")

Edits_Non_Politically$activities_memberships <- stringr::str_replace_all(Edits_Non_Politically$activities_memberships, "(.*)activities_memberships(.*)", "1")
Edits_Non_Politically$activities_memberships <- stringr::str_replace_all(Edits_Non_Politically$activities_memberships, "(.*)[[:alpha:]](.*)", "0")


# Lets make a pie chart

#  For Non-politically 
table(Edits_Non_Politically$religion == 1)
table(Edits_Non_Politically$financial_earnings == 1)
table(Edits_Non_Politically$family_current_life == 1)
table(Edits_Non_Politically$character == 1)
table(Edits_Non_Politically$early_life == 1)
table(Edits_Non_Politically$achievements_awards == 1)
table(Edits_Non_Politically$activities_memberships == 1)

Label_Names =  c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")
Entries_Per_Category_Non_Political = c(19, 4, 120, 37,109, 11, 34)


Pie_Data_TopicPersonal_Non_Politically <- data.frame (Category = Label_Names, NumberOfEntries = Entries_Per_Category_Non_Political)

pie(Pie_Data_TopicPersonal_Non_Politically$NumberOfEntries , border="white", col=myPalette, labels = Label_Names ) 


# Same For Politically 

table(Edits_Politically$Answer.topic_personal)
Edits_Politically$religion <- Edits_Politically$Answer.topic_personal
Edits_Politically$financial_earnings  <- Edits_Politically$Answer.topic_personal
Edits_Politically$family_current_life <- Edits_Politically$Answer.topic_personal
Edits_Politically$character <- Edits_Politically$Answer.topic_personal
Edits_Politically$early_life<- Edits_Politically$Answer.topic_personal
Edits_Politically$achievements_awards <- Edits_Politically$Answer.topic_personal
Edits_Politically$activities_memberships <- Edits_Politically$Answer.topic_personal

Edits_Politically$religion <- stringr::str_replace_all(Edits_Politically$religion, "(.*)religion(.*)", "1")
Edits_Politically$religion <- stringr::str_replace_all(Edits_Politically$religion, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$financial_earnings <- stringr::str_replace_all(Edits_Politically$financial_earnings, "(.*)financial_earnings(.*)", "1")
Edits_Politically$financial_earnings <- stringr::str_replace_all(Edits_Politically$financial_earnings, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$family_current_life <- stringr::str_replace_all(Edits_Politically$family_current_life, "(.*)family_current_life(.*)", "1")
Edits_Politically$family_current_life <- stringr::str_replace_all(Edits_Politically$family_current_life, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$character <- stringr::str_replace_all(Edits_Politically$character, "(.*)character(.*)", "1")
Edits_Politically$character <- stringr::str_replace_all(Edits_Politically$character, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$early_life <- stringr::str_replace_all(Edits_Politically$early_life, "(.*)early_life(.*)", "1")
Edits_Politically$early_life <- stringr::str_replace_all(Edits_Politically$early_life, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$achievements_awards <- stringr::str_replace_all(Edits_Politically$achievements_awards, "(.*)achievements_awards(.*)", "1")
Edits_Politically$achievements_awards <- stringr::str_replace_all(Edits_Politically$achievements_awards, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$activities_memberships <- stringr::str_replace_all(Edits_Politically$activities_memberships, "(.*)activities_memberships(.*)", "1")
Edits_Politically$activities_memberships <- stringr::str_replace_all(Edits_Politically$activities_memberships, "(.*)[[:alpha:]](.*)", "0")



table(Edits_Politically$religion == 1)
table(Edits_Politically$financial_earnings == 1)
table(Edits_Politically$family_current_life == 1)
table(Edits_Politically$character == 1)
table(Edits_Politically$early_life == 1)
table(Edits_Politically$achievements_awards == 1)
table(Edits_Politically$activities_memberships == 1)

Label_Names =  c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")
Entries_Per_Category_Political = c(44, 76, 152, 212, 128, 78, 73)




Pie_Data_TopicPersonal_Politically <- data.frame (Category = Label_Names, NumberOfEntries = Entries_Per_Category_Political)

pie(Pie_Data_TopicPersonal_Politically$NumberOfEntries , border="white", col=myPalette, labels = Label_Names ) 



# Better way to visualize the last two Pie Chart for the distribution of subtopics of the topic personal:
# -> double bar plot! 

data_double_bar <- rbind(c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships",  "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"),
                         c(44/763, 76/763, 152/763, 212/763, 128/763, 78/763, 73/763, 19/334, 4/334, 120/334, 37/334,109/334, 11/334, 34/334), 
                         c("Political","Political","Political","Political","Political","Political","Political", "Non-Political","Non-Political","Non-Political","Non-Political","Non-Political","Non-Political","Non-Political"))

data_double_bar <- as.data.frame(data_double_bar)

library(data.table) 
#Transpose 
t_data_double_bar <- transpose(data_double_bar)


ggplot(t_data_double_bar, aes(V1, V2, fill=V3)) + 
  geom_bar(stat="identity",position="dodge", sort=F) + 
  scale_fill_brewer(palette="Set1") + 
  labs(x = "x",y="value") + 
  guides(fill=guide_legend(title="version"))
dev.off()



# Double bar plot
ggplot(t_data_double_bar, aes(factor(V1), V2, fill = V3)) + 
  geom_bar(stat="identity", position = "dodge", color = "white") + 
  scale_fill_brewer(palette = "Set1")


# Those Graphs for difference in political and non-political edits are all not great
# Let reduce complexity to a simple table showing the differences



