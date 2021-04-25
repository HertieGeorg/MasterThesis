
# Figures and Regression-Tables showing the Differences between Groups of Edits 
# (political versus maintenance, beneficial versus harmful)


#Output: 

# 1. 4 Bar-Plots showing difference between maitenance and political edits 
  # a. Differences between Personal Life, Career, Political Positions, Other 
  # b. within differences in Personal Life
  # c. within differences in Career
  # d. within differences in Policical positions 
  # -> arrange bar-plots into square 

# 2. 4 Bar-Plots showing differnece between beneficial political edits and harmful political edits 
  # a. Differences between Personal Life, Career, Political Positions, Other 
  # b. within differences in Personal Life
  # c. within differences in Career
  # d. within differences in Policical positions 
  # -> arrange bar-plots into square 


# Packages: 
library(RColorBrewer)


# Load: "Inside_Congress_Edits_Politically" from Folder "OutPut_Data" 
# Load: "Inside_Congress_Edits_Politically_PositiveNegative" from Folder "OutPut_Data" 


#-------------------------------------------------------------------------------------------------------------------
#---------------------- 1. Comparison between political edits and maintenance edits --------------------------------
#-------------------------------------------------------------------------------------------------------------------


# Devide dataset into politically and non-politically 
Edits_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)
Edits_Non_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 0)

# a. Differences between Personal Life, Career, Political Positions, Other 

# Get number of entires for every topic category 
table(is.na(Edits_Politically$Answer.topic_personal))
table(is.na(Edits_Politically$Answer.topic_career))
table(is.na(Edits_Politically$Answer.topic_views))
table(is.na(Edits_Politically$Answer.topic_other))

sum(520, 1165, 466, 475) #2626
Distribution_Topics_Politically <- data.frame (Topic = c("Personal", "Career","Views","Other"), Political = c(520/2626, 1165/2626, 466/2626, 475/2626))

#  For Non-politically 
table(is.na(Edits_Non_Politically$Answer.topic_personal))
table(is.na(Edits_Non_Politically$Answer.topic_career))
table(is.na(Edits_Non_Politically$Answer.topic_views))
table(is.na(Edits_Non_Politically$Answer.topic_other))

sum(281, 766, 119, 257) #1423
Distribution_Topics_Non_Politically <- data.frame(Topic = c("Personal", "Career","Views","Other"), Maintenance  = c(281/1423, 766/1423, 119/1423, 257/1423))

Distribution_Topics_Congress_Edits <- left_join(Distribution_Topics_Politically , Distribution_Topics_Non_Politically   , by ="Topic")



#Bar-Plot: Political vs. Maintenance (a)

dat9 <- data.frame(
  Response = factor(c("Political","Political","Political","Political","Political","Political","Political", "Non-Political","Non-Political","Non-Political","Non-Political","Non-Political","Non-Political","Non-Political")),
  Category = factor( c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships",  "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"),
                     levels=c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")),
  percentage = c(44/763, 76/763, 152/763, 212/763, 128/763, 78/763, 73/763, 19/334, 4/334, 120/334, 37/334,109/334, 11/334, 34/334))



Bar_Plot_1a <- ggplot(data=dat9, aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1")
Bar_Plot_1a 


















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





# Double Bar-Plot: Political vs. Maintenance -> Within-Category Personal

dat9 <- data.frame(
  Response = factor(c("Political","Political","Political","Political","Political","Political","Political", "Non-Political","Non-Political","Non-Political","Non-Political","Non-Political","Non-Political","Non-Political")),
  Category = factor( c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships",  "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"),
                     levels=c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")),
  percentage = c(44/763, 76/763, 152/763, 212/763, 128/763, 78/763, 73/763, 19/334, 4/334, 120/334, 37/334,109/334, 11/334, 34/334))


plot_nice <- ggplot(data=dat9, aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1")














# ------------------------------------------ Ideenspeicher --------------------------------------------------------


# Political vs. Maintenance


  # Pie-Charts:

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
pie(Pie_Data_Topics_Politically$NumberOfEntries , border="white",  labels = c("Personal", "Career","Views","Other") ) 
#Pie Non_Politically
pie(Pie_Data_Topics_Non_Politically$NumberOfEntries , border="white",  labels = c("Personal", "Career","Views","Other") ) 





  # Waffle-Charts:


#library(waffle)
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




# Differences within Category Personal:

  # Pie chart:

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
pie(Pie_Data_TopicPersonal_Non_Politically$NumberOfEntries , border="white",  labels = Label_Names ) 


# Political Edits
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

pie(Pie_Data_TopicPersonal_Politically$NumberOfEntries , border="white",  labels = Label_Names ) 



