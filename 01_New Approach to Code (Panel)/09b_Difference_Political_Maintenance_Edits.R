
install.packages("data.table")
install.packages("dplyr")
install.packages("formattable")
install.packages("tidyr")

#Load the libraries
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(stargazer)
library(RColorBrewer)

# Figures and Regression-Tables showing the Differences between Groups of Edits 
# (political versus maintenance, beneficial versus harmful)


#Output: 

# 1. 4 Bar-Plots showing difference between maitenance and political edits 
# 1a. Differences between Personal Life, Career, Political Positions, Other 
# 1b. within differences in Personal Life
# 1c. within differences in Career
# 1d. within differences in Policical positions 
# 1e. within differences in category "others"
# + Simple Regression Models that show difference in mean


# 2. 4 Bar-Plots showing differnece between beneficial political edits and harmful political edits 
# 2a. Differences between Personal Life, Career, Political Positions, Other 
# 2b. within differences in Personal Life
# 2c. within differences in Career
# 2d. within differences in Policical positions 
# 2e. within differences in category "others"
# + Simple Regression Models that show difference in mean


# Load: "Inside_Congress_Edits_Politically" from Folder "OutPut_Data" 
# Load: "Inside_Congress_Edits_Politically_PositiveNegative" from Folder "OutPut_Data" 



# ------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#---------------------- 1. Comparison between political edits and maintenance edits --------------------------------
#-------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# Devide dataset into politically and non-politically 
Edits_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1) #1373
Edits_Non_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 0) #1075


# ----------------------------------------------------------------------------------------------------------------------------
# 1a. Differences between Personal Life, Career, Political Positions, Other 
# ----------------------------------------------------------------------------------------------------------------------------

# Get number of entires for every topic category 
table(is.na(Edits_Politically$Answer.topic_personal))
table(is.na(Edits_Politically$Answer.topic_career))
table(is.na(Edits_Politically$Answer.topic_views))
table(is.na(Edits_Politically$Answer.topic_other))

sum(489, 1096, 544, 458) #2587
Distribution_Topics_Politically <- data.frame (Topic = c("Personal", "Career","Views","Other"), Political = c(489/2587, 1096/2587, 544/2587, 458/2587))

#  For Non-politically 
table(is.na(Edits_Non_Politically$Answer.topic_personal))
table(is.na(Edits_Non_Politically$Answer.topic_career))
table(is.na(Edits_Non_Politically$Answer.topic_views))
table(is.na(Edits_Non_Politically$Answer.topic_other))

sum(260, 727, 113, 244) #1344
Distribution_Topics_Non_Politically <- data.frame(Topic = c("Personal", "Career","Views","Other"), Maintenance  = c(260/1344, 727/1344, 113/1344, 244/1344))

Distribution_Topics_Congress_Edits <- left_join(Distribution_Topics_Politically , Distribution_Topics_Non_Politically   , by ="Topic")


# (1a) Bar-Plot: Political vs. Maintenance in BROAD TOPICS

data_1a_Barplot <- data.frame(
  Response = factor(c("Political","Political","Political","Political", "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance")),
  Category = factor( c("Personal", "Career","Views","Other", "Personal", "Career","Views","Other"),
                     levels=c("Personal", "Career","Views","Other")),
  percentage = c(489/2587, 1096/2587, 544/2587, 458/2587, 260/1344, 727/1344, 113/1344, 244/1344))

Figure_1a_Barplot <- ggplot(data=data_1a_Barplot, aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Political versus Maintenance Edits: Broad Categories") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.8, 0.85))


#    Using a Linear Model 
Inside_Congress_Edits_Politically$Answer.topic_personal_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically$Answer.topic_personal)) {
  if (is.na(Inside_Congress_Edits_Politically$Answer.topic_personal[i]) == F) {
    Inside_Congress_Edits_Politically$Answer.topic_personal_dummy[i] = 1
  }
}

Inside_Congress_Edits_Politically$Answer.topic_career_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically$Answer.topic_career)) {
  if (is.na(Inside_Congress_Edits_Politically$Answer.topic_career[i]) == F) {
    Inside_Congress_Edits_Politically$Answer.topic_career_dummy[i] = 1
  }
}

Inside_Congress_Edits_Politically$Answer.topic_views_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically$Answer.topic_views)) {
  if (is.na(Inside_Congress_Edits_Politically$Answer.topic_views[i]) == F) {
    Inside_Congress_Edits_Politically$Answer.topic_views_dummy[i] = 1
  }
}

Inside_Congress_Edits_Politically$Answer.topic_other_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically$Answer.topic_other)) {
  if (is.na(Inside_Congress_Edits_Politically$Answer.topic_other[i]) == F) {
    Inside_Congress_Edits_Politically$Answer.topic_other_dummy[i] = 1
  }
}

linear_model1 <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model2 <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model3 <- lm(Inside_Congress_Edits_Politically$Answer.topic_views_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model4 <- lm(Inside_Congress_Edits_Politically$Answer.topic_other_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)

stargazer(linear_model1 , linear_model2, linear_model3 ,linear_model4,
          title="Table (1a). Difference between political edits and maintenance edits in broad categories",
          type = "text", style = "default", out="Table(1a)NEW Diff_Topics_Political_Maintenance.html")



# ----------------------------------------------------------------------------------------------------------------------------
# 1b. Differences within PERSONAL: "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"
# ----------------------------------------------------------------------------------------------------------------------------


# Politically 
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

sum(43, 73, 143, 200, 119, 75, 70) #/723
Personal_Subcategories_Pol = c(43/723, 73/723, 143/723, 200/723, 119/723, 75/723, 70/723)

Label_Names =  c("Personal:Religion", "Personal:Financial Earnings","Personal:Family/Current Life","Personal:Character", "Personal:Early Life", "Personal:Achievements/Awards", "Personal:Activites/Memberships")
Distribution_TopicPERSONAL_Politically <- data.frame(Topic = Label_Names,  Political  = Personal_Subcategories_Pol)


# Non Politically
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

table(Edits_Non_Politically$religion == 1)
table(Edits_Non_Politically$financial_earnings == 1)
table(Edits_Non_Politically$family_current_life == 1)
table(Edits_Non_Politically$character == 1)
table(Edits_Non_Politically$early_life == 1)
table(Edits_Non_Politically$achievements_awards == 1)
table(Edits_Non_Politically$activities_memberships == 1)

sum(18, 4, 110, 37,102, 10, 29) #/310
Personal_Subcategories_NonPol = c(18/310, 4/310, 110/310, 37/310, 102/310, 10/310, 29/310)

Distribution_TopicPERSONAL_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Personal_Subcategories_NonPol)

# Join political and non-political
Distribution_TopicsPersonal_Congress_Edits <- left_join(Distribution_TopicPERSONAL_Politically  , Distribution_TopicPERSONAL_Non_Politically   , by ="Topic")



# (1b) Bar-Plot: Political vs. Maintenance in PERSONAL 

data_BarPlot_1b <- data.frame(Response = factor(c("Political","Political","Political","Political","Political","Political","Political", "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance", "Maintenance",  "Maintenance",  "Maintenance")),
                              Category = factor( c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships", "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"),
                                                 levels=  c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")),
                              percentage = c(43/723, 73/723, 143/723, 200/723, 119/723, 75/723, 70/723, 18/310, 4/310, 110/310, 37/310, 102/310, 10/310, 29/310 ))

BarPlot_1b <- ggplot(data_BarPlot_1b , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Political versus Maintenance Edits in Category: Personal") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.85, 0.85))



# Using a Linear Model for basically the same

Inside_Congress_Edits_Politically$Answer.topic_personal_religion_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_religion_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_religion_dummy, "(.*)religion(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_religion_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_religion_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_personal_financial_earnings_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_financial_earnings_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_financial_earnings_dummy, "(.*)financial_earnings(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_financial_earnings_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_financial_earnings_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_personal_family_current_life_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_family_current_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_family_current_life_dummy, "(.*)family_current_life(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_family_current_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_family_current_life_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_personal_character_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_character_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_character_dummy, "(.*)character(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_character_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_character_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_personal_early_life_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_early_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_early_life_dummy, "(.*)early_life(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_early_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_early_life_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_personal_achievements_awards_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_achievements_awards_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_achievements_awards_dummy, "(.*)achievements_awards(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_achievements_awards_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_achievements_awards_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_personal_activities_memberships_dummy <- Inside_Congress_Edits_Politically$Answer.topic_personal
Inside_Congress_Edits_Politically$Answer.topic_personal_activities_memberships_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_activities_memberships_dummy, "(.*)activities_memberships(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_personal_activities_memberships_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_personal_activities_memberships_dummy, "(.*)[[:alpha:]](.*)", "0")



linear_model1b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_religion_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model2b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_financial_earnings_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model3b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_family_current_life_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model4b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_character_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model5b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_early_life_dummy~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model6b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_achievements_awards_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model7b <- lm(Inside_Congress_Edits_Politically$Answer.topic_personal_activities_memberships_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)

# Regression Models Output

stargazer(linear_model1b , linear_model2b , linear_model3b , linear_model4b , linear_model5b , linear_model6b , linear_model7b , 
          title="Table (1b). Difference between political edits and maintenance edits: Personal",
          type = "text", style = "default", out="Table(1b)NEWDiff_Personal.html")




# ----------------------------------------------------------------------------------------------------------------------------
# 1c. Differences within Career: "Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service" usw.
# -----------------------------------------------------------------------------------------------------------------------------


# For better insights the column should be entangled in different columns 

table(Edits_Politically$Answer.topic_career)


# Politically 
Edits_Politically$election_campaign <- Edits_Politically$Answer.topic_career
Edits_Politically$scandals_political  <- Edits_Politically$Answer.topic_career
Edits_Politically$tenure_activity  <- Edits_Politically$Answer.topic_career
Edits_Politically$legislation <- Edits_Politically$Answer.topic_career
Edits_Politically$district_service <- Edits_Politically$Answer.topic_career
Edits_Politically$early_political_career <- Edits_Politically$Answer.topic_career
Edits_Politically$congress_offices <- Edits_Politically$Answer.topic_career
Edits_Politically$early_career <- Edits_Politically$Answer.topic_career


Edits_Politically$election_campaign <- stringr::str_replace_all(Edits_Politically$election_campaign, "(.*)election_campaign(.*)", "1")
Edits_Politically$election_campaign <- stringr::str_replace_all(Edits_Politically$election_campaign, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$scandals_political  <- stringr::str_replace_all(Edits_Politically$scandals_political , "(.*)scandals_political(.*)", "1")
Edits_Politically$scandals_political <- stringr::str_replace_all(Edits_Politically$scandals_political, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$tenure_activity <- stringr::str_replace_all(Edits_Politically$tenure_activity, "(.*)tenure_activity(.*)", "1")
Edits_Politically$tenure_activity <- stringr::str_replace_all(Edits_Politically$tenure_activity, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$legislation <- stringr::str_replace_all(Edits_Politically$legislation, "(.*)legislation(.*)", "1")
Edits_Politically$legislation <- stringr::str_replace_all(Edits_Politically$legislation, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$district_service <- stringr::str_replace_all(Edits_Politically$district_service, "(.*)district_service(.*)", "1")
Edits_Politically$district_service <- stringr::str_replace_all(Edits_Politically$district_service, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$achievements_awards <- stringr::str_replace_all(Edits_Politically$early_political_career, "(.*)early_political_career(.*)", "1")
Edits_Politically$early_political_career <- stringr::str_replace_all(Edits_Politically$early_political_career, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$congress_offices <- stringr::str_replace_all(Edits_Politically$congress_offices, "(.*)congress_offices(.*)", "1")
Edits_Politically$congress_offices <- stringr::str_replace_all(Edits_Politically$congress_offices, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$early_career <- stringr::str_replace_all(Edits_Politically$early_career, "(.*)early_career(.*)", "1")
Edits_Politically$early_career <- stringr::str_replace_all(Edits_Politically$early_career, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Politically$election_campaign == 1)
table(Edits_Politically$scandals_political == 1)
table(Edits_Politically$tenure_activity == 1)
table(Edits_Politically$legislation == 1)
table(Edits_Politically$district_service == 1)
table(Edits_Politically$early_political_career == 1)
table(Edits_Politically$congress_offices == 1)
table(Edits_Politically$early_career == 1)

sum(218, 297, 156, 387, 138, 0, 366, 121) #/1683
Career_Subcategories_Pol = c(218/1683, 297/1683, 156/1683, 387/1683, 138/1683, 0/1683, 366/1683, 121/1683) 


Label_Names =  c("Career: Congress election campaign", "Career: Political scandals/controversis","Career: Activities during tenure","Career: Legislation", "Career: District service", 
                 "Career: Early political career", "Career: Offices/memberships in Congress", "Career: Early non-political career")
Distribution_TopicCareer_Politically <- data.frame(Topic = Label_Names,  Political  = Career_Subcategories_Pol)



# Non Politically
Edits_Non_Politically$election_campaign <- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$scandals_political  <- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$tenure_activity <- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$legislation <- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$district_service<- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$early_political_career <- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$congress_offices <- Edits_Non_Politically$Answer.topic_career
Edits_Non_Politically$early_career <- Edits_Non_Politically$Answer.topic_career

Edits_Non_Politically$election_campaign <- stringr::str_replace_all(Edits_Non_Politically$election_campaign, "(.*)election_campaign(.*)", "1")
Edits_Non_Politically$election_campaign <- stringr::str_replace_all(Edits_Non_Politically$election_campaign, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$scandals_political <- stringr::str_replace_all(Edits_Non_Politically$scandals_political, "(.*)scandals_political(.*)", "1")
Edits_Non_Politically$scandals_political <- stringr::str_replace_all(Edits_Non_Politically$scandals_political, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$tenure_activity <- stringr::str_replace_all(Edits_Non_Politically$tenure_activity, "(.*)tenure_activity(.*)", "1")
Edits_Non_Politically$tenure_activity <- stringr::str_replace_all(Edits_Non_Politically$tenure_activity, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$legislation <- stringr::str_replace_all(Edits_Non_Politically$legislation, "(.*)legislation(.*)", "1")
Edits_Non_Politically$legislation <- stringr::str_replace_all(Edits_Non_Politically$legislation, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$district_service <- stringr::str_replace_all(Edits_Non_Politically$district_service, "(.*)district_service(.*)", "1")
Edits_Non_Politically$district_service <- stringr::str_replace_all(Edits_Non_Politically$district_service, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$early_political_career <- stringr::str_replace_all(Edits_Non_Politically$early_political_career, "(.*)early_political_career(.*)", "1")
Edits_Non_Politically$early_political_career <- stringr::str_replace_all(Edits_Non_Politically$early_political_career, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$congress_offices <- stringr::str_replace_all(Edits_Non_Politically$congress_offices, "(.*)congress_offices(.*)", "1")
Edits_Non_Politically$congress_offices <- stringr::str_replace_all(Edits_Non_Politically$congress_offices, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$early_career <- stringr::str_replace_all(Edits_Non_Politically$early_career, "(.*)early_career(.*)", "1")
Edits_Non_Politically$early_career <- stringr::str_replace_all(Edits_Non_Politically$early_career, "(.*)[[:alpha:]](.*)", "0")


table(Edits_Non_Politically$election_campaign == 1)
table(Edits_Non_Politically$scandals_political == 1)
table(Edits_Non_Politically$tenure_activity == 1)
table(Edits_Non_Politically$legislation == 1)
table(Edits_Non_Politically$district_service == 1)
table(Edits_Non_Politically$early_political_career == 1)
table(Edits_Non_Politically$congress_offices == 1)
table(Edits_Non_Politically$early_career == 1)

sum(95, 8, 18, 77, 62, 23, 468, 46) #/797
Career_Subcategories_NonPol = c(95/797, 8/797, 18/797, 77/797, 62/797, 23/797, 468/797, 46/797)

Distribution_TopicCareer_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Career_Subcategories_NonPol)

# Join political and non-political

Distribution_TopicsCareer_Congress_Edits <- left_join(Distribution_TopicCareer_Politically  , Distribution_TopicCareer_Non_Politically   , by ="Topic")




# (1c) Bar-Plot: Political vs. Maintenance in CAREER

data_BarPlot_1c <- data.frame(Response = factor(c("Political","Political","Political","Political","Political","Political","Political","Political", "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance", "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance")),
                              Category = factor( c("Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service", 
                                                   "Early political career", "Offices/memberships in Congress", "Early non-political career", "Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service", 
                                                   "Early political career", "Offices/memberships in Congress", "Early non-political career"),
                                                 levels=  c("Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service", 
                                                            "Early political career", "Offices/memberships in Congress", "Early non-political career")),
                              percentage = c( 218/1683, 297/1683, 156/1683, 387/1683, 138/1683, 0/1683, 366/1683, 121/1683, 95/797, 8/797, 18/797, 77/797, 62/797, 23/797, 468/797, 46/797))


BarPlot_1c <- ggplot(data_BarPlot_1c , aes(x=Category, y=percentage, fill=Response,)) +
    geom_bar(stat="identity", position=position_dodge(), colour="white") + 
    scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
    ggtitle("Distribution of Political versus Maintenance Edits in Category: Career") +
    xlab("Category") + 
    ylab("Distribution over Categories") +
    scale_x_discrete(guide = guide_axis(angle = 70))  +
    theme(legend.position = c(0.15, 0.85))



# Using a Linear Model for basically the same


Inside_Congress_Edits_Politically$Answer.topic_career_election_campaign_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_election_campaign_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_election_campaign_dummy, "(.*)election_campaign(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_election_campaign_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_election_campaign_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_scandals_political_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_scandals_political_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_scandals_political_dummy, "(.*)scandals_political(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_scandals_political_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_scandals_political_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_tenure_activity_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_tenure_activity_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_tenure_activity_dummy, "(.*)tenure_activity(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_tenure_activity_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_tenure_activity_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_legislation_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_legislation_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_legislation_dummy, "(.*)legislation(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_legislation_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_legislation_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_district_service_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_district_service_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_district_service_dummy, "(.*)district_service(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_district_service_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_district_service_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_early_political_career_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_early_political_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_early_political_career_dummy, "(.*)early_political_career(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_early_political_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_early_political_career_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_congress_offices_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_congress_offices_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_congress_offices_dummy, "(.*)congress_offices(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_congress_offices_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_congress_offices_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_career_early_career_dummy <- Inside_Congress_Edits_Politically$Answer.topic_career
Inside_Congress_Edits_Politically$Answer.topic_career_early_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_early_career_dummy, "(.*)early_career(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_career_early_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_career_early_career_dummy, "(.*)[[:alpha:]](.*)", "0")




linear_model1c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_election_campaign_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model2c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_scandals_political_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model3c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_tenure_activity_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model4c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_legislation_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model5c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_district_service_dummy~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model6c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_early_political_career_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model7c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_congress_offices_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model8c <- lm(Inside_Congress_Edits_Politically$Answer.topic_career_early_career_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)

dim(Inside_Congress_Edits_Politically$politically_motivated)


# Regression Models Output

stargazer(linear_model1c , linear_model2c , linear_model3c , linear_model4c , linear_model5c , linear_model6c , linear_model7c , linear_model8c , 
          title="Table (1c). Difference between political edits and maintenance edits: Career",  covariate.labels = c("1",  "2",  "3" , "4", "5", "6", "7", "8"),
          type = "text", style = "default", out="Table(1c)NEWDiff_Career.html")





# ----------------------------------------------------------------------------------------------------------------------------
# 1d. Differences within VIEWS: "Issue-specific views", "Support of other politicians","Ideology","Controversial statements"
# -----------------------------------------------------------------------------------------------------------------------------



# For better insights the column should be entangled in different columns 

table(Edits_Politically$Answer.topic_views)

# issue_views
# support_politicians
# ideology
# statements_controversial 


# Politically 
Edits_Politically$issue_views <- Edits_Politically$Answer.topic_views
Edits_Politically$support_politicians  <- Edits_Politically$Answer.topic_views
Edits_Politically$ideology <- Edits_Politically$Answer.topic_views
Edits_Politically$statements_controversial <- Edits_Politically$Answer.topic_views


Edits_Politically$issue_views <- stringr::str_replace_all(Edits_Politically$issue_views, "(.*)issue_views(.*)", "1")
Edits_Politically$issue_views <- stringr::str_replace_all(Edits_Politically$issue_views, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$support_politicians  <- stringr::str_replace_all(Edits_Politically$support_politicians , "(.*)support_politicians(.*)", "1")
Edits_Politically$support_politicians <- stringr::str_replace_all(Edits_Politically$support_politicians, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$ideology<- stringr::str_replace_all(Edits_Politically$ideology, "(.*)ideology(.*)", "1")
Edits_Politically$ideology<- stringr::str_replace_all(Edits_Politically$ideology, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$statements_controversial <- stringr::str_replace_all(Edits_Politically$statements_controversial, "(.*)statements_controversial(.*)", "1")
Edits_Politically$statements_controversial <- stringr::str_replace_all(Edits_Politically$statements_controversial, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Politically$issue_views == 1)
table(Edits_Politically$support_politicians == 1)
table(Edits_Politically$ideology == 1)
table(Edits_Politically$statements_controversial == 1)


sum(346, 195, 152, 157) #/850
Views_Subcategories_Pol = c(346/850, 195/850, 152/850, 157/850)


Label_Names =  c("Views: Issue-specific views", "Views: Support of other politicians","Views: Ideology","Views: Controversial statements")
Distribution_TopicViews_Politically <- data.frame(Topic = Label_Names,  Political  = Views_Subcategories_Pol)



# Non Politically
Edits_Non_Politically$issue_views <- Edits_Non_Politically$Answer.topic_views
Edits_Non_Politically$support_politicians  <- Edits_Non_Politically$Answer.topic_views
Edits_Non_Politically$ideology <- Edits_Non_Politically$Answer.topic_views
Edits_Non_Politically$statements_controversial <- Edits_Non_Politically$Answer.topic_views


Edits_Non_Politically$issue_views <- stringr::str_replace_all(Edits_Non_Politically$issue_views, "(.*)issue_views(.*)", "1")
Edits_Non_Politically$issue_views <- stringr::str_replace_all(Edits_Non_Politically$issue_views, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$support_politicians <- stringr::str_replace_all(Edits_Non_Politically$support_politicians, "(.*)support_politicians(.*)", "1")
Edits_Non_Politically$support_politicians <- stringr::str_replace_all(Edits_Non_Politically$support_politicians, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$ideology <- stringr::str_replace_all(Edits_Non_Politically$ideology, "(.*)ideology(.*)", "1")
Edits_Non_Politically$ideology <- stringr::str_replace_all(Edits_Non_Politically$ideology, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$statements_controversial <- stringr::str_replace_all(Edits_Non_Politically$statements_controversial, "(.*)statements_controversial(.*)", "1")
Edits_Non_Politically$statements_controversial <- stringr::str_replace_all(Edits_Non_Politically$statements_controversial, "(.*)[[:alpha:]](.*)", "0")


table(Edits_Non_Politically$issue_views == 1)
table(Edits_Non_Politically$support_politicians == 1)
table(Edits_Non_Politically$ideology == 1)
table(Edits_Non_Politically$statements_controversial == 1)


sum(54, 45, 23, 9) #/131
Views_Subcategories_NonPol = c(54/131, 45/131, 23/131, 9/131)

Distribution_TopicViews_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Views_Subcategories_NonPol)

# Join political and non-political

Distribution_TopicsViews_Congress_Edits <- left_join(Distribution_TopicViews_Politically  , Distribution_TopicViews_Non_Politically   , by ="Topic")





# (1d) Bar-Plot: Political vs. Maintenance in VIEWS

data_BarPlot_1d <- data.frame(Response = factor(c("Political","Political","Political","Political", "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance")),
                              Category = factor( c("Issue-specific views", "Support of other politicians","Ideology","Controversial statements", "Issue-specific views", "Support of other politicians","Ideology","Controversial statements"),
                                                 levels=  c("Issue-specific views", "Support of other politicians","Ideology","Controversial statements")),
                              percentage = c(346/850, 195/850, 152/850, 157/850, 54/131, 45/131, 23/131, 9/131 ))


BarPlot_1d <- ggplot(data_BarPlot_1d , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Political versus Maintenance Edits in Category: Views") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.85, 0.85))




# Using a Linear Model for basically the same: Views 
Inside_Congress_Edits_Politically$Answer.topic_views_issue_views_dummy <- Inside_Congress_Edits_Politically$Answer.topic_views
Inside_Congress_Edits_Politically$Answer.topic_views_issue_views_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_issue_views_dummy, "(.*)issue_views(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_views_issue_views_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_issue_views_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_views_support_politicians_dummy <- Inside_Congress_Edits_Politically$Answer.topic_views
Inside_Congress_Edits_Politically$Answer.topic_views_support_politicians_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_support_politicians_dummy, "(.*)support_politicians(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_views_support_politicians_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_support_politicians_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_views_ideology_dummy <- Inside_Congress_Edits_Politically$Answer.topic_views
Inside_Congress_Edits_Politically$Answer.topic_views_ideology_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_ideology_dummy, "(.*)ideology(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_views_ideology_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_ideology_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_views_statements_controversial_dummy <- Inside_Congress_Edits_Politically$Answer.topic_views
Inside_Congress_Edits_Politically$Answer.topic_views_statements_controversial_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_statements_controversial_dummy, "(.*)statements_controversial(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_views_statements_controversial_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_views_statements_controversial_dummy, "(.*)[[:alpha:]](.*)", "0")

linear_model1d <- lm(Inside_Congress_Edits_Politically$Answer.topic_views_issue_views_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model2d <- lm(Inside_Congress_Edits_Politically$Answer.topic_views_support_politicians_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model3d <- lm(Inside_Congress_Edits_Politically$Answer.topic_views_ideology_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model4d <- lm(Inside_Congress_Edits_Politically$Answer.topic_views_statements_controversial_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)

# Regression Models Output
stargazer(linear_model1d , linear_model2d , linear_model3d , linear_model4d ,
          title="Table (1d). Difference between political edits and maintenance edits",
          type = "text", style = "default", out="Table(1d)NEWDiff_Views.html")




# ----------------------------------------------------------------------------------------------------------------------------
# 1e. Differences within OTHERs: "Other: External link", "Other: Publications","Other: References","Other: Categorization","Other: Other"
# -----------------------------------------------------------------------------------------------------------------------------



# For better insights the column should be entangled in different columns 

table(Edits_Politically$Answer.topic_other)
#external_link
#publications
#references
#categorization 
#other_other


# Politically 
Edits_Politically$external_link <- Edits_Politically$Answer.topic_other
Edits_Politically$publications  <- Edits_Politically$Answer.topic_other
Edits_Politically$references <- Edits_Politically$Answer.topic_other
Edits_Politically$categorization <- Edits_Politically$Answer.topic_other
Edits_Politically$other_other <- Edits_Politically$Answer.topic_other

Edits_Politically$external_link <- stringr::str_replace_all(Edits_Politically$external_link, "(.*)external_link(.*)", "1")
Edits_Politically$external_link <- stringr::str_replace_all(Edits_Politically$external_link, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$publications  <- stringr::str_replace_all(Edits_Politically$publications , "(.*)publications(.*)", "1")
Edits_Politically$publications <- stringr::str_replace_all(Edits_Politically$publications, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$references<- stringr::str_replace_all(Edits_Politically$references, "(.*)references(.*)", "1")
Edits_Politically$references<- stringr::str_replace_all(Edits_Politically$references, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$categorization <- stringr::str_replace_all(Edits_Politically$categorization, "(.*)categorization(.*)", "1")
Edits_Politically$categorization <- stringr::str_replace_all(Edits_Politically$categorization, "(.*)[[:alpha:]](.*)", "0")
Edits_Politically$other_other <- stringr::str_replace_all(Edits_Politically$other_other, "(.*)other_other(.*)", "1")
Edits_Politically$other_other <- stringr::str_replace_all(Edits_Politically$other_other, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Politically$external_link == 1)
table(Edits_Politically$publications == 1)
table(Edits_Politically$references == 1)
table(Edits_Politically$categorization == 1)
table(Edits_Politically$other_other == 1)

sum(153, 37, 326, 5, 33) #/554
Other_Subcategories_Pol = c(153/554, 37/554, 326/554, 5/554, 33/554)

Label_Names =  c("Other: External link", "Other: Publications","Other: References","Other: Categorization","Other: Other")
Distribution_TopicOther_Politically <- data.frame(Topic = Label_Names,  Political  = Other_Subcategories_Pol)

# Non Politically
Edits_Non_Politically$external_link <- Edits_Non_Politically$Answer.topic_other
Edits_Non_Politically$publications  <- Edits_Non_Politically$Answer.topic_other
Edits_Non_Politically$references <- Edits_Non_Politically$Answer.topic_other
Edits_Non_Politically$categorization <- Edits_Non_Politically$Answer.topic_other
Edits_Non_Politically$other_other <- Edits_Non_Politically$Answer.topic_other

Edits_Non_Politically$external_link <- stringr::str_replace_all(Edits_Non_Politically$external_link, "(.*)external_link(.*)", "1")
Edits_Non_Politically$external_link <- stringr::str_replace_all(Edits_Non_Politically$external_link, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$publications <- stringr::str_replace_all(Edits_Non_Politically$publications, "(.*)publications(.*)", "1")
Edits_Non_Politically$publications <- stringr::str_replace_all(Edits_Non_Politically$publications, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$references <- stringr::str_replace_all(Edits_Non_Politically$references, "(.*)references(.*)", "1")
Edits_Non_Politically$references <- stringr::str_replace_all(Edits_Non_Politically$references, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$categorization <- stringr::str_replace_all(Edits_Non_Politically$categorization, "(.*)categorization(.*)", "1")
Edits_Non_Politically$categorization <- stringr::str_replace_all(Edits_Non_Politically$categorization, "(.*)[[:alpha:]](.*)", "0")
Edits_Non_Politically$other_other <- stringr::str_replace_all(Edits_Non_Politically$other_other, "(.*)other_other(.*)", "1")
Edits_Non_Politically$other_other <- stringr::str_replace_all(Edits_Non_Politically$other_other, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Non_Politically$external_link == 1)
table(Edits_Non_Politically$publications == 1)
table(Edits_Non_Politically$references == 1)
table(Edits_Non_Politically$categorization == 1)
table(Edits_Non_Politically$other_other == 1)

sum(74, 7, 60, 15, 109) #/265
Other_Subcategories_NonPol = c(74/265, 7/265, 60/265, 15/265, 109/265)
Distribution_TopicOther_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Other_Subcategories_NonPol)

# Join political and non-political
Distribution_TopicsOther_Congress_Edits <- left_join(Distribution_TopicOther_Politically  , Distribution_TopicOther_Non_Politically   , by ="Topic")




# (1e) Bar-Plot: Political vs. Maintenance in VIEWS

data_BarPlot_1e <- data.frame(Response = factor(c("Political","Political","Political","Political","Political", "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance",  "Maintenance")),
                              Category = factor( c("External link", "Publications","References","Categorization","Other","External link", "Publications","References","Categorization","Other"),
                                                 levels=  c("External link", "Publications","References","Categorization","Other")),
                              percentage = c(153/554, 37/554, 326/554, 5/554, 33/554, 74/265, 7/265, 60/265, 15/265, 109/265))


BarPlot_1e <- ggplot(data_BarPlot_1e , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Political versus Maintenance Edits in Category: Other") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.85, 0.85))




# Using a Linear Model for basically the same: Other

Inside_Congress_Edits_Politically$Answer.topic_other_external_link_dummy <- Inside_Congress_Edits_Politically$Answer.topic_other
Inside_Congress_Edits_Politically$Answer.topic_other_external_link_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_external_link_dummy, "(.*)external_link(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_other_external_link_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_external_link_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_other_publications_dummy <- Inside_Congress_Edits_Politically$Answer.topic_other
Inside_Congress_Edits_Politically$Answer.topic_other_publications_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_publications_dummy, "(.*)publications(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_other_publications_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_publications_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_other_references_dummy <- Inside_Congress_Edits_Politically$Answer.topic_other
Inside_Congress_Edits_Politically$Answer.topic_other_references_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_references_dummy, "(.*)references(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_other_references_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_references_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_other_categorization_dummy <- Inside_Congress_Edits_Politically$Answer.topic_other
Inside_Congress_Edits_Politically$Answer.topic_other_categorization_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_categorization_dummy, "(.*)categorization(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_other_categorization_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_categorization_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically$Answer.topic_other_other_other_dummy <- Inside_Congress_Edits_Politically$Answer.topic_other
Inside_Congress_Edits_Politically$Answer.topic_other_other_other_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_other_other_dummy, "(.*)other_other(.*)", "1")
Inside_Congress_Edits_Politically$Answer.topic_other_other_other_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically$Answer.topic_other_other_other_dummy, "(.*)[[:alpha:]](.*)", "0")

linear_model1e <- lm(Inside_Congress_Edits_Politically$Answer.topic_other_external_link_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model2e <- lm(Inside_Congress_Edits_Politically$Answer.topic_other_publications_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model3e <- lm(Inside_Congress_Edits_Politically$Answer.topic_other_references_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model4e <- lm(Inside_Congress_Edits_Politically$Answer.topic_other_categorization_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)
linear_model5e <- lm(Inside_Congress_Edits_Politically$Answer.topic_other_other_other_dummy ~ Inside_Congress_Edits_Politically$politically_motivated)


# Regression Models Output

stargazer(linear_model1e , linear_model2e , linear_model3e , linear_model4e, linear_model5e,
          title="Table (1e). Difference between political edits and maintenance edits in category: Other",
          type = "text", style = "default", out="Table(1e)NEWDiff_Other.html")


# Save Inside_Congress_Edits_Politically with dummies that were create in this file
Inside_Congress_Edits_Politically_Dummies <- Inside_Congress_Edits_Politically
#save(Inside_Congress_Edits_Politically_Dummies, file = "Inside_Congress_Edits_Politically_Dummies.Rdata")



##----------------- Bring all together

# rbind Topics and Subtopics:Personal
Distribution_Topics_Political_vs_Maintenance <- rbind( Distribution_Topics_Congress_Edits, 
                                             Distribution_TopicsPersonal_Congress_Edits, 
                                             Distribution_TopicsCareer_Congress_Edits, 
                                             Distribution_TopicsViews_Congress_Edits,
                                             Distribution_TopicsOther_Congress_Edits )

# Transform into % 
Distribution_Topics_Political_vs_Maintenance <- Distribution_Topics_Political_vs_Maintenance %>% 
  mutate( Political_Percent= Political/1,Political_Percent=scales::percent(Political_Percent))  %>% 
  mutate( Maintenance_Percent=  Maintenance/1, Maintenance_Percent=scales::percent(Maintenance_Percent)) 
          
Distribution_Topics_Political_vs_Maintenance$Diffe <- Distribution_Topics_Political_vs_Maintenance$Political - Distribution_Topics_Political_vs_Maintenance$Maintenance 
Distribution_Topics_Political_vs_Maintenance$Difference <- Distribution_Topics_Political_vs_Maintenance$Diffe * 100

# Drop non-percent columns
Distribution_Topics_Political_vs_Maintenance <- Distribution_Topics_Political_vs_Maintenance %>% 
  dplyr::select(-c(Political, Maintenance, Diffe))

formattable(Distribution_Topics_Political_vs_Maintenance, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))






# ------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#----------------- 2. Comparison between beneficial political edits and harmful political edits --------------------
#-------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


# Divide dataset into beneficial political and harmful political
Edits_Beneficial <- Inside_Congress_Edits_Politically_PositiveNegative  %>% filter(politically_motivated_positive == 1)
Edits_Harmful <- Inside_Congress_Edits_Politically_PositiveNegative  %>% filter(politically_motivated_negative == 1)



# ----------------------------------------------------------------------------------------------------------------------------
# 2a. Differences between Personal Life, Career, Political Positions, Other 
# ----------------------------------------------------------------------------------------------------------------------------


# Clean Data: 
#    Drop all Neutral Edits

Inside_Congress_Edits_Politically_PositiveNegative$Summe <- Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive + Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_negative
Inside_Congress_Edits_Politically_PositiveNegative <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(Summe != 0)


# Beneficial 
table(is.na(Edits_Beneficial$Answer.topic_personal))
table(is.na(Edits_Beneficial$Answer.topic_career))
table(is.na(Edits_Beneficial$Answer.topic_views))
table(is.na(Edits_Beneficial$Answer.topic_other))

sum(746, 236, 689, 756) #/2427
# 746/2427, 236/2427, 689/2427, 756/2427

# Harmful
table(is.na(Edits_Harmful$Answer.topic_personal))
table(is.na(Edits_Harmful$Answer.topic_career))
table(is.na(Edits_Harmful$Answer.topic_views))
table(is.na(Edits_Harmful$Answer.topic_other))

sum(75, 30, 84, 94) #/283
# 75/283, 30/283, 84/283, 94/283


# (2a) Bar-Plot: Political vs. Maintenance in BROAD TOPICS

data_BarPlot_2a <- data.frame(
  Response = factor(c("Beneficial Political", "Beneficial Political", "Beneficial Political", "Beneficial Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political")),
  Category = factor( c("Personal", "Career","Views","Other", "Personal", "Career","Views","Other"),
                     levels=c("Personal", "Career","Views","Other")),
  percentage = c( 746/2427, 236/2427, 689/2427, 756/2427, 75/283, 30/283, 84/283, 94/283 ))

BarPlot_2a <- ggplot(data= data_BarPlot_2a, aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Beneficial Political versus Harmful Political Edits") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.31, 0.9))


#    Using a Linear Model 
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal)) {
  if (is.na(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal[i]) == F) {
    Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_dummy[i] = 1
  }
}

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career)) {
  if (is.na(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career[i]) == F) {
    Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_dummy[i] = 1
  }
}

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views)) {
  if (is.na(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views[i]) == F) {
    Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_dummy[i] = 1
  }
}

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_dummy <- 0
for(i in 1:length(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other)) {
  if (is.na(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other[i]) == F) {
    Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_dummy[i] = 1
  }
}


linear_model1 <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model2 <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model3 <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model4 <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)


stargazer(linear_model1 , linear_model2, linear_model3 ,linear_model4,
          title="Table (2a). Difference between beneficial and harmfl political edits in broad categories",
          type = "text", style = "default", out="Table(2a)NEWDiff_Topics_Political_Maintenance.html")






# ----------------------------------------------------------------------------------------------------------------------------
# 2b. Differences within PERSONAL: "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"
# ----------------------------------------------------------------------------------------------------------------------------


# Politically 
Edits_Beneficial$religion <- Edits_Beneficial$Answer.topic_personal
Edits_Beneficial$financial_earnings  <- Edits_Beneficial$Answer.topic_personal
Edits_Beneficial$family_current_life <- Edits_Beneficial$Answer.topic_personal
Edits_Beneficial$character <- Edits_Beneficial$Answer.topic_personal
Edits_Beneficial$early_life<- Edits_Beneficial$Answer.topic_personal
Edits_Beneficial$achievements_awards <- Edits_Beneficial$Answer.topic_personal
Edits_Beneficial$activities_memberships <- Edits_Beneficial$Answer.topic_personal

Edits_Beneficial$religion <- stringr::str_replace_all(Edits_Beneficial$religion, "(.*)religion(.*)", "1")
Edits_Beneficial$religion <- stringr::str_replace_all(Edits_Beneficial$religion, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$financial_earnings <- stringr::str_replace_all(Edits_Beneficial$financial_earnings, "(.*)financial_earnings(.*)", "1")
Edits_Beneficial$financial_earnings <- stringr::str_replace_all(Edits_Beneficial$financial_earnings, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$family_current_life <- stringr::str_replace_all(Edits_Beneficial$family_current_life, "(.*)family_current_life(.*)", "1")
Edits_Beneficial$family_current_life <- stringr::str_replace_all(Edits_Beneficial$family_current_life, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$character <- stringr::str_replace_all(Edits_Beneficial$character, "(.*)character(.*)", "1")
Edits_Beneficial$character <- stringr::str_replace_all(Edits_Beneficial$character, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$early_life <- stringr::str_replace_all(Edits_Beneficial$early_life, "(.*)early_life(.*)", "1")
Edits_Beneficial$early_life <- stringr::str_replace_all(Edits_Beneficial$early_life, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$achievements_awards <- stringr::str_replace_all(Edits_Beneficial$achievements_awards, "(.*)achievements_awards(.*)", "1")
Edits_Beneficial$achievements_awards <- stringr::str_replace_all(Edits_Beneficial$achievements_awards, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$activities_memberships <- stringr::str_replace_all(Edits_Beneficial$activities_memberships, "(.*)activities_memberships(.*)", "1")
Edits_Beneficial$activities_memberships <- stringr::str_replace_all(Edits_Beneficial$activities_memberships, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Beneficial$religion == 1)
table(Edits_Beneficial$financial_earnings == 1)
table(Edits_Beneficial$family_current_life == 1)
table(Edits_Beneficial$character == 1)
table(Edits_Beneficial$early_life == 1)
table(Edits_Beneficial$achievements_awards == 1)
table(Edits_Beneficial$activities_memberships == 1)

sum(41, 65, 125, 171, 105, 70, 68) #/645
# 41/645, 65/645, 125/645, 171/645, 105/645, 70/645, 68/645



# Non Politically
Edits_Harmful$religion <- Edits_Harmful$Answer.topic_personal
Edits_Harmful$financial_earnings  <- Edits_Harmful$Answer.topic_personal
Edits_Harmful$family_current_life <- Edits_Harmful$Answer.topic_personal
Edits_Harmful$character <- Edits_Harmful$Answer.topic_personal
Edits_Harmful$early_life<- Edits_Harmful$Answer.topic_personal
Edits_Harmful$achievements_awards <- Edits_Harmful$Answer.topic_personal
Edits_Harmful$activities_memberships <- Edits_Harmful$Answer.topic_personal

Edits_Harmful$religion <- stringr::str_replace_all(Edits_Harmful$religion, "(.*)religion(.*)", "1")
Edits_Harmful$religion <- stringr::str_replace_all(Edits_Harmful$religion, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$financial_earnings <- stringr::str_replace_all(Edits_Harmful$financial_earnings, "(.*)financial_earnings(.*)", "1")
Edits_Harmful$financial_earnings <- stringr::str_replace_all(Edits_Harmful$financial_earnings, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$family_current_life <- stringr::str_replace_all(Edits_Harmful$family_current_life, "(.*)family_current_life(.*)", "1")
Edits_Harmful$family_current_life <- stringr::str_replace_all(Edits_Harmful$family_current_life, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$character <- stringr::str_replace_all(Edits_Harmful$character, "(.*)character(.*)", "1")
Edits_Harmful$character <- stringr::str_replace_all(Edits_Harmful$character, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$early_life <- stringr::str_replace_all(Edits_Harmful$early_life, "(.*)early_life(.*)", "1")
Edits_Harmful$early_life <- stringr::str_replace_all(Edits_Harmful$early_life, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$achievements_awards <- stringr::str_replace_all(Edits_Harmful$achievements_awards, "(.*)achievements_awards(.*)", "1")
Edits_Harmful$achievements_awards <- stringr::str_replace_all(Edits_Harmful$achievements_awards, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$activities_memberships <- stringr::str_replace_all(Edits_Harmful$activities_memberships, "(.*)activities_memberships(.*)", "1")
Edits_Harmful$activities_memberships <- stringr::str_replace_all(Edits_Harmful$activities_memberships, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Harmful$religion == 1)
table(Edits_Harmful$financial_earnings == 1)
table(Edits_Harmful$family_current_life == 1)
table(Edits_Harmful$character == 1)
table(Edits_Harmful$early_life == 1)
table(Edits_Harmful$achievements_awards == 1)
table(Edits_Harmful$activities_memberships == 1)

sum(1, 4, 11, 25, 7, 1, 2) #/51
# 1/51, 4/51, 11/51, 25/51, 7/51, 1/51, 2/51


# (2b) Bar-Plot: Beneficial vs Harmful in PERSONAL 

data_BarPlot_2b <- data.frame(Response = factor(c("Beneficial Political", "Beneficial Political", "Beneficial Political", "Beneficial Political", "Beneficial Political", "Beneficial Political", "Beneficial Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political")),
                              Category = factor( c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships", "Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships"),
                                                 levels=  c("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")),
                              percentage = c(41/645, 65/645, 125/645, 171/645, 105/645, 70/645, 68/645, 1/51, 4/51, 11/51, 25/51, 7/51, 1/51, 2/51 ))

BarPlot_2b <- ggplot(data_BarPlot_2b , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Beneficial versus Harmful Edits in Category: Personal") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.85, 0.85))



# Using a Linear Model for basically the same

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_religion_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_religion_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_religion_dummy, "(.*)religion(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_religion_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_religion_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_financial_earnings_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_financial_earnings_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_financial_earnings_dummy, "(.*)financial_earnings(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_financial_earnings_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_financial_earnings_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_family_current_life_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_family_current_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_family_current_life_dummy, "(.*)family_current_life(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_family_current_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_family_current_life_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_character_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_character_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_character_dummy, "(.*)character(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_character_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_character_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_early_life_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_early_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_early_life_dummy, "(.*)early_life(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_early_life_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_early_life_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_achievements_awards_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_achievements_awards_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_achievements_awards_dummy, "(.*)achievements_awards(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_achievements_awards_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_achievements_awards_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_activities_memberships_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_activities_memberships_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_activities_memberships_dummy, "(.*)activities_memberships(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_activities_memberships_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_activities_memberships_dummy, "(.*)[[:alpha:]](.*)", "0")



linear_model1b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_religion_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model2b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_financial_earnings_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model3b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_family_current_life_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model4b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_character_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model5b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_early_life_dummy~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model6b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_achievements_awards_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model7b <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_personal_activities_memberships_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)

# Regression Models Output

stargazer(linear_model1b , linear_model2b , linear_model3b , linear_model4b , linear_model5b , linear_model6b , linear_model7b , 
          title="Table (2b). Difference between political edits and maintenance edits: Personal",
          type = "text", style = "default", out="Table(2b)NEWDiff_Personal.html")




# ----------------------------------------------------------------------------------------------------------------------------
# 2c. Differences within Career: "Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service" usw.
# -----------------------------------------------------------------------------------------------------------------------------


# Politically 
Edits_Beneficial$election_campaign <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$scandals_political  <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$tenure_activity  <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$legislation <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$district_service <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$early_political_career <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$congress_offices <- Edits_Beneficial$Answer.topic_career
Edits_Beneficial$early_career <- Edits_Beneficial$Answer.topic_career

Edits_Beneficial$election_campaign <- stringr::str_replace_all(Edits_Beneficial$election_campaign, "(.*)election_campaign(.*)", "1")
Edits_Beneficial$election_campaign <- stringr::str_replace_all(Edits_Beneficial$election_campaign, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$scandals_political  <- stringr::str_replace_all(Edits_Beneficial$scandals_political , "(.*)scandals_political(.*)", "1")
Edits_Beneficial$scandals_political <- stringr::str_replace_all(Edits_Beneficial$scandals_political, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$tenure_activity <- stringr::str_replace_all(Edits_Beneficial$tenure_activity, "(.*)tenure_activity(.*)", "1")
Edits_Beneficial$tenure_activity <- stringr::str_replace_all(Edits_Beneficial$tenure_activity, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$legislation <- stringr::str_replace_all(Edits_Beneficial$legislation, "(.*)legislation(.*)", "1")
Edits_Beneficial$legislation <- stringr::str_replace_all(Edits_Beneficial$legislation, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$district_service <- stringr::str_replace_all(Edits_Beneficial$district_service, "(.*)district_service(.*)", "1")
Edits_Beneficial$district_service <- stringr::str_replace_all(Edits_Beneficial$district_service, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$achievements_awards <- stringr::str_replace_all(Edits_Beneficial$early_political_career, "(.*)early_political_career(.*)", "1")
Edits_Beneficial$early_political_career <- stringr::str_replace_all(Edits_Beneficial$early_political_career, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$congress_offices <- stringr::str_replace_all(Edits_Beneficial$congress_offices, "(.*)congress_offices(.*)", "1")
Edits_Beneficial$congress_offices <- stringr::str_replace_all(Edits_Beneficial$congress_offices, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$early_career <- stringr::str_replace_all(Edits_Beneficial$early_career, "(.*)early_career(.*)", "1")
Edits_Beneficial$early_career <- stringr::str_replace_all(Edits_Beneficial$early_career, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Beneficial$election_campaign == 1)
table(Edits_Beneficial$scandals_political == 1)
table(Edits_Beneficial$tenure_activity == 1)
table(Edits_Beneficial$legislation == 1)
table(Edits_Beneficial$district_service == 1)
table(Edits_Beneficial$early_political_career == 1)
table(Edits_Beneficial$congress_offices == 1)
table(Edits_Beneficial$early_career == 1)

sum(186, 253, 128, 346, 124, 0, 311, 109) #/1457
# 186/1457, 253/1457, 128/1457, 346/1457, 124/1457, 0/1457, 311/1457, 109/1457



# Non Politically
Edits_Harmful$election_campaign <- Edits_Harmful$Answer.topic_career
Edits_Harmful$scandals_political  <- Edits_Harmful$Answer.topic_career
Edits_Harmful$tenure_activity <- Edits_Harmful$Answer.topic_career
Edits_Harmful$legislation <- Edits_Harmful$Answer.topic_career
Edits_Harmful$district_service<- Edits_Harmful$Answer.topic_career
Edits_Harmful$early_political_career <- Edits_Harmful$Answer.topic_career
Edits_Harmful$congress_offices <- Edits_Harmful$Answer.topic_career
Edits_Harmful$early_career <- Edits_Harmful$Answer.topic_career

Edits_Harmful$election_campaign <- stringr::str_replace_all(Edits_Harmful$election_campaign, "(.*)election_campaign(.*)", "1")
Edits_Harmful$election_campaign <- stringr::str_replace_all(Edits_Harmful$election_campaign, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$scandals_political <- stringr::str_replace_all(Edits_Harmful$scandals_political, "(.*)scandals_political(.*)", "1")
Edits_Harmful$scandals_political <- stringr::str_replace_all(Edits_Harmful$scandals_political, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$tenure_activity <- stringr::str_replace_all(Edits_Harmful$tenure_activity, "(.*)tenure_activity(.*)", "1")
Edits_Harmful$tenure_activity <- stringr::str_replace_all(Edits_Harmful$tenure_activity, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$legislation <- stringr::str_replace_all(Edits_Harmful$legislation, "(.*)legislation(.*)", "1")
Edits_Harmful$legislation <- stringr::str_replace_all(Edits_Harmful$legislation, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$district_service <- stringr::str_replace_all(Edits_Harmful$district_service, "(.*)district_service(.*)", "1")
Edits_Harmful$district_service <- stringr::str_replace_all(Edits_Harmful$district_service, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$early_political_career <- stringr::str_replace_all(Edits_Harmful$early_political_career, "(.*)early_political_career(.*)", "1")
Edits_Harmful$early_political_career <- stringr::str_replace_all(Edits_Harmful$early_political_career, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$congress_offices <- stringr::str_replace_all(Edits_Harmful$congress_offices, "(.*)congress_offices(.*)", "1")
Edits_Harmful$congress_offices <- stringr::str_replace_all(Edits_Harmful$congress_offices, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$early_career <- stringr::str_replace_all(Edits_Harmful$early_career, "(.*)early_career(.*)", "1")
Edits_Harmful$early_career <- stringr::str_replace_all(Edits_Harmful$early_career, "(.*)[[:alpha:]](.*)", "0")


table(Edits_Harmful$election_campaign == 1)
table(Edits_Harmful$scandals_political == 1)
table(Edits_Harmful$tenure_activity == 1)
table(Edits_Harmful$legislation == 1)
table(Edits_Harmful$district_service == 1)
table(Edits_Harmful$early_political_career == 1)
table(Edits_Harmful$congress_offices == 1)
table(Edits_Harmful$early_career == 1)

sum(14, 27, 15, 18, 7, 3, 36, 5) #/125
# 14/125, 27/125, 15/125, 18/125, 7/125, 3/125, 36/125, 5/125


# (2c) Bar-Plot: Political vs. Maintenance in CAREER

data_BarPlot_2c <- data.frame(Response = factor(c( "Beneficial Political", "Beneficial Political","Beneficial Political","Beneficial Political","Beneficial Political","Beneficial Political","Beneficial Political","Beneficial Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political" )),
                              Category = factor( c("Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service", 
                                                   "Early political career", "Offices/memberships in Congress", "Early non-political career", "Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service", 
                                                   "Early political career", "Offices/memberships in Congress", "Early non-political career"),
                                                 levels=  c("Congress election campaign", "Political scandals/controversis","Activities during tenure","Legislation", "District service", 
                                                            "Early political career", "Offices/memberships in Congress", "Early non-political career")),
                              percentage = c( 186/1457, 253/1457, 128/1457, 346/1457, 124/1457, 0/1457, 311/1457, 109/1457, 14/125, 27/125, 15/125, 18/125, 7/125, 3/125, 36/125, 5/125))


BarPlot_2c <- ggplot(data_BarPlot_2c , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Beneficial versus Harmful Political Edits in Category: Career") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 70))  +
  theme(legend.position = c(0.15, 0.9))



# Using a Linear Model for basically the same

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_election_campaign_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_election_campaign_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_election_campaign_dummy, "(.*)election_campaign(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_election_campaign_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_election_campaign_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_scandals_political_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_scandals_political_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_scandals_political_dummy, "(.*)scandals_political(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_scandals_political_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_scandals_political_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_tenure_activity_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_tenure_activity_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_tenure_activity_dummy, "(.*)tenure_activity(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_tenure_activity_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_tenure_activity_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_legislation_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_legislation_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_legislation_dummy, "(.*)legislation(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_legislation_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_legislation_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_district_service_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_district_service_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_district_service_dummy, "(.*)district_service(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_district_service_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_district_service_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_political_career_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_political_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_political_career_dummy, "(.*)early_political_career(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_political_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_political_career_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_congress_offices_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_congress_offices_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_congress_offices_dummy, "(.*)congress_offices(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_congress_offices_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_congress_offices_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_career_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_career_dummy, "(.*)early_career(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_career_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_career_dummy, "(.*)[[:alpha:]](.*)", "0")




linear_model1c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_election_campaign_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model2c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_scandals_political_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model3c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_tenure_activity_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model4c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_legislation_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model5c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_district_service_dummy~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model6c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_political_career_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model7c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_congress_offices_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model8c <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_career_early_career_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)




# Regression Models Output

stargazer(linear_model1c , linear_model2c , linear_model3c , linear_model4c , linear_model5c , linear_model6c , linear_model7c , linear_model8c , 
          title="Table (2c). Difference between political edits and maintenance edits: Career",  
          type = "text", style = "default", out="Table(2c)NEWDiff_Career.html")





# ----------------------------------------------------------------------------------------------------------------------------
# 2d. Differences within VIEWS: "Issue-specific views", "Support of other politicians","Ideology","Controversial statements"
# -----------------------------------------------------------------------------------------------------------------------------


# Politically 
Edits_Beneficial$issue_views <- Edits_Beneficial$Answer.topic_views
Edits_Beneficial$support_politicians  <- Edits_Beneficial$Answer.topic_views
Edits_Beneficial$ideology <- Edits_Beneficial$Answer.topic_views
Edits_Beneficial$statements_controversial <- Edits_Beneficial$Answer.topic_views


Edits_Beneficial$issue_views <- stringr::str_replace_all(Edits_Beneficial$issue_views, "(.*)issue_views(.*)", "1")
Edits_Beneficial$issue_views <- stringr::str_replace_all(Edits_Beneficial$issue_views, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$support_politicians  <- stringr::str_replace_all(Edits_Beneficial$support_politicians , "(.*)support_politicians(.*)", "1")
Edits_Beneficial$support_politicians <- stringr::str_replace_all(Edits_Beneficial$support_politicians, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$ideology<- stringr::str_replace_all(Edits_Beneficial$ideology, "(.*)ideology(.*)", "1")
Edits_Beneficial$ideology<- stringr::str_replace_all(Edits_Beneficial$ideology, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$statements_controversial <- stringr::str_replace_all(Edits_Beneficial$statements_controversial, "(.*)statements_controversial(.*)", "1")
Edits_Beneficial$statements_controversial <- stringr::str_replace_all(Edits_Beneficial$statements_controversial, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Beneficial$issue_views == 1)
table(Edits_Beneficial$support_politicians == 1)
table(Edits_Beneficial$ideology == 1)
table(Edits_Beneficial$statements_controversial == 1)


sum(309, 169, 137, 141) #/756
# 309/756, 169/756, 137/756, 141/756


# Non Politically
Edits_Harmful$issue_views <- Edits_Harmful$Answer.topic_views
Edits_Harmful$support_politicians  <- Edits_Harmful$Answer.topic_views
Edits_Harmful$ideology <- Edits_Harmful$Answer.topic_views
Edits_Harmful$statements_controversial <- Edits_Harmful$Answer.topic_views


Edits_Harmful$issue_views <- stringr::str_replace_all(Edits_Harmful$issue_views, "(.*)issue_views(.*)", "1")
Edits_Harmful$issue_views <- stringr::str_replace_all(Edits_Harmful$issue_views, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$support_politicians <- stringr::str_replace_all(Edits_Harmful$support_politicians, "(.*)support_politicians(.*)", "1")
Edits_Harmful$support_politicians <- stringr::str_replace_all(Edits_Harmful$support_politicians, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$ideology <- stringr::str_replace_all(Edits_Harmful$ideology, "(.*)ideology(.*)", "1")
Edits_Harmful$ideology <- stringr::str_replace_all(Edits_Harmful$ideology, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$statements_controversial <- stringr::str_replace_all(Edits_Harmful$statements_controversial, "(.*)statements_controversial(.*)", "1")
Edits_Harmful$statements_controversial <- stringr::str_replace_all(Edits_Harmful$statements_controversial, "(.*)[[:alpha:]](.*)", "0")


table(Edits_Harmful$issue_views == 1)
table(Edits_Harmful$support_politicians == 1)
table(Edits_Harmful$ideology == 1)
table(Edits_Harmful$statements_controversial == 1)


sum(20, 14, 10, 9) #/53
# 20/53, 14/53, 10/53, 9/53


# (2d) Bar-Plot: Political vs. Maintenance in VIEWS

data_BarPlot_2d <- data.frame(Response = factor(c("Beneficial Political", "Beneficial Political", "Beneficial Political", "Beneficial Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political" )),
                              Category = factor( c("Issue-specific views", "Support of other politicians","Ideology","Controversial statements", "Issue-specific views", "Support of other politicians","Ideology","Controversial statements"),
                                                 levels=  c("Issue-specific views", "Support of other politicians","Ideology","Controversial statements")),
                              percentage = c(309/756, 169/756, 137/756, 141/756, 20/53, 14/53, 10/53, 9/53 ))


BarPlot_2d <- ggplot(data_BarPlot_2d , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Political versus Maintenance Edits in Category: Views") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.85, 0.85))




# Using a Linear Model for basically the same: Views 
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_issue_views_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_issue_views_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_issue_views_dummy, "(.*)issue_views(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_issue_views_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_issue_views_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_support_politicians_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_support_politicians_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_support_politicians_dummy, "(.*)support_politicians(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_support_politicians_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_support_politicians_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_ideology_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_ideology_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_ideology_dummy, "(.*)ideology(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_ideology_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_ideology_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_statements_controversial_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_statements_controversial_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_statements_controversial_dummy, "(.*)statements_controversial(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_statements_controversial_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_statements_controversial_dummy, "(.*)[[:alpha:]](.*)", "0")

linear_model1d <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_issue_views_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model2d <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_support_politicians_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model3d <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_ideology_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model4d <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_views_statements_controversial_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)

# Regression Models Output
stargazer(linear_model1d , linear_model2d , linear_model3d , linear_model4d ,
          title="Table (2d). Difference between beneficial versus harmful political edits",
          type = "text", style = "default", out="Table(2d)NEWDiff_Views.html")




# ----------------------------------------------------------------------------------------------------------------------------
# 2e. Differences within OTHERs: "Other: External link", "Other: Publications","Other: References","Other: Categorization","Other: Other"
# -----------------------------------------------------------------------------------------------------------------------------


# Politically 
Edits_Beneficial$external_link <- Edits_Beneficial$Answer.topic_other
Edits_Beneficial$publications  <- Edits_Beneficial$Answer.topic_other
Edits_Beneficial$references <- Edits_Beneficial$Answer.topic_other
Edits_Beneficial$categorization <- Edits_Beneficial$Answer.topic_other
Edits_Beneficial$other_other <- Edits_Beneficial$Answer.topic_other

Edits_Beneficial$external_link <- stringr::str_replace_all(Edits_Beneficial$external_link, "(.*)external_link(.*)", "1")
Edits_Beneficial$external_link <- stringr::str_replace_all(Edits_Beneficial$external_link, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$publications  <- stringr::str_replace_all(Edits_Beneficial$publications , "(.*)publications(.*)", "1")
Edits_Beneficial$publications <- stringr::str_replace_all(Edits_Beneficial$publications, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$references<- stringr::str_replace_all(Edits_Beneficial$references, "(.*)references(.*)", "1")
Edits_Beneficial$references<- stringr::str_replace_all(Edits_Beneficial$references, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$categorization <- stringr::str_replace_all(Edits_Beneficial$categorization, "(.*)categorization(.*)", "1")
Edits_Beneficial$categorization <- stringr::str_replace_all(Edits_Beneficial$categorization, "(.*)[[:alpha:]](.*)", "0")
Edits_Beneficial$other_other <- stringr::str_replace_all(Edits_Beneficial$other_other, "(.*)other_other(.*)", "1")
Edits_Beneficial$other_other <- stringr::str_replace_all(Edits_Beneficial$other_other, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Beneficial$external_link == 1)
table(Edits_Beneficial$publications == 1)
table(Edits_Beneficial$references == 1)
table(Edits_Beneficial$categorization == 1)
table(Edits_Beneficial$other_other == 1)

sum(136, 28, 298, 4, 27) #/493
# 136/493, 28/493, 298/493, 4/493, 27/493


# Non Politically
Edits_Harmful$external_link <- Edits_Harmful$Answer.topic_other
Edits_Harmful$publications  <- Edits_Harmful$Answer.topic_other
Edits_Harmful$references <- Edits_Harmful$Answer.topic_other
Edits_Harmful$categorization <- Edits_Harmful$Answer.topic_other
Edits_Harmful$other_other <- Edits_Harmful$Answer.topic_other

Edits_Harmful$external_link <- stringr::str_replace_all(Edits_Harmful$external_link, "(.*)external_link(.*)", "1")
Edits_Harmful$external_link <- stringr::str_replace_all(Edits_Harmful$external_link, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$publications <- stringr::str_replace_all(Edits_Harmful$publications, "(.*)publications(.*)", "1")
Edits_Harmful$publications <- stringr::str_replace_all(Edits_Harmful$publications, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$references <- stringr::str_replace_all(Edits_Harmful$references, "(.*)references(.*)", "1")
Edits_Harmful$references <- stringr::str_replace_all(Edits_Harmful$references, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$categorization <- stringr::str_replace_all(Edits_Harmful$categorization, "(.*)categorization(.*)", "1")
Edits_Harmful$categorization <- stringr::str_replace_all(Edits_Harmful$categorization, "(.*)[[:alpha:]](.*)", "0")
Edits_Harmful$other_other <- stringr::str_replace_all(Edits_Harmful$other_other, "(.*)other_other(.*)", "1")
Edits_Harmful$other_other <- stringr::str_replace_all(Edits_Harmful$other_other, "(.*)[[:alpha:]](.*)", "0")

table(Edits_Harmful$external_link == 1)
table(Edits_Harmful$publications == 1)
table(Edits_Harmful$references == 1)
table(Edits_Harmful$categorization == 1)
table(Edits_Harmful$other_other == 1)

sum(10, 4, 13, 26, 5) #/58
# 10/58, 4/58, 13/58, 26/58, 5/58



# (2e) Bar-Plot: Political vs. Maintenance in VIEWS

data_BarPlot_2e <- data.frame(Response = factor(c("Beneficial Political","Beneficial Political","Beneficial Political","Beneficial Political","Beneficial Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political", "Harmful Political")),
                              Category = factor( c("External link", "Publications","References","Categorization","Other","External link", "Publications","References","Categorization","Other"),
                                                 levels=  c("External link", "Publications","References","Categorization","Other")),
                              percentage = c(136/493, 28/493, 298/493, 4/493, 27/493, 10/58, 4/58, 13/58, 26/58, 5/58))


BarPlot_2e <- ggplot(data_BarPlot_2e , aes(x=Category, y=percentage, fill=Response,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Subgroups of Edits") + 
  ggtitle("Distribution of Beneficial versus Harmful Edits in Category: Other") +
  xlab("Category") + 
  ylab("Distribution over Categories") +
  scale_x_discrete(guide = guide_axis(angle = 45))  +
  theme(legend.position = c(0.85, 0.85))



# Using a Linear Model for basically the same: Other

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_external_link_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_external_link_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_external_link_dummy, "(.*)external_link(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_external_link_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_external_link_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_publications_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_publications_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_publications_dummy, "(.*)publications(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_publications_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_publications_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_references_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_references_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_references_dummy, "(.*)references(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_references_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_references_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_categorization_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_categorization_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_categorization_dummy, "(.*)categorization(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_categorization_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_categorization_dummy, "(.*)[[:alpha:]](.*)", "0")

Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_other_other_dummy <- Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_other_other_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_other_other_dummy, "(.*)other_other(.*)", "1")
Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_other_other_dummy <- stringr::str_replace_all(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_other_other_dummy, "(.*)[[:alpha:]](.*)", "0")

linear_model1e <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_external_link_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model2e <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_publications_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model3e <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_references_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model4e <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_categorization_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)
linear_model5e <- lm(Inside_Congress_Edits_Politically_PositiveNegative$Answer.topic_other_other_other_dummy ~ Inside_Congress_Edits_Politically_PositiveNegative$politically_motivated_positive)


# Regression Models Output

stargazer(linear_model1e , linear_model2e , linear_model3e , linear_model4e, linear_model5e,
          title="Table (2e). Difference between beneficial versus harmful political edits in category: Other",
          type = "text", style = "default", out="Table(2e)NEWDiff_Other.html")










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
table(is.na(Edits_Harmful$Answer.topic_personal))
table(is.na(Edits_Harmful$Answer.topic_career))
table(is.na(Edits_Harmful$Answer.topic_views))
table(is.na(Edits_Harmful$Answer.topic_other))

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



