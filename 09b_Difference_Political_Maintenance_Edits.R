
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





# Load dataset: 
# IN: Inside_Congress_Edits_Politically


# 4. Comparison of distribution of topics between non political edits from within congress and political edits 


# Devide dataset into politically and non-politically 
Edits_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)
Edits_Non_Politically <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 0)


#----- TOPIC CATEGORY ("Personal", "Career","Views","Other")

# Get number of entires for every topic category (to use it for pie-chart)
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


# Using a Linear Model 

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
          title="Table (1). Difference between political edits and maintenance edits",
          type = "text", style = "default", out="Table(1)_Löschbar.html")





#----- TOPIC PERSONAL SUBCATEGORIES ("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")


# For better insights the column should be entangled in different columns 


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

sum(44, 76, 152, 212, 128, 78, 73) #/763
Personal_Subcategories_Pol = c(44/763, 76/763, 152/763, 212/763, 128/763, 78/763, 73/763)

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

sum(19, 4, 120, 37,109, 11, 34) #/334
Personal_Subcategories_NonPol = c(19/334, 4/334, 120/334, 37/334,109/334, 11/334, 34/334)

Distribution_TopicPERSONAL_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Personal_Subcategories_NonPol)

# Join political and non-political

Distribution_TopicsPersonal_Congress_Edits <- left_join(Distribution_TopicPERSONAL_Politically  , Distribution_TopicPERSONAL_Non_Politically   , by ="Topic")


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
          title="Table (1). Difference between political edits and maintenance edits",
          type = "text", style = "default", out="Table(1)_Löschbar.html")







#----- TOPIC CAREER SUBCATEGORIES ("Religion", "Financial Earnings","Family/Current Life","Character", "Early Life", "Achievements/Awards", "Activites/Memberships")


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

sum(243, 310, 166, 410, 150, 0, 383, 129) #/1791
Career_Subcategories_Pol = c(243/1791, 310/1791, 166/1791, 410/1791, 150/1791, 0/1791, 383/1791, 129/1791) 


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

sum(105, 9, 19, 81, 65, 26, 485, 49) #/839
Career_Subcategories_NonPol = c(105/839, 9/839, 19/839, 81/839, 65/839, 26/839, 485/839, 49/839)

Distribution_TopicCareer_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Career_Subcategories_NonPol)

# Join political and non-political

Distribution_TopicsCareer_Congress_Edits <- left_join(Distribution_TopicCareer_Politically  , Distribution_TopicCareer_Non_Politically   , by ="Topic")




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
          title="Table (1). Difference between political edits and maintenance edits",
          type = "text", style = "default", out="Table(3)_Löschbar.html")






#----- TOPIC Political positions (VIEWS) SUBCATEGORIES


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


sum(359, 204, 157, 162) #/882
Views_Subcategories_Pol = c(359/882, 204/882, 157/882, 162/882)


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


sum(62, 47, 25, 9) #/143
Views_Subcategories_NonPol = c(62/143, 47/143, 25/143, 9/143)

Distribution_TopicViews_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Views_Subcategories_NonPol)

# Join political and non-political

Distribution_TopicsViews_Congress_Edits <- left_join(Distribution_TopicViews_Politically  , Distribution_TopicViews_Non_Politically   , by ="Topic")




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
          title="Table (1). Difference between political edits and maintenance edits",
          type = "text", style = "default", out="Table(4)_Löschbar.html")






#----- TOPIC Other SUBCATEGORIES


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


sum(155, 38, 338, 6, 35) #/572
Other_Subcategories_Pol = c(155/572, 38/572, 338/572, 6/572, 35/572)


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


sum(75, 7, 66, 16, 115) #/279
Other_Subcategories_NonPol = c(75/279, 7/279, 66/279, 16/279, 115/279)

Distribution_TopicOther_Non_Politically <- data.frame(Topic = Label_Names,  Maintenance  = Other_Subcategories_NonPol)

# Join political and non-political

Distribution_TopicsOther_Congress_Edits <- left_join(Distribution_TopicOther_Politically  , Distribution_TopicOther_Non_Politically   , by ="Topic")





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
          title="Table (1). Difference between political edits and maintenance edits",
          type = "text", style = "default", out="Table(6)_Löschbar.html")







##----------------- Bring all together

# rbind Topics and Subtopics:Personal
Distribution_Topics_Political_vs_Maintenance <- rbind( Distribution_Topics_Congress_Edits, 
                                             Distribution_TopicsPersonal_Congress_Edits, 
                                             Distribution_TopicsCareer_Congress_Edits, 
                                             Distribution_Topicsother_Congress_Edits,
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



