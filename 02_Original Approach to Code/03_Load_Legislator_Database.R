
#Source: https://github.com/saschagobel/legislatoR


# load and attach legislatoR and dplyr
library(devtools)
devtools::install_github("saschagobel/legislatoR")
install.packages("legislatoR")
library(legislatoR)
library(dplyr)





# HOUSE-DATA

# Get core data 
house_us_core <- get_core(legislature = "usa_house")
#save(house_us_core, file = "house_core_LegislatoR.Rdata")

# Get political data  
house_us_political <- get_political(legislature = "usa_house") 
#save(house_us_political, file = "house_political_LegislatoR.Rdata")


# Get  profession data 
house_us_profession <- get_profession(legislature = "usa_house") 
#save(house_us_profession, file = "house_profession_LegislatoR.Rdata")

# Get history data 
house_us_history <- get_history(legislature = "usa_house") 
#save(house_us_history, file = "house_history_LegislatoR.Rdata")


# Get wikipedia traffic data 
house_us_traffic <- get_traffic(legislature = "usa_house") 
save(house_us_traffic, file = "house_traffic_LegislatoR.Rdata")


# SENATE-DATA

# Get core data 
senate_us_core <- get_core(legislature = "usa_senate")
#save(senate_us_core, file = "senate_core_LegislatoR.Rdata")

# Get political data  
senate_us_political <- get_political(legislature = "usa_senate") 
#save(senate_us_political, file = "senate_political_LegislatoR.Rdata")

# Get wikipedia traffic data 
senate_us_traffic <- get_traffic(legislature = "usa_senate") 
#save(senate_us_traffic, file = "senate_traffic_LegislatoR.Rdata")

# Get  profession data 
senate_us_profession <- get_profession(legislature = "usa_senate") 
#save(senate_us_profession, file = "senate_profession_LegislatoR.Rdata")

# Get history data 
senate_us_history <- get_history(legislature = "usa_senate") 
#save(senate_us_history, file = "senate_history_LegislatoR.Rdata")






#Merge Data

house_us_merge <- left_join(x = house_us_core, 
                      y = house_us_political, 
                      by = "pageid")


# Filter for the 115th session
us_sessions_since_2000 <- US_Sessions %>% filter(session >= 106)







#--------- Ideenspeicher------------------------------


bern <- us_merge %>%  filter( wikititle == 	"Ron_Kind")



#Finding out to which Sessions the edits belong

mturk_wikititle <- unique(mturk_df_dates$pageid)

sessions <- us_merge %>% filter(pageid %in% mturk_wikititle)

table(sessions$session)

list <- c("116", "115", "114", "113", "112", "111", "110", "109", "108", "107", "106", "105", "104", "103", "102")

solution <- sessions %>% filter(session %in% list)

length(unique(solution$pageid))




us_session <- left_join( x = us_political, y = mturk_df_dates, 
                      by = "pageid")

# To do: merge mturk with sessions, but only keep MP that are in Mturk
# filter by sessions and see which session contains all MPs 












