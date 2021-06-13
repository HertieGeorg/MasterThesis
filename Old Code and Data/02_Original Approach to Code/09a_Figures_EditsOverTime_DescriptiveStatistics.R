
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
library(gridExtra)
library(ggplot2)
library(dplyr)


# Defining the election year: Just the election year 




# Load Edit Data (Sessions 109th-114th)
data <- main_dataframe_core


print(list_of_biggest_general_election_dates)
# All elections dates before 2005 are not needed
list_of_biggest_general_election_dates_short <- as.Date(c("2006-11-07" , "2010-11-02", "2004-11-02" , "2014-11-04" ,  "2008-11-04", 
                                                          "2012-11-06" ,"2016-11-08")) #



#---------------------------------------------------------------------------------


#A: Overall Congress Edits vs. Overall-non-congress edits
#Figure 1: Overall Congress Edits compared to overall-non-congress edits with time-frame marker around elections (1 Figure - 2 graphs) (with smooth line over all years) 


# Choosing observed time frame 
house_us_history$year <- stringr::str_extract(house_us_history$timestamp , "^\\d{4}") 
house_us_history_109_114 <- house_us_history  %>%  filter(year >= 2005) %>%  filter(year <= 2016)

# Choosing the examined MoHs -> load: Characteristics_Data_MoCs
list_pageids_MoH <- unique(Characteristics_Data_MoCs$pageid) #create list that contains pageids of the examined MoHs
house_us_history_109_114 <- house_us_history_109_114 %>%  filter(pageid %in% list_pageids_MoH) 

# Creating Date Column without exat timestamp
house_us_history_109_114$date <- stringr::str_extract(house_us_history_109_114$timestamp , "^\\d{4}-\\d{2}-\\d{2}") 
house_us_history_109_114$date  <- stringr::str_replace_all(house_us_history_109_114$date , " ", "")  
house_us_history_109_114$date <- as.Date(house_us_history_109_114$date)

#Experiment to reduce number of observations by taking out a random sample 
house_us_history_109_114$sample <- house_us_history_109_114$revid
house_us_history_109_114$sample <- stringr::str_extract(house_us_history_109_114$sample, "\\d$") 
table(house_us_history_109_114$sample) # is equally distributet over numbers 1-10 
house_us_history_109_114_short <- sample_n(house_us_history_109_114, 50862)
table(house_us_history_109_114_short$sample) # is also equally distributet over numbers 1-10 



# The graphs only includes 10% of all observations, therefore y-scale must be taken times 10 to interpret it 
# Grey marked area indicates election year 
AA3 <- ggplot(house_us_history_109_114_short, aes(x= date)) + 
  geom_rect(aes(xmin = as.Date("2004-01-24"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2005-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-01"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-01"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-01"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-01"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-01"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-01"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-12-31"), ymax = 1500),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6, alpha=1,  color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(a) All edits (109th-114th Session)") + 
  theme_classic() + 
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 




# Grey marked area indicates election year 
BB3 <- ggplot(Inside_Congress_Edits_Politically, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-24"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-01"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-01"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-01"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-01"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-01"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-01"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-12-31"), ymax = 85),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6, alpha=1,  color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(b) Edits from the Congress IT network (109th-114th Session)") + 
  theme_classic() + 
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 



# Figure3A_EditsTime
grid.arrange(AA3, BB3, ncol = 1, nrow = 2)


#-----------------------------------------------------

#B:Political Edits and Maintenance-Edits 
#Figure 2: Compare Political and Maintenance edits over time with 2 different timeframe marker (1 Figure, 2 graphs (with smooth line over all years) (first timeframe marker before election for political edits, and second after election for maintenance edits) with clear hard line for election date 



dataA <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1) 
dataB <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 0)

#Take 10 months before election to also see effects of primaries: starting start of january 
AA2 <- ggplot(dataA, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-01"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-01"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-01"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-01"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-01"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-01"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-01"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 62),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "blue", fill = "blue") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(a) Political edits from the Congress IT network") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


# Grey marked area are the 6 Months after election date 
BB2 <- ggplot(dataB, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2005-01-24"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2005-05-02"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-11-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2007-05-07"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-11-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2009-05-04"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-11-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2011-05-02"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-11-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2013-05-06"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-11-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2015-05-04"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-11-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2017-01-03"), ymax = 35),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6, alpha=1, color = "red", fill = "red") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(b) Maintenance edits from the Congress IT network") + 
  theme_classic() + 
  xlim( as.Date(c("2005-01-24","2017-01-03"))) 

#red3
#royalblue4

# Figure3B_EditsTime
grid.arrange(AA2, BB2, ncol = 1, nrow = 2)


#-----------------------------------------------------

#C: Political Edits and Maintenance-Edits and Overall Edits 
#Figure 3: Compared all 3 edit categories in election year with timeframe marker around election (1 Figure, 3 graphs) with clear hard line for election date 

# Chosen Time Frame: End of January of Election Year to incorporate effects of primaries 
# and End of March of next year to incorporate effects of first-tenure Congressman 

Inside_Congress_Edits_Politically$year_dummy <- 0

for(i in 1:length(Inside_Congress_Edits_Politically$year_dummy)) {
  if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2007-01-31" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2006-01-31") {
    Inside_Congress_Edits_Politically$year_dummy[i] = "2006 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2009-01-31" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2008-01-31") {
    Inside_Congress_Edits_Politically$year_dummy[i] = "2008 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2011-01-31" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2010-01-31") {
    Inside_Congress_Edits_Politically$year_dummy[i] = "2010 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2013-01-31" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2012-01-31") {
    Inside_Congress_Edits_Politically$year_dummy[i] = "2012 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2015-01-31" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2014-01-31") {
    Inside_Congress_Edits_Politically$year_dummy[i] = "2014 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2017-01-02" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2016-01-31") {
    Inside_Congress_Edits_Politically$year_dummy[i] = "2016 Election" 
  }
  else {
    Inside_Congress_Edits_Politically$year_dummy[i] = 0
  }
}


table(Inside_Congress_Edits_Politically$year_dummy) # worked quite well
# Drop all other observations outside of election years 
Inside_Congress_Edits_Politically_Years <- Inside_Congress_Edits_Politically %>%  filter(year_dummy != 0)


#Newcomers do not take office until the end of january!! -> increase timeframe up until March 
#From Januar to March



CC9 <- ggplot(Inside_Congress_Edits_Politically_Years , aes(x= date_LegislatoR)) + 
  geom_histogram(bins=72, size = 0.6, alpha=1,  color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  facet_grid(. ~ year_dummy,  scales = "free_x") + 
  labs(y="Aggregated", x = "(a) All edits from the Congress IT network during elections years") + 
  theme_classic() +
  scale_x_date(date_labels = " %b'%y") 



dataA_Years <- Inside_Congress_Edits_Politically_Years %>% filter(politically_motivated == 1) 
dataB_Years <- Inside_Congress_Edits_Politically_Years %>% filter(politically_motivated == 0)


AA9 <- ggplot(dataA_Years  , aes(x= date_LegislatoR)) + 
  geom_histogram(bins=72, size = 0.6, alpha=1,  color = "blue", fill = "blue") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  facet_grid(. ~ year_dummy,  scales = "free_x") + 
  labs(y="Aggregated", x = "(b) Political edits from the Congress IT network during elections years") + 
  theme_classic()  +
  scale_x_date(date_labels = " %b'%y")

AA10 <- AA9 + theme(strip.text.x = element_blank()) 



BB9 <- ggplot(dataB_Years , aes(x= date_LegislatoR)) + 
  geom_histogram(bins=72, size = 0.6, alpha=1,  color = "red", fill = "red") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  facet_grid(. ~ year_dummy,  scales = "free_x") + 
  labs(y="Aggregated", x = "(c) Maintenance edits from the Congress IT network during elections years") + 
  theme_classic()  +
  scale_x_date(date_labels = " %b'%y") 

BB10 <- BB9 + theme(strip.text.x = element_blank()) 

# Figure3C_EditsTime
grid.arrange(CC9, AA10, BB10, ncol = 1, nrow = 3)



# Figure3(Table)C_EditsTime
#Number of Observations for each year in each category 

x <- data.frame(table(Inside_Congress_Edits_Politically$year_dummy)) # worked quite well
y <- data.frame(table(dataA_Years$year_dummy)) # Political
z <- data.frame(table(dataB_Years$year_dummy)) # Maintenance

x <- left_join(x, y, by = "Var1")
x <- left_join(x, z, by = "Var1")
x <- data.frame(x)

x["Var1"] <- c("0", "2006", "2008", "2010", "2012", "2014", "2016") 
x <- rename(x,  "Year" = "Var1")
x <- rename(x,  "All Edits" = "Freq.x")
x <- rename(x,  "Political Edits" = "Freq.y")
x <-rename(x,  "Maintenance Edits" = "Freq")
x  = x [-1,]

row.names(x) <- NULL

library(formattable)
formattable(x, align =c("l","c","r","r"), 
            list(`Year` = formatter("span", 
                                    style = ~ style(color = "lightblack",
                                                    font.weight = "bold"))))



#-----------------------------------------------------------------

#D: Political Edits and Maintenance-Edits and Overall Edits 
#Figure 4: Compared all 3 edit categories in month around election (1 Figure, 3 graphs) with clear hard line for election date

# Chosen Time Frame: End of January of Election Year to incorporate effects of primaries 
# and End of March of next year to incorporate effects of first-tenure Congressman 

Inside_Congress_Edits_Politically$month_dummy <- 0

for(i in 1:length(Inside_Congress_Edits_Politically$month_dummy)) {
  if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2006-11-21" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2006-08-17") {
    Inside_Congress_Edits_Politically$month_dummy[i] = "2006 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2008-11-18" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2008-08-14") {
    Inside_Congress_Edits_Politically$month_dummy[i] = "2008 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2010-11-16" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2010-08-12") {
    Inside_Congress_Edits_Politically$month_dummy[i] = "2010 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2012-11-20" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2012-08-16") {
    Inside_Congress_Edits_Politically$month_dummy[i] = "2012 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2014-11-18" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2014-08-20") {
    Inside_Congress_Edits_Politically$month_dummy[i] = "2014 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2016-11-22" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2016-08-18") {
    Inside_Congress_Edits_Politically$month_dummy[i] = "2016 Election" 
  }
  else {
    Inside_Congress_Edits_Politically$month_dummy[i] = 0
  }
}


table(Inside_Congress_Edits_Politically$month_dummy) # worked quite well
# Drop all other observations outside of election months 
Inside_Congress_Edits_Politically_months <- Inside_Congress_Edits_Politically %>%  filter(month_dummy != 0)



CC9 <- ggplot(Inside_Congress_Edits_Politically_months , aes(x= date_LegislatoR)) + 
  geom_histogram(bins=30, size = 0.6, alpha=1,  color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.8)  + 
  facet_grid(. ~ month_dummy,  scales = "free_x") + 
  labs(y="Aggregated", x = "(a) All edits from the Congress IT network during election months") + 
  theme_classic()  +
  scale_x_date(date_labels = " %b'%y") 



dataA_months <- Inside_Congress_Edits_Politically_months %>% filter(politically_motivated == 1) 
#dataB_months <- Inside_Congress_Edits_Politically_months %>% filter(politically_motivated == 0)


AA9 <- ggplot(dataA_months  , aes(x= date_LegislatoR)) + 
  geom_histogram(bins=30, size = 0.6, alpha=1,  color = "blue", fill = "blue") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.8)  + 
  facet_grid(. ~ month_dummy,  scales = "free_x") + 
  labs(y="Aggregated", x = "(b) Political edits from the Congress IT network during election months") + 
  theme_classic()  +
  scale_x_date(date_labels = " %b'%y") 


AA10 <- AA9 + theme(strip.text.x = element_blank()) 



# Extra Data for Maintenacne Edits 
Inside_Congress_Edits_Politically$month_maint <- 0

for(i in 1:length(Inside_Congress_Edits_Politically$month_maint)) {
  if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2007-01-16" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2006-10-27") {
    Inside_Congress_Edits_Politically$month_maint[i] = "2006 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2009-01-18" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2008-10-18") {
    Inside_Congress_Edits_Politically$month_maint[i] = "2008 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2011-01-16" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2010-10-16") {
    Inside_Congress_Edits_Politically$month_maint[i] = "2010 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2013-01-20" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2012-10-20") {
    Inside_Congress_Edits_Politically$month_maint[i] = "2012 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2015-01-18" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2014-10-18") {
    Inside_Congress_Edits_Politically$month_maint[i] = "2014 Election" 
  }
  else if (Inside_Congress_Edits_Politically$date_LegislatoR[i] <= "2017-01-19" && Inside_Congress_Edits_Politically$date_LegislatoR[i] >= "2016-10-19") {
    Inside_Congress_Edits_Politically$month_maint[i] = "2016 Election" 
  }
  else {
    Inside_Congress_Edits_Politically$month_maint[i] = 0
  }
}



table(Inside_Congress_Edits_Politically_month2$month_maint) # worked quite well
# Drop all other observations outside of election months 
Inside_Congress_Edits_Politically_month2 <- Inside_Congress_Edits_Politically %>%  filter(month_maint != 0)

dataB_month2 <- Inside_Congress_Edits_Politically_month2 %>% filter(politically_motivated == 0)


BB9 <- ggplot(dataB_month2 , aes(x= date_LegislatoR)) + 
  geom_histogram(bins=30, size = 0.6, alpha=1,  color = "red", fill = "red") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.8)  + 
  facet_grid(. ~ month_maint,  scales = "free_x") + 
  labs(y="Aggregated", x = "(c) Maintenance edits from the Congress IT network during election months") + 
  theme_classic() +
  scale_x_date(date_labels = " %b'%y") 


BB10 <- BB9 + theme(strip.text.x = element_blank()) 

# Figure3D_EditsTime
grid.arrange(CC9, AA10, BB10, ncol = 1, nrow = 3)




#----------------------------------------------------------------------------------


#F: Showing political edits over entire frame with timeframe marker around elections 
#once for Democrats and once for Republicans (1 Figure, 2 Graphs)


Pageid_Party <- Characteristics_Data_MoCs %>% select(c("pageid", "party"))
Inside_Congress_Edits_Politically_Party <- left_join(Inside_Congress_Edits_Politically, Pageid_Party, by = "pageid")


data_D1 <- Inside_Congress_Edits_Politically_Party %>% filter(politically_motivated == 1) %>% filter(party == "D")
data_D2 <- Inside_Congress_Edits_Politically_Party %>% filter(politically_motivated == 0) %>% filter(party == "D") 

data_R1 <- Inside_Congress_Edits_Politically_Party %>% filter(politically_motivated == 1) %>% filter(party == "R")
data_R2 <- Inside_Congress_Edits_Politically_Party %>% filter(politically_motivated == 0) %>% filter(party == "R") 



AD1 <- ggplot(data_D1, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "blue", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(a)  Political edits from the Congress IT network on profiles of Democratic MoCs") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


AR1 <- ggplot(data_R1, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 40),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "red", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(b) Political edits from the Congress IT network on profiles of Republican MoCs") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 

#Figure3E_EditsTime
grid.arrange(AD1, AR1, ncol = 1, nrow = 2)


#------------------------------------------------------

#Figure3E2_EditsTime für den Appendix

AD2 <- ggplot(data_D2, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-11-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2005-07-02"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-11-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2007-07-07"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-11-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2009-07-04"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-11-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2011-07-02"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-11-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2013-07-06"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-11-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2015-07-04"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-11-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2017-07-02"), ymax = 30),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "blue", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "black", size=0.6)  + 
  labs(y="Aggregated", x = "(a) Maintenance edits from the Congress IT network on profiles of Democratic MoCs") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


AR2 <- ggplot(data_R2, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-11-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2005-07-02"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-11-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2007-07-07"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-11-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2009-07-04"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-11-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2011-07-02"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-11-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2013-07-06"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-11-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2015-07-04"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-11-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2017-07-02"), ymax = 18),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "red", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(b) Maintenance edits from the Congress IT network on profiles of Republican MoCs") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


#Figure3E2_EditsTime für den Appendix
grid.arrange(AD2, AR2, ncol = 1, nrow = 2)

#-------------------------------------------------

#Figure3F_EditsTime
#F: Showing political edits over entire frame with timeframe marker around elections
#once for MPs running in top 20% competitive Districts and once for lower 20% (1 Figure, 2 Graphs)



# Set NAs in competitiveness (which occur for US terretories. e.g. Puerto Rico, DC, etc.)
Characteristics_Data_MoCs <- Characteristics_Data_MoCs %>% drop_na(Combined_Mean_vote_maxdiff_relative)
table(is.na(Characteristics_Data_MoCs$Combined_Mean_vote_maxdiff_relative)) #check whether it worked 

summary(Characteristics_Data_MoCs$Combined_Mean_vote_maxdiff_relative)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000734 0.152832 0.256164 0.293194 0.398327 1.000000 

Characteristics_Data_MoCs$Competitive_Dummy <- 0

for(i in 1:length(Characteristics_Data_MoCs$pageid)) {
  if (Characteristics_Data_MoCs$Combined_Mean_vote_maxdiff_relative[i] >=  0.398327) {
    Characteristics_Data_MoCs$Competitive_Dummy[i] = "Top25"}
  else if (Characteristics_Data_MoCs$Combined_Mean_vote_maxdiff_relative[i] <=  0.152832) {
    Characteristics_Data_MoCs$Competitive_Dummy[i] = "Bottom25" 
  }
}
table(Characteristics_Data_MoCs$Competitive_Dummy)

Pageid_Competitive <- Characteristics_Data_MoCs %>% select(c( "pageid", "Competitive_Dummy"))

Inside_Congress_Edits_Politically_Competitive <- left_join(Inside_Congress_Edits_Politically, Pageid_Competitive, by = "pageid")



data_CompetitiveT25 <- Inside_Congress_Edits_Politically_Competitive %>% filter(politically_motivated == 1) %>% filter(Competitive_Dummy == "Top25")
data_CompetitiveB25 <- Inside_Congress_Edits_Politically_Competitive %>% filter(politically_motivated == 1) %>% filter(Competitive_Dummy == "Bottom25")



ATop25 <- ggplot(data_CompetitiveT25, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=72, size = 0.6,  alpha=1, color = "blue", fill = "blue") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(a) Political edits from the Congress IT network on profiles of MoCs that run in the 25% most competitive elections districts")+
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


ABottom25 <- ggplot(data_CompetitiveB25, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 25),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=72, size = 0.6,  alpha=1, color = "blue", fill = "blue") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(b) Political edits from the Congress IT network on profiles of MoCs that run in the 25% least competitive elections districts")+
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 

# We can  already see that Top25 (=418) see much more political edtis then Bottom25 (= 248) 

#Figure3F_EditsTime
grid.arrange(ATop25, ABottom25, ncol = 1, nrow = 2)


#---------------------------------------


#Figure3G_EditsTime
#G: Graph showing all political edits, beneficial political edits and harmful political edits 
#over entire time frame with markers before elections and elections dates as line (1 Figure, 3 Graphs) 

dataA <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1) 
dataBP <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1) 
dataHP <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_negative == 1) 



Political_A <- ggplot(dataA, aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 65),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(a) All political edits from the Congress IT network") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


Political_BP <- ggplot(dataBP , aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 55),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(b) Beneficial political edits from the Congress IT network") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 



Political_HP <- ggplot(dataHP , aes(x= date_LegislatoR)) + 
  geom_rect(aes(xmin = as.Date("2004-01-02"), ymin = 0,  # "2004-11-02"
                xmax = as.Date("2004-11-02"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2006-01-07"), ymin = 0,  # "2006-11-07"
                xmax = as.Date("2006-11-07"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2008-01-04"), ymin = 0,  # "2008-11-04" 
                xmax = as.Date("2008-11-04"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2010-01-02"), ymin = 0,  # "2010-11-02"
                xmax = as.Date("2010-11-02"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2012-01-06"), ymin = 0,  #"2012-11-06"
                xmax = as.Date("2012-11-06"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2014-01-04"), ymin = 0,  #"2014-11-04"
                xmax = as.Date("2014-11-04"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_rect(aes(xmin = as.Date("2016-01-08"), ymin = 0,  # "2016-11-08"
                xmax = as.Date("2016-11-08"), ymax = 12),
            fill = "lightgrey", alpha=0.01) +
  geom_histogram(bins=144, size = 0.6,  alpha=1, color = "black", fill = "black") + 
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "darkgreen", size=0.6)  + 
  labs(y="Aggregated", x = "(c) Harmful political edits from the Congress IT network") + 
  theme_classic() +
  xlim( as.Date(c("2005-01-24","2017-01-02"))) 


# Just a reminder that there is a slightley different selection process between 
# political edits and the other two (beneficail and harmful)

#Figure3G_EditsTime
grid.arrange(Political_A, Political_BP, Political_HP , ncol = 1, nrow = 3)



#-----------------------------------------------------------------------------
###------------------------ Ideenspeicher ------------------------------------
#-----------------------------------------------------------------------------


# 1. All edits from within Congress in observed time frame over time 

#Figure 9b(1)
edits_90days <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=90, alpha=0.8) +
  ggtitle("All edits on MoCs Wikipedia Profiles from within Congress during 109th-114th session (Bin size = 90 Days)") +
  theme_ipsum()
edits_90days 

# Same but with markers for election dates (general elections every two months)
edits_210days_elections  <- data %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates_short, color = "orange", size=0.7) 
edits_210days_elections  





# 2. Political edits from within Congress over time (with markers for elections dates)
#Load Edit data with markers for politically motivated edits: 

data1 <- Inside_Congress_Edits_Politically %>% filter(politically_motivated == 1)

edits_politically_210days_elections   <- data1 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates_short, color = "orange", size=0.7) 
edits_politically_210days_elections  





# 3. Beneficial Political edits from within Congress over time (with markers for elections dates)
# Load: Inside_Congress_Edits_Politically_PositiveNegative

data2 <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_positive == 1)

edits_politically_210days_elections   <- data2 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All beneficial political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates_short, color = "orange", size=0.7) 
edits_politically_210days_elections  





# 4. Harmful Political edits from within Congress over time (with markers for elections dates)
# Load: Inside_Congress_Edits_Politically_PositiveNegative

data3 <- Inside_Congress_Edits_Politically_PositiveNegative %>% filter(politically_motivated_negative == 1)

edits_politically_210days_elections   <- data3 %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=210, alpha=0.9) +
  ggtitle("All harmful political edits on MoC profiles from within Congress (109th-114th Session)") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates_short, color = "orange", size=0.7) 
edits_politically_210days_elections  




# --------------- NEW COLLECTION OF PLOTS: SECOND GENERATION ------------------------------------------------------

#A: Overall Congress Edits vs. Overall-non-congress edits
#Figure 1: Overall Congress Edits compared to overall-non-congress edits with time-frame marker around elections (1 Figure - 2 graphs) (with smooth line over all years) 



facet_label_names <- c(
  `0` = "Maintenance Edits",
  `1` = "Political Edits")

data1 <- Inside_Congress_Edits_Politically
data1["count"] <- 1

ggplot(data1, aes(x= date_LegislatoR)) + 
  geom_histogram(bins=144, size = 0.6, alpha=1, fill="white",  aes(y = ..count.. , color = politically_motivated)) + 
  facet_grid( politically_motivated ~ .,  scales = "free_y",  labeller = as_labeller(facet_label_names)) + 
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("All maintenance and political edits on MoC profiles from within Congress (109th-114th Session)") +
  geom_vline(xintercept = list_of_biggest_general_election_dates_short,  linetype="dashed", color = "grey", size=0.6)  + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(y="Aggregated", x = "Date") + 
  geom_rect(aes(xmin = as.Date("2012-11-06"), ymin = 0, 
                xmax = as.Date("2014-11-04"), ymax = 60),
            fill = "white", alpha=0.006)






