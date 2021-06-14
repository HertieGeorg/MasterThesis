# Cleaning election date data
# Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3WZFK9
# Codebook: SLERs1967to2016_20180927_Codebook.docx
# Data: Election_Dates_196slers1967to2016_20180908.rdata
# WD: "/Users/georg/Documents/GitHub/MasterThesis/Data on Election Dates and Vote Margins /Election Dates/Dataverse Havard Election Dates "

election_data <- table #name of data "Election_Dates_196slers1967to2016_20180908.rdata" if loaded into environment

election_data <- dplyr::select(election_data, c(year, month, day, sab, etype , sen, eseats, party,  cand, 
                                                   candid, vote, outcome, v19_20171211,  sfips , sicpsr, cando, candformat))


election_data <- election_data %>%  filter(year >= 1998)
#unique(election_data$year)

#  NAs 
summary(election_data$day)
summary(election_data$month)
summary(election_data$year)


#------------------Intermezzo------------------------
# how to deal with the NAs? 
  # library(ggplot2)
  # p <- ggplot(data=election_data, aes(x=date, fill = etype)) + geom_histogram()
  # p

# election_NAdays <-  election_data %>%  filter(is.na(election_data$day) == T)
# table(election_NAdays$etype)
# sort(table(election_data$etype))

#Elections that I wil focus on exclude: 
#dp: Democratic primary (5545)
#rp: Republican primary (5556)
#la: System of elections utilized in Louisiana (ca. 2700)
# I will focus only on general elections (i.e., g = 137670) 

# for general elections there are no missing values
#-----------------------------------------------------

# Filter just for general elections 
election_data <- election_data %>%  filter(etype == "g")


election_data <- election_data %>% 
  unite(date, year, month, day, sep = "-", remove = FALSE)

# Convert character into Date-Format 
election_data$date <- as.Date(election_data$date, format='%Y-%m-%d')
typeof(election_data$date)
summary(election_data$date)
table(election_data$date)
length(unique(election_data$date))

#Output 1: 
election_data

#Output 2:
list_of_all_general_election_dates <- unique(election_data$date)

#Output 3:
# Which are the biggest general elections? 
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[tabulate(match(x, uniqx)) >= 10000]}

list_of_biggest_general_election_dates  <- calculate_mode(election_data$date)


#Out: list_of_biggest_general_election_dates
# Saving Dataframe 
# save(list_of_biggest_general_election_dates, file = "list_of_biggest_general_election_dates.Rdata")


# I will focus first on general elections but it might be interesting at...
# ...a later point in time to also assess the connection between primary elections and edit-volume


