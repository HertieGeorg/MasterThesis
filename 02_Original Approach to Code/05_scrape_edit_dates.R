#Next: Get the Dates 


# e.g.: https://en.wikipedia.org/w/index.php?title=John_Spratt&diff=prev&oldid=186838604

# Create new column with Revision-ID
mturk_df$revid <- str_extract(mturk_df$Input.website_url, "oldid=(.+)")

# Clear Revision-ID
mturk_df$revid <- stringr::str_replace_all(mturk_df$revid, "oldid=", "")


# The revid in us_history is the same Id that is given in the links to the edits pages in the mturk_df 
# by merging the dataframes bases on this id, it is possible to get the dates of each edit 
#Example 
# This is the link to an edits of the profile of John Sprat
#https://en.wikipedia.org/w/index.php?title=John_Spratt&diff=prev&oldid=186838604

us_history %>% filter(revid == 186838604)
us_core %>% filter(pageid == 862737)

# Let's see if this works for all edits of John Spratt: 

mturk_df_JS <- mturk_df %>% filter(wikititle == "John_Spratt") 

mturk_df_JS$Input.website_url
us_history %>% filter(revid == 157398342)
us_history %>% filter(revid == 243429428)
us_history %>% filter(revid == 243429129)
us_history %>% filter(revid == 186838249)
us_history %>% filter(revid == 157927353)
us_history %>% filter(revid == 156982625)
us_history %>% filter(revid == 157399788)

# Seems to work, all edits from mturk_df can be found in us_history 

# So let's join mturk_df and us_history by the revid's found in mturk

# Transform Revid in mturk_df into integers so they match with us_history
mturk_df$revid <- as.integer(mturk_df$revid)

mturk_df_dates <- left_join(x = mturk_df, 
                      y = us_history, 
                      by = "revid")

# Drop unused variables
mturk_df_dates <- dplyr::select(mturk_df_dates, -c( Name,id, user, userid, WorkerId))


# Create new column that contains the only the day of the edit without exact time
mturk_df_dates$date_LegislatoR <- str_extract(mturk_df_dates$timestamp, "(.+) ")
mturk_df_dates$date_LegislatoR <- stringr::str_replace_all(mturk_df_dates$date_LegislatoR , " ", "")

summary(mturk_df_dates$timestamp)
# 211 NA's, seems like the us_history table is missing entires for 211 edits 

#library(lubridate)
mturk_df_dates$date_LegislatoR <- as.Date(mturk_df_dates$date_LegislatoR)


# Scrape all Dates including the missing ones 

# Create New Column based on the LegisloR-Dates 
mturk_df_dates$date_scraped <- mturk_df_dates$date_LegislatoR


#Loop that reads URL into html and then scrapes using xpath the date of the edit and saves it in the column "ScrapeDate"

for (i in 1:2758) {
  tryCatch({
    mturk_df_dates$date_scraped[i] <- read_html(mturk_df_dates$Input.website_url[i]) %>%
      html_nodes(xpath = "//h2[@class='diff-currentversion-title']") %>%
      html_text()
  }, error=function(e){})
  next
}




# Several dates cannot be scraped, apprently due to two reasons: 
# 1. Spaces within the URLs, e.g.: https://en.wikipedia.org/w/index.php?title=Tim_Johnson (South Dakota politician)&diff=prev&oldid=433395707
# 2. Spanish Names with accents above some letters, e.g.: https://en.wikipedia.org/w/index.php?title=Luis_Gutiérrez&diff=prev&oldid=443088797

# Replacing Spaces and Spanish letters using regular expressions

# Creating new cloumn with URLs
mturk_df_dates$Edited_Input.website_url <- mturk_df_dates$Input.website_url

# Replace all spaces with "20%"
mturk_df_dates$Edited_Input.website_url <- stringr::str_replace_all(mturk_df_dates$Edited_Input.website_url, " ", "%20")

# Replace spanish special letters with english letters
mturk_df_dates$Edited_Input.website_url <- stringr::str_replace_all(mturk_df_dates$Edited_Input.website_url, "é", "e")
mturk_df_dates$Edited_Input.website_url <- stringr::str_replace_all(mturk_df_dates$Edited_Input.website_url, "á", "a")
mturk_df_dates$Edited_Input.website_url <- stringr::str_replace_all(mturk_df_dates$Edited_Input.website_url, "í", "i")
mturk_df_dates$Edited_Input.website_url <- stringr::str_replace_all(mturk_df_dates$Edited_Input.website_url, "ñ", "n")

# Run Loop Again and save results in the same column

for (i in 1:2758) {
  tryCatch({
    mturk_df_dates$date_scraped[i] <- read_html(mturk_df_dates$Edited_Input.website_url[i]) %>%
      html_nodes(xpath = "//h2[@class='diff-currentversion-title']") %>%
      html_text()
  }, error=function(e){})
  next
}

# One-by-one Example
#wiki <- read_html("https://en.wikipedia.org/w/index.php?title=Luis_Gutiérrez&diff=prev&oldid=443088797")
#birth_date <- wiki %>%
#  html_nodes(xpath = "//h2[@class='diff-currentversion-title']") %>%
#  html_text()



# Saving Dataframe outside of R, so I don't have to scrape the dates again (takes quite a while)
#main_dataframe <- mturk_df_dates
#save(main_dataframe, file = "main_dataframe.Rdata")

# Getting it back:
#mturk_df_dates <- main_dataframe 


# Clean scraped strings into data form 
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "(.+)[[:punct:]] ", "")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, " ", "-")

# Change months from character into integer 
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "January", "01")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "February", "02")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "March", "03")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "April", "04")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "May", "05")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "June", "06")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "July", "07")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "August", "08")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "September", "09")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "October", "10")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "November", "11")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "December", "12")


# Convert Scraped-Dates into Date-Format 
mturk_df_dates$date_scraped <- as.Date(mturk_df_dates$date_scraped, format='%d-%m-%Y')

# Convert Scraped-Dates back in to character to be able to merge it with LegislatoR-Dates
mturk_df_dates$date_scraped <- as.character(mturk_df_dates$date_scraped)

typeof(mturk_df_dates$date_scraped)

# Check whether LegislatoR contains the same dates that were scraped 

summary(mturk_df_dates$date_scraped == mturk_df_dates$date_LegislatoR)
# Returns True for all but 220, but this is just the combined number of NAs of both lists (211 + 9)
summary(is.na(mturk_df_dates$date_LegislatoR == TRUE)) #211
summary(is.na(mturk_df_dates$date_scraped == TRUE))  #9
# Execpt of the NAs all entries seem to match 


# Fill NAs in date_LegislatoR with scraped dates
mturk_df_dates$date_LegislatoR <- ifelse(is.na(mturk_df_dates$date_LegislatoR) == TRUE, mturk_df_dates$date_scraped, mturk_df_dates$date_LegislatoR) 


# Convert LegislatoR-Dates into Date-Format 
mturk_df_dates$date_LegislatoR <- as.Date(mturk_df_dates$date_LegislatoR)
typeof(mturk_df_dates$date_LegislatoR)

#Check whether transformation worked for all entires
summary(is.na(mturk_df_dates$date_LegislatoR == TRUE))


# Drop unused variables
mturk_df_dates <- dplyr::select(mturk_df_dates, -c( date_scraped, `Edited_Input.website_url`))


# Check whether conversion to date worked and can be used for graphs 
#ggplot(mturk_df_dates , aes(x = date_LegislatoR, y = size)) +
#  geom_point()
# size has 211 NAs 



# There are 211 edits in mturk_df_dates that are missing an entry for pageid (which I use for mergin) 
sum(is.na( mturk_df_dates$pageid))
# Lets scrape the missing pageids using xpath

#install.packages("XML")
#library(XML)

MoCs_Without_Pageid <- as.data.frame(mturk_df_dates[is.na(mturk_df_dates$pageid),])
#names(MoCs_Without_Pageid )[names(MoCs_Without_Pageid) == "mturk_df_dates$wikititle[is.na(mturk_df_dates$pageid)]"] <- "wikititle"
MoCs_Without_Pageid <- MoCs_Without_Pageid %>%  dplyr::select(c(Input.website_url, wikititle))
MoCs_Without_Pageid$pageid_scraped <- 0

MoCs_Without_Pageid$Input.website_url <- stringr::str_replace_all(MoCs_Without_Pageid$Input.website_url, " ", "_")
MoCs_Without_Pageid$Input.website_url <- stringr::str_replace_all(MoCs_Without_Pageid$Input.website_url, "í", "i")
MoCs_Without_Pageid$Input.website_url <- stringr::str_replace_all(MoCs_Without_Pageid$Input.website_url, "á", "a")
MoCs_Without_Pageid$Input.website_url <- stringr::str_replace_all(MoCs_Without_Pageid$Input.website_url, "é", "e")
MoCs_Without_Pageid$Input.website_url <- stringr::str_replace_all(MoCs_Without_Pageid$Input.website_url, "ñ", "n")

# the following scraping loop takes at least 15min to execute 
for (i in 1:211) {
  tryCatch({
    String <- read_html(MoCs_Without_Pageid$Input.website_url[i]) %>%
      html_nodes(xpath = "//head//script") %>%
      html_text()
  }, error=function(e){})
  String <- as.data.frame(String)
  String <- String[-c(2, 3), ]
  String <- stringr::str_extract(String, "(?<=wgArticleId)[[:punct:]]+\\d+")
  String <- stringr::str_extract(String, "\\d+")
  MoCs_Without_Pageid$pageid_scraped[i] <-  as.double(String)
  next
}


MoCs_Without_Pageid <- MoCs_Without_Pageid %>%  dplyr::select(-c(Input.website_url))
MoCs_Without_Pageid <- unique(MoCs_Without_Pageid)
#Sicherheitskopie <- MoCs_Without_Pageid

mturk_df_dates <- left_join(mturk_df_dates, MoCs_Without_Pageid, by  = "wikititle")


#combine different pageid cloumns with an if else loop 
# if df$pageid[i] == 0, do df$pageid[i] <-  df$pageid_scraped[i], else  df$pageid[i] <-  df$pageid[i]

#Set Nas in pageid to zero
mturk_df_dates$pageid[is.na(mturk_df_dates$pageid)] <- 0


for (i in 1:2758) {
    if (mturk_df_dates$pageid[i] == 0) {
    mturk_df_dates$pageid[i] <- mturk_df_dates$pageid_scraped[i] 
    } else {
    mturk_df_dates$pageid[i] <- mturk_df_dates$pageid[i] 
    }
}
      

# Drop unused variables
mturk_df_dates <- mturk_df_dates %>%  dplyr::select(-c( pageid_scraped))


# Saving Dataframe outside of R, so I don't have to scrape the dates again (takes quite a while)
main_dataframe_dates <- mturk_df_dates
save(main_dataframe_dates , file = "main_dataframe_dates.Rdata")












#-------------------------------- Ideenspeicher-----------------------------


for (i in 1:2758) {
  mturk_df_dates$html[1] <- read_html(mturk_df_dates$Input.website_url[i]) %>%
    html_nodes(xpath = "//h2[@class='diff-currentversion-title']") %>%
    html_text()
  }


#2758
for (i in 1:2) {
  tryCatch({
    mturk_df_dates$html[i] <- read_html(mturk_df_dates$Input.website_url[i]) %>%
      html_nodes(xpath = "//h2[@class='diff-currentversion-title']") %>%
      html_text()
  }, error=function(e){})
  next
}




mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^1(?=[[:punct:]])", "01")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^2(?=[[:punct:]])", "02")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^3(?=[[:punct:]])", "03")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^4(?=[[:punct:]])", "04")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^5(?=[[:punct:]])", "05")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^6(?=[[:punct:]])", "06")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^7(?=[[:punct:]])", "07")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^8(?=[[:punct:]])", "08")
mturk_df_dates$date_scraped <- stringr::str_replace_all(mturk_df_dates$date_scraped, "^9(?=[[:punct:]])", "09")





#Check whether transformation worked for all entires
summary(is.na(mturk_df_dates$date_LegislatoR == TRUE))

#Check whether transformation worked for all entires
summary(is.na(mturk_df_dates$date_scraped == TRUE))
# Did not work for 9 entries, as we have 9 NAs

# Merge LegislatoR and Scraped Dates into the date_scraped cloumn 
mturk_df_dates$date_LegislatoR <- ifelse(is.na(mturk_df_dates$date_LegislatoR) == TRUE, as.Date(mturk_df_dates$date_scraped), as.Date(mturk_df_dates$date_LegislatoR) ) 

  

