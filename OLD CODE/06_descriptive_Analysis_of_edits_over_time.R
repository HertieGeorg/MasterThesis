#06_descriptive_Analysis_of_edits_over_time

# This analysis mainly focuses on the distribution of different types of edits over time

# in: main_dataframe_dates
# Main_dataframe_dates contains all classified edits of MPs proflies from IP-adressses from Congress and the dates of the respective edits

colnames(main_dataframe_dates)

# Libraries
library(tidyverse)
library(hrbrthemes)

data <- main_dataframe_dates


# All edits over time 

edits_30days <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=30, alpha=0.9) +
  ggtitle("Bin size = 30 Days") +
  theme_ipsum()
edits_30days

edits_60days <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=60, alpha=0.9) +
  ggtitle("Bin size = 60 Days") +
  theme_ipsum()
edits_60days


edits_90days <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=90, alpha=0.9) +
  ggtitle("Bin size = 90 Days") +
  theme_ipsum()
edits_90days


# Edits over time: just looking at edits that added content 


edits_300days <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=300, alpha=0.9) +
  ggtitle("Bin size = 300 Days") +
  theme_ipsum()
edits_300days


edits_9skkays <- data %>%
  filter(Answer.characteristics== "insults_mockery") %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=300, alpha=0.9) +
  ggtitle("Bin size = skk Days") +
  theme_ipsum()
edits_9skkays


unique(data$Answer.characteristics)


#---------------------------------
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

# Looking at the distributions of different motivations behind added content 
p <- data %>%
  ggplot( aes(x=date_LegislatoR, color= Answer.content_added  , fill=Answer.content_added)) +
  geom_histogram(alpha=0.6, binwidth = 300) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  facet_wrap(~Answer.content_added) +
  xlab("Date of Edit") +
  ylab("Number of Edits") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8))
p 



# Looking at the distributions of different motivations behind editing
p <- data %>%
  ggplot( aes(x=date_LegislatoR, color= Answer.characteristics  , fill=Answer.characteristics)) +
  geom_histogram(alpha=0.6, binwidth = 300) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  facet_wrap(~Answer.characteristics) +
  xlab("Date of Edit") +
  ylab("Number of Edits") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8))
p 



# For better insights the column should be entangled in different columns 

data2  <- dplyr::select(data, c(Answer.characteristics,  date_LegislatoR)) 

data2$corrections <- data2$Answer.characteristics
data2$reformatting <- data2$Answer.characteristics
data2$insults_mockery <- data2$Answer.characteristics
data2$mass_removal <- data2$Answer.characteristics
data2$praise_adulation <- data2$Answer.characteristics
data2$poor_style <- data2$Answer.characteristics


data2$corrections <- stringr::str_replace_all(data2$corrections, "(.*)corrections(.*)", "1")
data2$corrections <- stringr::str_replace_all(data2$corrections, "(.*)[[:alpha:]](.*)", "0")

data2$reformatting <- stringr::str_replace_all(data2$reformatting, "(.*)reformatting(.*)", "1")
data2$reformatting <- stringr::str_replace_all(data2$reformatting, "(.+)[[:alpha:]](.+)", "0")

data2$insults_mockery <- stringr::str_replace_all(data2$insults_mockery, "(.*)insults_mockery(.*)", "1")
data2$insults_mockery <- stringr::str_replace_all(data2$insults_mockery, "(.+)[[:alpha:]](.+)", "0")

data2$mass_removal <- stringr::str_replace_all(data2$mass_removal, "(.*)mass_removal(.*)", "1")
data2$mass_removal <- stringr::str_replace_all(data2$mass_removal, "(.+)[[:alpha:]](.+)", "0")

data2$praise_adulation <- stringr::str_replace_all(data2$praise_adulation, "(.*)reformatting(.*)", "1")
data2$praise_adulation <- stringr::str_replace_all(data2$praise_adulation, "(.+)[[:alpha:]](.+)", "0")

data2$poor_style <- stringr::str_replace_all(data2$poor_style, "(.*)poor_style(.*)", "1")
data2$poor_style <- stringr::str_replace_all(data2$poor_style, "(.+)[[:alpha:]](.+)", "0")




edits_corrections <- data2 %>%
  filter(corrections == 1) %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=200, alpha=0.9) +
  ggtitle("Corrections - Bin size = 200 Days") +
  theme_ipsum()
edits_corrections 


edits_reformatting <- data2 %>%
  filter(reformatting == 1) %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=200, alpha=0.9) +
  ggtitle("Reformatting - Bin size = 200 Days") +
  theme_ipsum()
edits_reformatting 

edits_insults_mockery <- data2 %>%
  filter(insults_mockery == 1) %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=200, alpha=0.9) +
  ggtitle("insults_mockery - Bin size = 200 Days") +
  theme_ipsum()
edits_insults_mockery

edits_mass_removal <- data2 %>%
  filter(mass_removal == 1) %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=200, alpha=0.9) +
  ggtitle("mass_removal - Bin size = 200 Days") +
  theme_ipsum()
edits_mass_removal

edits_praise_adulation <- data2 %>%
  filter(praise_adulation == 1) %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=200, alpha=0.9) +
  ggtitle("praise_adulation - Bin size = 200 Days") +
  theme_ipsum()
edits_praise_adulation

edits_poor_style <- data2 %>%
  filter(poor_style == 1) %>% 
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=200, alpha=0.9) +
  ggtitle("poor_style - Bin size = 200 Days") +
  theme_ipsum()
edits_poor_style



# Combining Edits over time with election dates 


data %>% 
  ggplot( aes(x=date, y=value)) +
  geom_line(color="#69b3a2") +
  ylim(0,22000) +
  annotate(geom="text", x=as.Date("2017-01-01"), y=20089, 
           label="Bitcoin price reached 20k $\nat the end of 2017") +
  annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=5000, color="orange", size=.5) +
  theme_ipsum()


edits_30days_elections <- data %>%
  ggplot( aes(x= date_LegislatoR)) +
  geom_histogram( binwidth=220, alpha=0.9) +
  ggtitle("Bin size = 220 Days") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_30days_elections  


# linetype="dotted"


edits_60days_elections <- data %>%
  ggplot( aes(x=date_LegislatoR)) +
  geom_histogram( binwidth=60, alpha=0.9) +
  ggtitle("Bin size = 60 Days") +
  theme_ipsum() +
  geom_vline(xintercept = list_of_biggest_general_election_dates, color = "orange", size=0.7) 
edits_60days_elections




#install.packages("fastDummies")
#library(fastDummies) 
#data2 <- data2 %>% drop_na(Answer.characteristics)
#data2 <- dummy_cols(data2)




