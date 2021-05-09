

# Goal: add biographie/characteristics data of MoCs to the examined edits from the Congress IT Network


# Input data: 
# 1. Inside_Congress_Edits_Politically_Dummies
# 2. Inside_Congress_Edits_Politically_PositiveNegative  
# 3. BothChambers_Session_109_to_114_Short



# Add politically_motivated column from Inside_Congress_Edits_Politically to Inside_Congress_Edits_Politically_PositiveNegative

#Version 1)
politicallyMotivated2 <-  Inside_Congress_Edits_Politically_PositiveNegative %>% dplyr::select(c("revid", "politically_motivated_positive", "politically_motivated_negative"))
df <- left_join( Inside_Congress_Edits_Politically_Dummies , politicallyMotivated2  , by = "revid")
# Add characteristics/biography data by left-join on pageid_session
df <-  left_join(df , BothChambers_Session_109_to_114_Short  , by = "pageid_session")

# Kirsten Gillibrand is the only MoC that served in both Chambers during the observed time frame and that see edits from within Congress
# As she only 23 days in the 111th session in the House, all edits will be associated with her term in the senate 
# Therefore, the un-needed two rows are dropped:
df <- df %>%  filter(service != 23)
#doubles <- df %>% group_by(revid) %>% filter(n()>1) # -> no doubles, each edit appears just once in the dataset




# Next Step: Visualizations of Many aspects of the Edit-Data 


# 1. Difference Democrats versus Republicans 


# 2. Difference Men versus Women 


# 3. Difference between competitive MoCs and less competitive MoCs


















#---------------------------------------------------------------------------------------
#------------------------------------- Ideenspeicher------------------------------------

#Old Version 
#politicallyMotivated <-  Inside_Congress_Edits_Politically %>% dplyr::select(c("revid", "politically_motivated"))
#df <- left_join(Inside_Congress_Edits_Politically_PositiveNegative , politicallyMotivated  , by = "revid")
# Add characteristics/biography data by left-join on pageid_session
#df <-  left_join(df , BothChambers_Session_109_to_114_Short  , by = "pageid_session")

# Kirsten Gillibrand is the only MoC that served in both Chambers during the observed time frame and that see edits from within Congress
# As she only 23 days in the 111th session in the House, all edits will be associated with her term in the senate 
# Therefore, the un-needed two rows are dropped:
#df <- df %>%  filter(service != 23)
#doubles <- df %>% group_by(revid) %>% filter(n()>1) # -> no doubles, each edit appears just once in the dataset

# Compare df and df_2

length(unique(df$revid))
length(unique(df$pageid.x))
table(is.na(df$pageid.x))
table(is.na(df$All_Politically_CongressEdits_Per_MoC_Session))
table(is.na(df$vote_maxdiff_relative))

length(unique(df_2$revid))
length(unique(df_2$pageid.x))
table(is.na(df_2$pageid.x))
table(is.na(df_2$All_Politically_CongressEdits_Per_MoC_Session))
table(is.na(df_2$vote_maxdiff_relative))

# -> are basically the same execpt of dummies, therefore we go with df_2






