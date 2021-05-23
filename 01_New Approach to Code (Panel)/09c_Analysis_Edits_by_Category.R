

# Goal: add biographie/characteristics data of MoCs to the examined edits from the Congress IT Network


# Input data: 
# 1. Inside_Congress_Edits_Politically_Dummies
# 2. Inside_Congress_Edits_Politically_PositiveNegative  
# 3. BothChambers_Session_109_to_114_Short

library(gridExtra)


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

# Create a Dummy for Cometitiveness

# Set NAs in competitiveness to median (which occur for US terretories. e.g. Puerto Rico, DC, etc.)
# Median not mean to not distrub the distribution 
summary(BothChambers_Session_109_to_114_Short$vote_maxdiff_relative)
for(i in 1:length(BothChambers_Session_109_to_114_Short$pageid)) {
  if (is.na(BothChambers_Session_109_to_114_Short$vote_maxdiff_relative[i]) ==  TRUE) {
    BothChambers_Session_109_to_114_Short$vote_maxdiff_relative[i] = 0.28742}
}



Vote_Diff_Mean <- BothChambers_Session_109_to_114_Short %>% dplyr::select(c(pageid, vote_maxdiff_relative))
Vote_Diff_Mean <- aggregate(Vote_Diff_Mean[, 2], list(Vote_Diff_Mean$pageid), mean)
Vote_Diff_Mean<- rename(Vote_Diff_Mean, "vote_maxdiff_relative_MoC_MEAN" = x)
Vote_Diff_Mean <- rename(Vote_Diff_Mean, "pageid.x" = Group.1)
summary(Vote_Diff_Mean$vote_maxdiff_relative_MoC_MEAN)
Vote_Diff_Mean$Competitive_Dummy <- 0

for(i in 1:length(Vote_Diff_Mean$pageid)) {
  if (Vote_Diff_Mean$vote_maxdiff_relative_MoC_MEAN[i] >=  0.401551) {
    Vote_Diff_Mean$Competitive_Dummy[i] = "Bottom25"}
  else if (Vote_Diff_Mean$vote_maxdiff_relative_MoC_MEAN[i] <=   0.149963) {
    Vote_Diff_Mean$Competitive_Dummy[i] = "Top25" 
  }
}

Pageid_Competitive <- Vote_Diff_Mean %>% dplyr::select(c( "pageid.x", "Competitive_Dummy"))

df <- left_join(df, Pageid_Competitive, by = "pageid.x")
# Also join with BothChambers_Session_109_to_114_Short (but just locally)
Pageid_Competitive <- rename(Pageid_Competitive, "pageid" = "pageid.x")
BothChambers_Session_109_to_114_Short <- left_join(BothChambers_Session_109_to_114_Short, Pageid_Competitive, by = "pageid")




# Functions:

# 1. Define function that returns table with shares of single categories 
get_distribution_table <- function(df_go, character, number) {
  NumCat <- length(df_go)
  MoC_Characteristic <- rep(character, NumCat)
  Share <- rep(NA, NumCat)
  Category <- rep(NA, NumCat)
  for (i in 1:NumCat) {
    m <- data.frame(table(df_go[i]))
    Share[i] <-  m[2,2]
    Category[i] <- colnames(df_go[i])
  }
  sum <- sum(Share)
  Share <- Share/number
  x <- data.frame(MoC_Characteristic, Category, Share) 
  return(x)
}

# 2. Define SECOND function that returns table with shares of single categories without taking a predefined number
get_distribution_table_SECOND <- function(df_go, character) {
  NumCat <- length(df_go)
  MoC_Characteristic <- rep(character, NumCat)
  Share <- rep(NA, NumCat)
  Category <- rep(NA, NumCat)
  for (i in 1:NumCat) {
    m <- data.frame(table(df_go[i]))
    Share[i] <-  m[2,2]
    Category[i] <- colnames(df_go[i])
  }
  sum <- sum(Share)
  Share <- Share/sum
  x <- data.frame(MoC_Characteristic, Category, Share) 
  return(x)
}




# Next Step: Visualizations of many aspects of the Edit-Data 


# --------------------------------------------------------------------------------------------------------------------------
#Figure4_AA_Categories: Showing distribution of congress edits over party and gender and chamber in general 
# --------------------------------------------------------------------------------------------------------------------------

chamberA <- data.frame(table(df$Chamber))
chamberR <- chamberA
N_H <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="H"]))
N_S <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="S"]))
chamberR[1,2] <- chamberA[1,2] /N_H
chamberR[2,2] <- chamberA[2,2] /N_S
chamberR$Freq <- round(chamberR$Freq, digits = 2)

sexA <- data.frame(table(df$sex)) 
sexR <- sexA
N_female <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="female"]))
N_male <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="male"]))
sexR[1,2] <- sexA[1,2] /N_female
sexR[2,2] <- sexA[2,2] /N_male
sexR$Freq <- round(sexR$Freq, digits = 2)



partyA <- data.frame(table(df$party_dual)) 
partyR <- partyA
N_D <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="D"]))
N_R <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="R"]))
partyR[1,2] <- partyA[1,2] /N_D
partyR[2,2] <- partyA[2,2] /N_R
partyR$Freq <- round(partyR$Freq, digits = 2)

CompetA <- data.frame(table(df$Competitive_Dummy)) 
CompetA <- CompetA[-1,]
CompetR <- CompetA
#Dividing by the number of MoC-Session pairs that fall into each category (independent whether they see an edits or not)
N_B <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Bottom25"]))
N_T <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Top25"]))
CompetR[1,2] <- CompetA[1,2] /N_B
CompetR[2,2] <- CompetA[2,2] /N_T
CompetR$Freq <- round(CompetR$Freq, digits = 2)


Figure4X1_Plot1 <- ggplot(chamberA , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Oranges",name = "Party") + 
  #ggtitle("Total Number of Congress Edits by Chamber") +
  xlab("Chamber") + 
  theme_classic() +
  ylab("Total Number of Edits") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "grey75","grey43")) +  
  scale_x_discrete(labels=c("House", "Senate")) +
  theme(legend.position = "none")

Figure4X1_Plot2 <-  ggplot(sexA , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Greens",name = "Party") + 
  #ggtitle("Total Number of Congress Edits by Gender") +
  xlab("Gender") + 
  theme_classic() +
  #ylab() +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c(  "darkolivegreen1","yellowgreen")) +  
  scale_x_discrete(labels=c("Female", "Male")) +
  theme(legend.position = "none", axis.title.y = element_blank())

Figure4X1_Plot3 <-  ggplot(CompetA , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Party") + 
  #ggtitle("Total Number of Congress Edits by Competitiveness") +
  xlab("MoC's election district competitiveness") + 
  theme_classic() +
  #ylab("none") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "plum", "orchid1")) +  
  scale_x_discrete(labels=c("Bottom 25%", "Top 25%")) +
  theme(legend.position = "none",  axis.title.y = element_blank())


Figure4X1_Plot4 <-  ggplot(partyA , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Party") + 
  #ggtitle("Total Number of Congress Edits  by Party") +
  xlab("Party") + 
  theme_classic() +
  #ylab("none") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "steelblue1", "indianred1")) +  
  scale_x_discrete(labels=c("Democrats", "Republicans")) +
  theme(legend.position = "none",  axis.title.y = element_blank())



Figure4X2_Plot1 <- ggplot(chamberR , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Greys",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Party") +
  xlab("Chamber") + 
  theme_classic() +
  ylab("Number of Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "grey75","grey43")) +  
  scale_x_discrete(labels=c("House", "Senate")) +
  theme(legend.position = "none")

Figure4X2_Plot2 <- ggplot(sexR , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Greens",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Party") +
  xlab("Gender") + 
  theme_classic() +
  #ylab("none") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c(  "darkolivegreen1","yellowgreen")) +  
  scale_x_discrete(labels=c("Female", "Male")) +
  theme(legend.position = "none",  axis.title.y = element_blank())

Figure4X2_Plot3 <-  ggplot(CompetR , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Set1",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Competitiveness") +
  xlab("MoC's election district competitiveness") + 
  theme_classic() +
  #ylab("none") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "plum", "orchid1")) +  
  scale_x_discrete(labels=c("Bottom 25%", "Top 25%")) +
  theme(legend.position = "none",  axis.title.y = element_blank())

Figure4X2_Plot4 <- ggplot(partyR , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Party") +
  xlab("Party") + 
  #ylab("none") +
  theme_classic() +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "steelblue1", "indianred1")) +  
  scale_x_discrete(labels=c("Democrats", "Republicans")) +
  theme(legend.position = "none",  axis.title.y = element_blank())

#Figure4_AA_1_Categories  
grid.arrange(Figure4X1_Plot1, Figure4X1_Plot2, Figure4X1_Plot3 ,Figure4X1_Plot4, ncol = 4, nrow = 1, top=("Total number of edits from the Congress IT network by chamber, gender, competitiveness and party"))
#Figure4_AA_2_Categories  
grid.arrange(Figure4X2_Plot1, Figure4X2_Plot2, Figure4X2_Plot3 ,Figure4X2_Plot4, ncol = 4, nrow = 1, top=("Number of edits from the Congress IT network per MoC by chamber, gender, competitiveness and party"))



# --------------------------------------------------------------------------------------------------------------------------
#Figure4_AB_Categories: Showing distribution of POLITICAL congress edits over party,gender, cometitiveness, chamber in general 
# --------------------------------------------------------------------------------------------------------------------------

df_P <- df %>%  filter(politically_motivated == 1)

chamber_P_A <- data.frame(table(df_P$Chamber))
chamber_P_R <- chamber_P_A
N_H <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="H"]))
N_S <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="S"]))
chamber_P_R[1,2] <- chamber_P_A[1,2] /N_H
chamber_P_R[2,2] <- chamber_P_A[2,2] /N_S
chamber_P_R$Freq <- round(chamber_P_R$Freq, digits = 2)

sex_P_A <- data.frame(table(df_P$sex)) 
sex_P_R <- sex_P_A
N_female <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="female"]))
N_male <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="male"]))
sex_P_R[1,2] <- sex_P_A[1,2] /N_female
sex_P_R[2,2] <- sex_P_A[2,2] /N_male
sex_P_R$Freq <- round(sex_P_R$Freq, digits = 2)

party_P_A <- data.frame(table(df_P$party_dual)) 
party_P_R <- party_P_A
N_D <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="D"]))
N_R <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="R"]))
party_P_R[1,2] <- party_P_A[1,2] /N_D
party_P_R[2,2] <- party_P_A[2,2] /N_R
party_P_R$Freq <- round(party_P_R$Freq, digits = 2)

Compet_P_A <- data.frame(table(df_P$Competitive_Dummy)) 
Compet_P_A <- Compet_P_A[-1,]
Compet_P_R <- Compet_P_A
N_B <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Bottom25"]))
N_T <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Top25"]))
Compet_P_R[1,2] <- Compet_P_A[1,2] /N_B
Compet_P_R[2,2] <- Compet_P_A[2,2] /N_T
Compet_P_R$Freq <- round(Compet_P_R$Freq, digits = 2)

Figure4_AB_1_Plot1 <- ggplot(chamber_P_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Greys",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Party") +
  xlab("Chamber") + 
  theme_classic() +
  ylab("Number of Political Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "grey75","grey43")) +  
  scale_x_discrete(labels=c("House", "Senate")) +
  theme(legend.position = "none")


Figure4_AB_1_Plot2 <- ggplot(sex_P_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Greens",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Party") +
  xlab("Gender") + 
  theme_classic() +
  #ylab("none") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c(  "darkolivegreen1","yellowgreen")) +  
  scale_x_discrete(labels=c("Female", "Male")) +
  theme(legend.position = "none",  axis.title.y = element_blank())

Figure4_AB_1_Plot3 <-  ggplot(Compet_P_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Set1",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Competitiveness") +
  xlab("MoC's election district competitiveness") + 
  theme_classic() +
  #ylab("none") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "plum", "orchid1")) +  
  scale_x_discrete(labels=c("Bottom 25%", "Top 25%")) +
  theme(legend.position = "none",  axis.title.y = element_blank())


Figure4_AB_1_Plot4 <- ggplot(party_P_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  scale_fill_brewer(palette = "Set1",name = "Party") + 
  #ggtitle("Number of Congress Edits per MoC by Party") +
  xlab("Party") + 
  #ylab("none") +
  theme_classic() +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "steelblue1", "indianred1")) +  
  scale_x_discrete(labels=c("Democrats", "Republicans")) +
  theme(legend.position = "none",  axis.title.y = element_blank())

#Figure4_AB_Categories
grid.arrange(Figure4_AB_1_Plot1, Figure4_AB_1_Plot2, Figure4_AB_1_Plot3,Figure4_AB_1_Plot4, ncol = 4, nrow = 1, top=("Number of political edits from the Congress IT network per MoC by chamber, gender, competitiveness and party"))




# --------------------------------------------------------------------------------------------------------------------------
# Figure4_AC_1_Categories: Distribution of ALL Congress Edits over 4 broad categories by chamber, party, competiv and gender 
# --------------------------------------------------------------------------------------------------------------------------


df_D <- df %>% filter(party_dual == "D") #%>% filter(Answer.topic_personal_religion_dummy ==  1)
df_R <- df %>% filter(party_dual == "R") # %>% filter(Answer.topic_personal_religion_dummy ==  1)

df_F <- df %>% filter(sex == "female") #%>% filter(Answer.topic_personal_religion_dummy ==  1)
df_M <- df %>% filter(sex == "male") # %>% filter(Answer.topic_personal_religion_dummy ==  1)

df_H <- df %>% filter(Chamber == "H") #%>% filter(Answer.topic_personal_religion_dummy ==  1)
df_S <- df %>% filter(Chamber == "S") # %>% filter(Answer.topic_personal_religion_dummy ==  1)

df_T <- df %>% filter(Competitive_Dummy == "Top25") #%>% filter(Answer.topic_personal_religion_dummy ==  1)
df_B <- df %>% filter(Competitive_Dummy == "Bottom25") # %>% filter(Answer.topic_personal_religion_dummy ==  1)


four_broad_categroies <- c("Answer.topic_personal_dummy","Answer.topic_career_dummy","Answer.topic_views_dummy", "Answer.topic_other_dummy" ) 

df_D_BroadC <- df_D %>% dplyr::select(four_broad_categroies) 
df_R_BroadC <- df_R %>% dplyr::select(four_broad_categroies)

df_F_BroadC <- df_F %>% dplyr::select(four_broad_categroies) 
df_M_BroadC <- df_M %>% dplyr::select(four_broad_categroies)

df_H_BroadC <- df_H %>% dplyr::select(four_broad_categroies) 
df_S_BroadC <- df_S %>% dplyr::select(four_broad_categroies)

df_T_BroadC <- df_T %>% dplyr::select(four_broad_categroies) 
df_B_BroadC <- df_B %>% dplyr::select(four_broad_categroies)

X_D_Broad <- get_distribution_table(df_D_BroadC, "D", 1090)
X_R_Broad <- get_distribution_table(df_R_BroadC, "R", 1358)
Party_All_Broad <- rbind(X_D_Broad, X_R_Broad)

X_F_Broad <- get_distribution_table(df_F_BroadC, "Female", 347)
X_M_Broad <- get_distribution_table(df_M_BroadC, "Male", 2101)
Sex_All_Broad <- rbind(X_F_Broad, X_M_Broad)

X_H_Broad <- get_distribution_table(df_H_BroadC, "House", 2036)
X_S_Broad <- get_distribution_table(df_S_BroadC, "Senate", 412)
Chamber_All_Broad <- rbind(X_H_Broad, X_S_Broad)

X_T_Broad <- get_distribution_table(df_T_BroadC, "Top 25%", 345)
X_B_Broad <- get_distribution_table(df_B_BroadC, "Bottom 25%", 779)
Compet_All_Broad <- rbind(X_T_Broad, X_B_Broad)


labels_Broad <- c("Personal life", "Career", "Political positions", "Other")

#Figure4_AC_1_Plot1
Figure4_AC_1_Plot1 <- ggplot(Chamber_All_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by chamber (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all chamber edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("House", "Senate"), values = c(  "grey75","grey43")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#Figure4_AC_1_Plot2
Figure4_AC_1_Plot2 <- ggplot(Sex_All_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by gender (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all gender edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#Figure4_AC_1_Plot3
Figure4_AC_1_Plot3 <- ggplot(Compet_All_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by competitiveness (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all competitive-group edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Bottom 25% ", "Top 25%"), values = c( "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#Figure4_AC_1_Plot4
Figure4_AC_1_Plot4 <- ggplot(Party_All_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by party (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all party edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Democrats", "Republicans"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#Figure4_AC_1a_Categories
grid.arrange(Figure4_AC_1_Plot1, Figure4_AC_1_Plot2, Figure4_AC_1_Plot3 , Figure4_AC_1_Plot4, ncol = 4, nrow = 1,
             top=("Distribution of all edits from the Congress IT network over broad categories by chamber, gender, competitiveness and party"))


#------- Alternative Specification of the samen graphs with other function----
# Difference: instead of e.g total number of edits that target females, here the total sum of the numbers of edits that fall into the four categories is used:


X_D_Broadx <- get_distribution_table_SECOND(df_D_BroadC, "D")
X_R_Broadx <- get_distribution_table_SECOND(df_R_BroadC, "R")
Party_All_Broadx <- rbind(X_D_Broadx, X_R_Broadx)

X_F_Broadx <- get_distribution_table_SECOND(df_F_BroadC, "Female")
X_M_Broadx <- get_distribution_table_SECOND(df_M_BroadC, "Male")
Sex_All_Broadx <- rbind(X_F_Broadx, X_M_Broadx)

X_H_Broadx <- get_distribution_table_SECOND(df_H_BroadC, "House")
X_S_Broadx <- get_distribution_table_SECOND(df_S_BroadC, "Senate")
Chamber_All_Broadx <- rbind(X_H_Broadx, X_S_Broadx)

X_T_Broadx <- get_distribution_table_SECOND(df_T_BroadC, "Top 25%")
X_B_Broadx <- get_distribution_table_SECOND(df_B_BroadC, "Bottom 25%")
Compt_All_Broadx <- rbind(X_T_Broadx, X_B_Broadx)


labels_Broad <- c("Personal life", "Career", "Political positions", "Other")

#Figure4_AC_1_Plot1x
Figure4_AC_1_Plot1x <- ggplot(Chamber_All_Broadx, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by chamber (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all chamber edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("House", "Senate"), values = c(  "grey75","grey43")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#Figure4_AC_1_Plot2x
Figure4_AC_1_Plot2x <- ggplot(Sex_All_Broadx, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by gender (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all gender edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#Figure4_AC_1_Plot3x
Figure4_AC_1_Plot3x <- ggplot(Compt_All_Broadx, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by competitiveness (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all competitive-group edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Bottom 25%", "Top 25%"), values = c( "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4_AC_1_Plot4x
Figure4_AC_1_Plot4x <- ggplot(Party_All_Broadx, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Edits from the Congress IT network by party (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all party edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Democrats", "Republicans"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#Figure4_AC_1b_Categories
grid.arrange(Figure4_AC_1_Plot1x, Figure4_AC_1_Plot2x, Figure4_AC_1_Plot3x ,Figure4_AC_1_Plot4x, ncol = 4, nrow = 1,
             top=("Distribution of all edits from the Congress IT network over broad categories by chamber, gender, competitiveness and party (Other dividing-mechanism)"))







# -----------------------------------------------------------------------------------------------------------------------------
#Figure4_AC_2_Categories: Distribution of POLITICAL Congress Edits over 4 broad categories by chamber, party, competiv, gender 
# -----------------------------------------------------------------------------------------------------------------------------


df_DP <- df %>% filter(party_dual == "D") %>% filter(politically_motivated == 1) 
df_RP <- df %>% filter(party_dual == "R") %>% filter(politically_motivated == 1)

df_FP <- df %>% filter(sex == "female") %>% filter(politically_motivated == 1) 
df_MP <- df %>% filter(sex == "male") %>% filter(politically_motivated == 1)

df_HP <- df %>% filter(Chamber == "H") %>% filter(politically_motivated == 1) 
df_SP <- df %>% filter(Chamber == "S") %>% filter(politically_motivated == 1)

df_TP <- df %>% filter(Competitive_Dummy == "Top25") %>% filter(politically_motivated == 1) 
df_BP <- df %>% filter(Competitive_Dummy == "Bottom25") %>% filter(politically_motivated == 1)


df_DP_BroadC <- df_DP %>% dplyr::select(four_broad_categroies) 
df_RP_BroadC <- df_RP %>% dplyr::select(four_broad_categroies)

df_FP_BroadC <- df_FP %>% dplyr::select(four_broad_categroies) 
df_MP_BroadC <- df_MP %>% dplyr::select(four_broad_categroies)

df_HP_BroadC <- df_HP %>% dplyr::select(four_broad_categroies) 
df_SP_BroadC <- df_SP %>% dplyr::select(four_broad_categroies)

df_TP_BroadC <- df_TP %>% dplyr::select(four_broad_categroies) 
df_BP_BroadC <- df_BP %>% dplyr::select(four_broad_categroies)


X_DP_Broad <- get_distribution_table_SECOND(df_DP_BroadC, "D") #Number of Edits: 567
X_RP_Broad <- get_distribution_table_SECOND(df_RP_BroadC, "R") #Number of Edits: 806
Party_Political_Broad <- rbind(X_DP_Broad, X_RP_Broad)

X_FP_Broad <- get_distribution_table_SECOND(df_FP_BroadC, "Female") #Number of Edits: 186
X_MP_Broad <- get_distribution_table_SECOND(df_MP_BroadC, "Male") #Number of Edits: 1187
Sex_Political_Broad <- rbind(X_FP_Broad, X_MP_Broad)

X_HP_Broad <- get_distribution_table_SECOND(df_HP_BroadC, "House") #Number of Edits: 1145
X_SP_Broad <- get_distribution_table_SECOND(df_SP_BroadC, "Senate") #Number of Edits: 228
Chamber_Political_Broad <- rbind(X_HP_Broad, X_SP_Broad)

X_TP_Broad <- get_distribution_table_SECOND(df_TP_BroadC, "Top 25%") #Number of Edits: 1145
X_BP_Broad <- get_distribution_table_SECOND(df_BP_BroadC, "Bottom 25%") #Number of Edits: 228
Compt_Political_Broad <- rbind(X_TP_Broad, X_BP_Broad)


#Figure4_AC_2_Plot1
Figure4_AC_2_Plot1 <- ggplot(Chamber_Political_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Political Edits from the Congress IT network by chamber (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political chamber edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("House", "Senate"), values = c(  "grey75","grey43")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4_AC_2_Plot2
Figure4_AC_2_Plot2 <- ggplot(Sex_Political_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Political Edits from the Congress IT network by gender (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political gender edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#Figure4_AC_2_Plot3
Figure4_AC_2_Plot3 <- ggplot(Compt_Political_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Political Edits from the Congress IT network by party (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political competitive edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Bottom 25%", "Top 25%"), values = c( "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4_AC_2_Plot4
Figure4_AC_2_Plot4 <- ggplot(Party_Political_Broad, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Political Edits from the Congress IT network by party (four broad categories)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political party edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_Broad )  +
  scale_fill_manual(labels = c("Democrats", "Republicans"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#Figure4_AC_2_Categories
grid.arrange(Figure4_AC_2_Plot1, Figure4_AC_2_Plot2, Figure4_AC_2_Plot3, Figure4_AC_2_Plot4 , ncol = 4, nrow = 1,
             top=("Distribution of political edits from the Congress IT network over broad categories by chamber, gender, competitiveness and party"))





# --------------------------------------------------------------------------------------------------------------------------
#Figure4a_Categories:  All edits  + All categories from Personal Life -> D versus R in one Barplot 
# --------------------------------------------------------------------------------------------------------------------------



df_D <- df %>% filter(party_dual == "D") #%>% filter(Answer.topic_personal_religion_dummy ==  1)
df_R <- df %>% filter(party_dual == "R") # %>% filter(Answer.topic_personal_religion_dummy ==  1)

personal_categories <- c("Answer.topic_personal_religion_dummy", "Answer.topic_personal_financial_earnings_dummy", 
                         "Answer.topic_personal_family_current_life_dummy" , "Answer.topic_personal_character_dummy" ,"Answer.topic_personal_early_life_dummy",        
                         "Answer.topic_personal_achievements_awards_dummy","Answer.topic_personal_activities_memberships_dummy")


df_D_Personal <- df_D %>% dplyr::select(personal_categories) 
df_R_Personal <- df_R %>% dplyr::select(personal_categories)



X_D <- get_distribution_table_SECOND(df_D_Personal, "D") # Number of Edits: 1090
X_R <- get_distribution_table_SECOND(df_R_Personal, "R") # Number of Edits: 1358
Party_All <- rbind(X_D, X_R)

labels_PersonalLife <- c("Achievements/Awards", "Activities/Membershios", 
                         "Personal Character", "Early Life",
                         "Family and current life", "Financial erarnings",
                         "Religion")

#Figure4a_Categories
Figure4a_Categories <- ggplot(Party_All, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Edits from the Congress IT network by party (Personal life)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all party edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels = c("Democrats", "Republicans"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) 
  #+labs(fill = "Party") 
  


# --------------------------------------------------------------------------------------------------------------------------
#Figure4b_Categories:  Political edits  + All categories from Personal Life -> D versus R in one Barplot
# --------------------------------------------------------------------------------------------------------------------------


df_DP <- df %>% filter(party_dual == "D") %>%  filter(politically_motivated == 1) 
df_RP <- df %>% filter(party_dual == "R") %>%  filter(politically_motivated == 1)


df_DP_Personal <- df_DP %>% dplyr::select(personal_categories) 
df_RP_Personal <- df_RP %>% dplyr::select(personal_categories)


X_DP <- get_distribution_table_SECOND(df_DP_Personal, "D") #1090
X_RP <- get_distribution_table_SECOND(df_RP_Personal, "R") #1358
Party_Political <- rbind(X_DP, X_RP)


labels_PersonalLife <- c("Achievements/Awards", "Activities/Memberships", 
                         "Personal Character", "Early Life",
                         "Family and current life", "Financial earnings",
                         "Religion")

#Figure4b_Categories
Figure4b_Categories <- ggplot(Party_Political, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Political edits from the Congress IT network by party (Personal life)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political party edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels = c("Democrats", "Republicans"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


grid.arrange(Figure4a_Categories , Figure4b_Categories , ncol = 2, nrow = 1,
             top=("All edits and political edits from the Congress IT network by party (Personal life)"))





# --------------------------------------------------------------------------------------------------------------------------
#Figure4c_Categories:  All edits  + All categories from Personal Life -> Male versus Female in one Barplot 
# --------------------------------------------------------------------------------------------------------------------------

df_F_Personal <- df_F %>% dplyr::select(personal_categories)
df_M_Personal <- df_M %>% dplyr::select(personal_categories)

X_F_Personal2 <- get_distribution_table(df_F_Personal, "Female",107) #distribution within category
X_M_Personal2 <- get_distribution_table(df_M_Personal, "Male", 926) #distribution within category
Sex_All_Personal2 <- rbind(X_F_Personal2, X_M_Personal2)


#Figure4c_Plot2
Figure4c_Plot2 <- ggplot(Sex_All_Personal2 , aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("All edits from the Congress IT network by gender (Personal life) (devided by total number of edits WITHIN PERSONAl per gender") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of gender edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#--------------------------------------------------------------------------------------------------------------------------
# Figure4d_Categories:  Political edits  + All categories from Personal Life -> Male versus Female in one Barplot
# --------------------------------------------------------------------------------------------------------------------------

df_FP_Personal <- df_FP %>% dplyr::select(personal_categories)
df_MP_Personal <- df_MP %>% dplyr::select(personal_categories)


X_FP_Personal2 <- get_distribution_table(df_FP_Personal, "Female",74) #distribution within category
X_MP_Personal2 <- get_distribution_table(df_MP_Personal, "Male", 649) #distribution within category
Sex_Political_Personal2 <- rbind(X_FP_Personal2, X_MP_Personal2)


#Figure4d_Plot2
Figure4d_Plot2 <- ggplot(Sex_Political_Personal2 , aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Political edits from the Congress IT network by gender (Personal life) (devided by total number of edits WITHIN PERSONAl per gender") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political gender edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#Figure4d_Categories
grid.arrange(Figure4c_Plot2 , Figure4d_Plot2, ncol = 2, nrow = 1,
             top=("Distribution of political edits from the Congress IT network over personal categories gender (3 different benchmarks"))




# --------------------------------------------------------------------------------------------------------------------------
#Figure4Compt1_Categories:  All edits  + All categories from Personal Life -> Top25 versus Bottom25 in one Barplot 
# --------------------------------------------------------------------------------------------------------------------------


df_T_Personal <- df_T %>% dplyr::select(personal_categories) 
df_B_Personal <- df_B %>% dplyr::select(personal_categories)


X_T <- get_distribution_table_SECOND(df_T_Personal, "Top 25%") 
X_B <- get_distribution_table_SECOND(df_B_Personal, "Bottom 25%") 
Compt_All <- rbind(X_T, X_B)


#Figure4Compt1_Categories
Figure4Compt1_Categories <- ggplot(Compt_All, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Edits from the Congress IT network by competitiveness (Personal life)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all competitive-groups edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels =c("Bottom 25%", "Top 25%"), values = c(  "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) 
#+labs(fill = "Party") 



# --------------------------------------------------------------------------------------------------------------------------
#Figure4Compt2_Categories:   POLITICAL edits  + All categories from Personal Life -> Top25 versus Bottom25 in one Barplot 
# --------------------------------------------------------------------------------------------------------------------------


df_TP <- df %>% filter(Competitive_Dummy == "Top25") %>%  filter(politically_motivated == 1) 
df_BP <- df %>% filter(Competitive_Dummy == "Bottom25") %>%  filter(politically_motivated == 1)

df_TP_Personal <- df_TP %>% dplyr::select(personal_categories) 
df_BP_Personal <- df_BP %>% dplyr::select(personal_categories)

X_TP <- get_distribution_table_SECOND(df_TP_Personal, "Top25") 
X_BP <- get_distribution_table_SECOND(df_BP_Personal, "Bottom25") 
Compt_Political <- rbind(X_TP, X_BP)


#FigureCompt2_Categories
Figure4Compt2_Categories <- ggplot(Compt_Political, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Political edits from the Congress IT network by competitiveness (Personal life)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political competitive-groups edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels =c("Bottom 25%", "Top 25%"), values = c(  "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 

#FigureCompt_Categories
grid.arrange(Figure4Compt1_Categories ,Figure4Compt2_Categories, ncol = 2, nrow = 1,
             top=("Distribution of overall and political edits from the Congress IT network over personal categories by competitiveness (25%)"))




## --------------------------------------------------------------------------------------------------------------------------
#Figure4Compt3_Categories: Overall edits and political edits PERSONAL by competitiveness 50-50
# --------------------------------------------------------------------------------------------------------------------------

# Create dummy for 50 top - 50 bottom 

Vote_Diff_Mean <- BothChambers_Session_109_to_114_Short %>% dplyr::select(c(pageid, vote_maxdiff_relative))
Vote_Diff_Mean <- aggregate(Vote_Diff_Mean[, 2], list(Vote_Diff_Mean$pageid), mean)
Vote_Diff_Mean<- rename(Vote_Diff_Mean, "vote_maxdiff_relative_MoC_MEAN" = x)
Vote_Diff_Mean <- rename(Vote_Diff_Mean, "pageid.x" = Group.1)
summary(Vote_Diff_Mean$vote_maxdiff_relative_MoC_MEAN)

Vote_Diff_Mean$Competitive_Dummy50 <- 0

for(i in 1:length(Vote_Diff_Mean$pageid)) {
  if (Vote_Diff_Mean$vote_maxdiff_relative_MoC_MEAN[i] >=  0.26044) {
    Vote_Diff_Mean$Competitive_Dummy50[i] = "Bottom50"}
  else if (Vote_Diff_Mean$vote_maxdiff_relative_MoC_MEAN[i] < 0.26044) {
    Vote_Diff_Mean$Competitive_Dummy50[i] = "Top50" 
  }
}

Pageid_Competitive <- Vote_Diff_Mean %>% dplyr::select(c( "pageid.x", "Competitive_Dummy50"))
df <- left_join(df, Pageid_Competitive, by = "pageid.x")

# Create Figures for overall and political edits 

df_T50 <- df %>% filter(Competitive_Dummy50 == "Top50")
df_B50 <- df %>% filter(Competitive_Dummy50 == "Bottom50")
df_T50_Personal <- df_T50 %>% dplyr::select(personal_categories) 
df_B50_Personal <- df_B50 %>% dplyr::select(personal_categories)
X_T50 <- get_distribution_table_SECOND(df_T50_Personal, "Top 25%") 
X_B50 <- get_distribution_table_SECOND(df_B50_Personal, "Bottom 25%") 
Compt_All50 <- rbind(X_T50, X_B50)

#Figure4Compt3a_Categories
Figure4Compt3a_Categories <- ggplot(Compt_All50, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Edits from the Congress IT network by competitiveness (Personal life)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all competitive-groups edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels =c("Bottom 25%", "Top 25%"), values = c(  "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) 
#+labs(fill = "Party") 


df_TP50 <- df_T50 %>%  filter(politically_motivated == 1) 
df_BP50 <- df_B50  %>%  filter(politically_motivated == 1)
df_TP_Personal50 <- df_TP50 %>% dplyr::select(personal_categories) 
df_BP_Personal50 <- df_BP50 %>% dplyr::select(personal_categories)
X_TP50 <- get_distribution_table_SECOND(df_TP_Personal50, "Top25") 
X_BP50 <- get_distribution_table_SECOND(df_BP_Personal50, "Bottom25") 
Compt_Political50 <- rbind(X_TP50, X_BP50)

#FigureCompt3b_Categories
Figure4Compt3b_Categories <- ggplot(Compt_Political50, aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Political edits from the Congress IT network by competitiveness (Personal life)") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of political competitive-groups edits ") +
  scale_x_discrete(guide = guide_axis(angle = 45), labels= labels_PersonalLife)  +
  scale_fill_manual(labels =c("Bottom 25%", "Top 25%"), values = c(  "plum", "orchid1")) + 
  theme(legend.position = c(0.85, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#FigureComt3_Categories
grid.arrange(Figure4Compt3a_Categories ,Figure4Compt3b_Categories, ncol = 2, nrow = 1,
             top=("Distribution of overall and political edits from the Congress IT network over personal categories by competitiveness (50%)"))


# --------------------------------------------------------------------------------------------------------------------------
#Figure4e_Categories:  Beneficial edits versus Harmful -> male versus female 
# --------------------------------------------------------------------------------------------------------------------------

# grid with 3 Plots 

#1 Harmful (F vs M) divided by number of total female and male edits + Beneficial (F vs M) divided by number of total female and male edits 
#2 Percentage of individual Females and Males that see at least 1 harmful edit
#3 Percentage of individual Females and Males that see at least 1 beneficial edit


political_categories = c( "politically_motivated_positive" , "politically_motivated_negative")


df_F_PC <- df_F %>% dplyr::select(political_categories)
df_M_PC <- df_M %>% dplyr::select(political_categories)


X_F_PC <- get_distribution_table(df_F_PC, "Female",347) #number of edits on profiles of female MoCs
X_M_PC <- get_distribution_table(df_M_PC, "Male", 2101 )#number of edits on profiles of male MoCs

Sex_PC <- rbind(X_F_PC, X_M_PC)

label_polCategories <- c("Harmful", "Beneficial")

#Figure4e_Plot1
Figure4e_Plot1 <- ggplot(Sex_PC , aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Harmful and Beneficial Political edits from the Congress IT network by gender ") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all gender edits ") +
  scale_x_discrete(guide = guide_axis(angle =0), labels= label_polCategories)  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#2 Percentage of individual Females and Males that see at least 1 harmful edit
#3 Percentage of individual Females and Males that see at least 1 beneficial edit

df_F_PCB <- df_F %>% filter(politically_motivated_positive ==1)
df_F_PCH <- df_F %>% filter(politically_motivated_negative ==1)

df_M_PCB <- df_M %>% filter(politically_motivated_positive ==1)
df_M_PCH <- df_M %>% filter(politically_motivated_negative ==1)


# Total number of females in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex == "female"])) #168 females -> that is crazy low btw
# Total number of males in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex == "male"])) #813 males


length(unique(df_F_PCB$pageid.x)) #61
61/168
length(unique(df_F_PCH$pageid.x)) #11
11/168
length(unique(df_M_PCB$pageid.x)) #338
388/813
length(unique(df_M_PCH$pageid.x)) #75
75/813

data_HarmBeneSex <- data.frame(MoC_Characteristic = c("Male", "Male", "Female", "Female"),
           Category = c("Harmful", "Beneficial","Harmful","Beneficial"),
           Share = c(75/813, 388/813, 11/168, 61/168))



#Figure4e_Plot2
Figure4e_Plot2 <- ggplot(data_HarmBeneSex , aes(x=fct_rev(fct_infreq(Category)), y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Share of MoCs that see at least one harmful or beneficial political edit by gender ") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of MoCs that see respective type of edits by gender") +
  #scale_x_discrete(guide = guide_axis(angle = 45), labels= label_polCategories)  +
  scale_fill_manual(labels = c("Female", "Male"), values = c( "darkolivegreen1","yellowgreen")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4e_Categories
grid.arrange(Figure4e_Plot1, Figure4e_Plot2, ncol = 2, nrow = 1,
             top=("Distribution of harmful and beneficial political edits by gender"))


# --------------------------------------------------------------------------------------------------------------------------
#Figure4f_Categories:  Beneficial edits versus Harmful -> democrats versus republicans 
# --------------------------------------------------------------------------------------------------------------------------


df_D_PC <- df_D %>% dplyr::select(political_categories)
df_R_PC <- df_R %>% dplyr::select(political_categories)


X_D_PC <- get_distribution_table(df_D_PC, "Democrats",1090) #number of edits on profiles of democratic MoCs
X_R_PC <- get_distribution_table(df_R_PC, "Republicans", 1358 )#number of edits on profiles of republican MoCs

Party_PC <- rbind(X_D_PC, X_R_PC)


#Figure4f_Plot1
Figure4f_Plot1 <- ggplot(Party_PC , aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Percentage of Harmful and Beneficial Political edits on all edits by party") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all party edits ") +
  scale_x_discrete(guide = guide_axis(angle =0), labels= label_polCategories)  +
  scale_fill_manual(labels = c("Democratic", "Republican"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#2 Percentage of individual Females and Males that see at least 1 harmful edit
#3 Percentage of individual Females and Males that see at least 1 beneficial edit

df_D_PCB <- df_D %>% filter(politically_motivated_positive ==1)
df_D_PCH <- df_D %>% filter(politically_motivated_negative ==1)

df_R_PCB <- df_R %>% filter(politically_motivated_positive ==1)
df_R_PCH <- df_R %>% filter(politically_motivated_negative ==1)


# Total number of democrats in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual == "D"])) #451
# Total number of republicans in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual == "R"])) #530


length(unique(df_D_PCB$pageid.x)) 
187/451
length(unique(df_D_PCH$pageid.x)) 
37/451
length(unique(df_R_PCB$pageid.x)) 
212/530
length(unique(df_R_PCH$pageid.x)) 
49/530

data_HarmBeneParty <- data.frame(MoC_Characteristic = c("Democratic", "Democratic", "Republican", "Republican"),
                            Category = c("Harmful", "Beneficial","Harmful","Beneficial"),
                            Share = c(37/451, 187/451, 49/530, 212/530))



#Figure4f_Plot2
Figure4f_Plot2 <- ggplot(data_HarmBeneParty , aes(x=fct_rev(fct_infreq(Category)), y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #ggtitle("Share of MoCs that see at least one harmful or beneficial political edit by party ") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of MoCs that see respective type of edits by party") +
  #scale_x_discrete(guide = guide_axis(angle = 45), labels= label_polCategories)  +
  scale_fill_manual(labels = c("Democratic", "Republican"), values = c( "steelblue1", "indianred1")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4e_Categories
grid.arrange(Figure4f_Plot1, Figure4f_Plot2, ncol = 2, nrow = 1,
             top=("Distribution of harmful and beneficial political edits by party"))




# --------------------------------------------------------------------------------------------------------------------------
#Figure4g1_Categories:  Beneficial edits versus Harmful -> house versus senate
# --------------------------------------------------------------------------------------------------------------------------


df_H_PC <- df_H %>% dplyr::select(political_categories)
df_S_PC <- df_S %>% dplyr::select(political_categories)


X_H_PC <- get_distribution_table(df_H_PC, "House",2036) #number of edits on profiles of House MoCs
X_S_PC <- get_distribution_table(df_S_PC, "Senate", 412 )#number of edits on profiles of Senate MoCs

Chamber_PC <- rbind(X_H_PC, X_S_PC)


#Figure4g1_Plot1
Figure4g1_Plot1 <- ggplot(Chamber_PC , aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Percentage of Harmful and Beneficial Political edits on all edits by chamber") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all chamber edits ") +
  scale_x_discrete(guide = guide_axis(angle =0), labels= label_polCategories)  +
  scale_fill_manual(labels = c("House", "Senate"), values = c(  "grey75","grey43")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#2 Percentage of individual Senators and MoH that see at least 1 harmful edit
#3 Percentage of individual Senators and MoH that see at least 1 beneficial edit

df_H_PCB <- df_H %>% filter(politically_motivated_positive ==1)
df_H_PCH <- df_H %>% filter(politically_motivated_negative ==1)

df_S_PCB <- df_S %>% filter(politically_motivated_positive ==1)
df_S_PCH <- df_S %>% filter(politically_motivated_negative ==1)


# Total number of democrats in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber == "H"])) #830
# Total number of republicans in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber == "S"])) #180


length(unique(df_H_PCB$pageid.x)) 
321/830
length(unique(df_H_PCH$pageid.x)) 
62/830
length(unique(df_S_PCB$pageid.x)) 
80/180
length(unique(df_S_PCH$pageid.x)) 
24/180

data_HarmBeneChamber <- data.frame(MoC_Characteristic = c("House", "House", "Senate", "Senate"),
                                 Category = c("Harmful", "Beneficial","Harmful","Beneficial"),
                                 Share = c(62/830, 321/830, 24/180, 80/180))



#Figure4g1_Plot2
Figure4g1_Plot2 <- ggplot(data_HarmBeneChamber , aes(x=fct_rev(fct_infreq(Category)), y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Share of MoCs that see at least one harmful or beneficial political edit by chamber") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of MoCs that see respective type of edits by chamber") +
  #scale_x_discrete(guide = guide_axis(angle = 45), labels= label_polCategories)  +
  scale_fill_manual(labels = c("House", "Senate"), values = c(  "grey75","grey43")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4g1_Categories
grid.arrange(Figure4g1_Plot1, Figure4g1_Plot2, ncol = 2, nrow = 1,
             top=("Distribution of harmful and beneficial political edits by chamber"))



# --------------------------------------------------------------------------------------------------------------------------
#Figure4g2_Categories:  Beneficial edits versus Harmful -> top25 versus bottom25 (for appendix)
# --------------------------------------------------------------------------------------------------------------------------


df_T_PC <- df_T %>% dplyr::select(political_categories)
df_B_PC <- df_B %>% dplyr::select(political_categories)


X_T_PC <- get_distribution_table(df_T_PC, "Top 25%",779) #number of edits on profiles of Top25% competitive MoCs
X_B_PC <- get_distribution_table(df_B_PC, "Bottom 25%", 345 )#number of edits on profiles of Bottom25% competitive MoCs


Compt_PC <- rbind(X_T_PC, X_B_PC)


#Figure4g2_Plot1
Figure4g2_Plot1 <- ggplot(Compt_PC , aes(x=Category, y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Percentage of Harmful and Beneficial Political edits on all edits by competitive groups") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of all competitive-groups edits ") +
  scale_x_discrete(guide = guide_axis(angle =0), labels= label_polCategories)  +
  scale_fill_manual(labels = c("Bottom 25%", "Top 25%"), values = c(  "plum", "orchid1")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 



#2 Percentage of individual MoCs in both competitiveness groups that see at least 1 harmful edit
#3 Percentage of individual  MoCs in both competitiveness groups that see at least 1 beneficial edit

df_T_PCB <- df_T %>% filter(politically_motivated_positive ==1)
df_T_PCH <- df_T %>% filter(politically_motivated_negative ==1)

df_B_PCB <- df_B %>% filter(politically_motivated_positive ==1)
df_B_PCH <- df_B %>% filter(politically_motivated_negative ==1)


# Total number of democrats in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Competitive_Dummy == "Top25"])) #246
# Total number of republicans in oberserved time frame
length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Competitive_Dummy == "Bottom25"])) #245



length(unique(df_T_PCB$pageid.x)) 
121/246
length(unique(df_T_PCH$pageid.x)) 
22/246
length(unique(df_B_PCB$pageid.x)) 
73/245
length(unique(df_B_PCH$pageid.x)) 
17/245

data_HarmBeneCompt <- data.frame(MoC_Characteristic = c("Top 25%", "Top 25%", "Bottom 25%", "Bottom 25%"),
                                   Category = c("Harmful", "Beneficial","Harmful","Beneficial"),
                                   Share = c(22/246, 121/246, 17/245, 73/245))



#Figure4g2_Plot2
Figure4g2_Plot2 <- ggplot(data_HarmBeneCompt , aes(x=fct_rev(fct_infreq(Category)), y=Share, fill=MoC_Characteristic,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  ggtitle("Share of MoCs that see at least one harmful or beneficial political edit by competitiveness") +
  xlab("Category") + 
  theme_bw() + 
  ylab("Share of MoCs that see respective type of edits by competitiveness") +
  #scale_x_discrete(guide = guide_axis(angle = 45), labels= label_polCategories)  +
  scale_fill_manual(labels = c("Bottom 25%", "Top 25%"), values = c(  "plum", "orchid1")) + 
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(), axis.title.x = element_blank(),  legend.background = element_rect(fill = "grey95")) #+labs(fill = "Party") 


#Figure4g1_Categories
grid.arrange(Figure4g2_Plot1, Figure4g2_Plot2, ncol = 2, nrow = 1,
             top=("Distribution of harmful and beneficial political edits by competitiveness"))



# --------------------------------------------------------------------------------------------------------------------------
#Figure4h_Categories:  Political versus Maintenance ->  competitive versus non competitive 
# --------------------------------------------------------------------------------------------------------------------------

df_Pol   <- df %>% filter(politically_motivated == 1) #1373 edits
df_Main  <- df %>% filter(politically_motivated == 0) #1075


Compet_Pol_A <- data.frame(table(df_Pol$Competitive_Dummy))
Compet_Pol_A <- Compet_Pol_A[-1,]
Compet_Pol_R <- Compet_Pol_A
#old: N_B <- length(unique(Vote_Diff_Mean$pageid.x[Vote_Diff_Mean$Competitive_Dummy =="Bottom25"]))
#old: N_T <- length(unique(Vote_Diff_Mean$pageid.x[Vote_Diff_Mean$Competitive_Dummy =="Top25"]))
N_B <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Bottom25"]))
N_T <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Top25"]))
Compet_Pol_R[1,2] <- Compet_Pol_A[1,2] /N_B
Compet_Pol_R[2,2] <- Compet_Pol_A[2,2] /N_T
Compet_Pol_R$Freq <- round(Compet_Pol_R$Freq, digits = 2)

Compet_Main_A <- data.frame(table(df_Main$Competitive_Dummy))
Compet_Main_A <- Compet_Main_A[-1,]
Compet_Main_R <- Compet_Main_A
#old: N_B <- length(unique(Vote_Diff_Mean$pageid.x[Vote_Diff_Mean$Competitive_Dummy =="Bottom25"]))
#old: N_T <- length(unique(Vote_Diff_Mean$pageid.x[Vote_Diff_Mean$Competitive_Dummy =="Top25"]))
N_B <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Bottom25"]))
N_T <- length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Competitive_Dummy =="Top25"]))
Compet_Main_R[1,2] <- Compet_Main_A[1,2] /N_B
Compet_Main_R[2,2] <- Compet_Main_A[2,2] /N_T
Compet_Main_R$Freq <- round(Compet_Main_R$Freq, digits = 2)


Figure4h_Plot1 <- ggplot(Compet_Pol_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Political Edits by Competitiveness-Groups") +
  xlab("Competitiveness-Group") + 
  theme_classic() +
  ylab("Number of Political Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "plum", "orchid1")) +  
  scale_x_discrete(labels=c("Bottom 25%", "Top 25%")) +
  theme(legend.position = "none")


Figure4h_Plot2 <- ggplot(Compet_Main_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Maintenance Edits by Competitiveness-Groups") +
  xlab("Competitiveness-Group") + 
  theme_classic() +
  ylab("Number of Maintenance Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c(  "plum", "orchid1")) +  
  scale_x_discrete(labels=c("Bottom 25%", "Top 25%")) +
  theme(legend.position = "none")


#Figure4h_Categories:
grid.arrange(Figure4h_Plot1, Figure4h_Plot2 , ncol = 2, nrow = 1,
             top=("Distribution of maintenance and political edits by competitiveness"))


#Political
0.51/0.27 #=  1.888889
#Maintenace 
0.43/0.14 #= 3.071
#Within top25%
0.27/0.14 #= 1.928571
#Within Bottom25%
0.51/0.43 #= 1.186

# --------------------------------------------------------------------------------------------------------------------------
#Figure4i_Categories:  Political versus Maintenance/ divided by total number of edits in senate or in house (respectively) 
# --------------------------------------------------------------------------------------------------------------------------

df_Pol   <- df %>% filter(politically_motivated == 1) #1373 edits
df_Main  <- df %>% filter(politically_motivated == 0) #1075 edits

chamberPolA <- data.frame(table(df_Pol$Chamber))
chamberPolR <- chamberPolA
# For different specification use: pageid_session instead of pageid to account for Senators serving longer 
N_H <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="H"]))
N_S <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="S"]))
chamberPolR[1,2] <- chamberPolA[1,2] /N_H
chamberPolR[2,2] <- chamberPolA[2,2] /N_S
chamberPolR$Freq <- round(chamberPolR$Freq, digits = 2)


chamberMainA <- data.frame(table(df_Main$Chamber))
chamberMainR <- chamberMainA
# For different specification use: pageid_session instead of pageid to account for Senators serving longer 
N_H <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="H"]))
N_S <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber =="S"]))
chamberMainR[1,2] <- chamberMainA[1,2] /N_H
chamberMainR[2,2] <- chamberMainA[2,2] /N_S
chamberMainR$Freq <- round(chamberMainR$Freq, digits = 2)



Figure4i_Plot1 <- ggplot(chamberPolR , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Political Edits by chamber per Legislator") +
  xlab("Chamber") + 
  theme_classic() +
  ylab("Number of Political Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "grey75","grey43")) +  
  scale_x_discrete(labels=c("House", "Senate")) +
  theme(legend.position = "none")


Figure4i_Plot2 <- ggplot(chamberMainR , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Maintenance Edits by chamber") +
  xlab("Chamber") + 
  theme_classic() +
  ylab("Number of Maintenance Edits per MoC per Legislator") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c(  "grey75","grey43")) +  
  scale_x_discrete(labels=c("House", "Senate")) +
  theme(legend.position = "none")


#Figure4i_Categories:
grid.arrange(Figure4i_Plot1, Figure4i_Plot2 , ncol = 2, nrow = 1,
             top=("Distribution of maintenance and political edits by chamber per Legislator"))

#House
1.38/1.07 #= 1.28972
#Senate 
1.27/1.02 #= 1.245098


# --------------------------------------------------------------------------------------------------------------------------
#Figure4j_Categories:  Political versus Maintenance ->  Female versus Male
# --------------------------------------------------------------------------------------------------------------------------

df_Pol   <- df %>% filter(politically_motivated == 1) #1373 edits
df_Main  <- df %>% filter(politically_motivated == 0) #1075


Sex_Pol_A <- data.frame(table(df_Pol$sex))
Sex_Pol_R <- Sex_Pol_A
N_F <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="female"]))
N_M <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="male"]))
Sex_Pol_R[1,2] <- Sex_Pol_A[1,2] /N_F
Sex_Pol_R[2,2] <- Sex_Pol_A[2,2] /N_M
Sex_Pol_R$Freq <- round(Sex_Pol_R$Freq, digits = 2)

Sex_Main_A <- data.frame(table(df_Main$sex))
Sex_Main_R <- Sex_Main_A
N_F <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="female"]))
N_M <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$sex =="male"]))
Sex_Main_R[1,2] <- Sex_Main_A[1,2] /N_F
Sex_Main_R[2,2] <- Sex_Main_A[2,2] /N_M
Sex_Main_R$Freq <- round(Sex_Main_R$Freq, digits = 2)


Figure4j_Plot1 <- ggplot(Sex_Pol_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Political Edits by Gender") +
  xlab("Gender") +
  theme_classic() +
  ylab("Number of Political Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "darkolivegreen1","yellowgreen")) +  
  scale_x_discrete(labels=c("Female", "Male")) +
  theme(legend.position = "none")


Figure4j_Plot2 <- ggplot(Sex_Main_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Maintenance Edits by Gender") +
  xlab("Gender") + 
  theme_classic() +
  ylab("Number of Maintenance Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c("darkolivegreen1","yellowgreen")) +  
  scale_x_discrete(labels=c("Female", "Male")) +
  theme(legend.position = "none")


#Figure4j_Categories:
grid.arrange(Figure4j_Plot1, Figure4j_Plot2 , ncol = 2, nrow = 1,
             top=("Distribution of maintenance and political edits by gender"))

# Male
1.46/1.12 #= 1.303571
#Female
1.11/0.96 #= 1.15625




# --------------------------------------------------------------------------------------------------------------------------
#Figure4k_Categories:  Political versus Maintenance ->  D verus R
# --------------------------------------------------------------------------------------------------------------------------

df_Pol   <- df %>% filter(politically_motivated == 1) #1373 edits
df_Main  <- df %>% filter(politically_motivated == 0) #1075


Party_Pol_A <- data.frame(table(df_Pol$party_dual))
Party_Pol_R <- Party_Pol_A
N_D <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="D"]))
N_R <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="R"]))
Party_Pol_R[1,2] <- Party_Pol_A[1,2] /N_D
Party_Pol_R[2,2] <- Party_Pol_A[2,2] /N_R
Party_Pol_R$Freq <- round(Party_Pol_R$Freq, digits = 2)

Party_Main_A <- data.frame(table(df_Main$party_dual))
Party_Main_R <- Party_Main_A
N_D <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="D"]))
N_R <- length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$party_dual =="R"]))
Party_Main_R[1,2] <- Party_Main_A[1,2] /N_D
Party_Main_R[2,2] <- Party_Main_A[2,2] /N_R
Party_Main_R$Freq <- round(Party_Main_R$Freq, digits = 2)


Figure4k_Plot1 <- ggplot(Party_Pol_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Political Edits by Party per MoC") +
  xlab("Party") + 
  theme_classic() +
  ylab("Number of Political Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c(  "steelblue1", "indianred1")) +  
  scale_x_discrete(labels=c("Democrats", "Republicans")) +
  theme(legend.position = "none")


Figure4k_Plot2 <- ggplot(Party_Main_R , aes(x=Var1, y=Freq, fill=Var1,)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white") + 
  #scale_fill_brewer(palette = "Greys",name = "Party") + 
  ggtitle("Maintenance Edits by Party per MoC") +
  xlab("Party") + 
  theme_classic() +
  ylab("Number of Maintenance Edits per MoC") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.20, size = 5)+
  scale_fill_manual(values = c( "steelblue1", "indianred1")) +  
  scale_x_discrete(labels=c("Democrats", "Republicans")) +
  theme(legend.position = "none")


#Figure4k_Categories:
grid.arrange(Figure4k_Plot1, Figure4k_Plot2 , ncol = 2, nrow = 1,
             top=("Distribution of maintenance and political edits by party"))


#Republicans
1.52/1.04 #= 1.461538
#Democrats
1.26/1.16 #= 1.086207


# --------------------------------------------------------------------------------------------------------------------------
#Figure4l_Categories: Ratio of political edits over all congress edits over all sessions 
# --------------------------------------------------------------------------------------------------------------------------

# Ratio of political edits on all edits by Session
n109 <- length(df$session[df$session==109 & df$politically_motivated == 1]) / length(df$session[df$session==109])
n110 <- length(df$session[df$session==110 & df$politically_motivated == 1]) / length(df$session[df$session==110])
n111 <- length(df$session[df$session==111 & df$politically_motivated == 1]) / length(df$session[df$session==111])

n112 <- length(df$session[df$session==112 & df$politically_motivated == 1]) / length(df$session[df$session==112])
n113 <- length(df$session[df$session==113 & df$politically_motivated == 1]) / length(df$session[df$session==113])
n114 <- length(df$session[df$session==114 & df$politically_motivated == 1]) / length(df$session[df$session==114])


data_Ratio_Sessions <- data.frame(Session = c("109th", "110th","111th", "112th","113th", "114th"),
                                 Share = c(n109, n110, n111, n112, n113, n114))

#Figure4j1_Categories
Figure4j1_Categories <- ggplot(data_Ratio_Sessions , aes(x= Session, y=Share)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white", fill = "orange3") + 
  #ggtitle("Share of Political edits compared to all edits") +
  ggtitle("Political Edits") +
  xlab("Session") + 
  theme_bw() + 
  ylab("Share of Political edits compared to all edits")




# Ratio of BENEFICIAL political edits on all edits by Session

nB109 <- length(df$session[df$session==109 & df$politically_motivated_positive == 1]) / length(df$session[df$session==109])
nB110 <- length(df$session[df$session==110 & df$politically_motivated_positive == 1]) / length(df$session[df$session==110])
nB111 <- length(df$session[df$session==111 & df$politically_motivated_positive == 1]) / length(df$session[df$session==111])

nB112 <- length(df$session[df$session==112 & df$politically_motivated_positive == 1]) / length(df$session[df$session==112])
nB113 <- length(df$session[df$session==113 & df$politically_motivated_positive == 1]) / length(df$session[df$session==113])
nB114 <- length(df$session[df$session==114 & df$politically_motivated_positive == 1]) / length(df$session[df$session==114])


data_Ratio_Sessions_B <- data.frame(Session = c("109th", "110th","111th", "112th","113th", "114th"),
                                  Share = c(nB109, nB110, nB111, nB112, nB113, nB114))

#Figure4j2_Categories
Figure4j2_Categories <- ggplot(data_Ratio_Sessions_B , aes(x= Session, y=Share)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white", fill = "orange2") + 
  #ggtitle("Share of Beneficial Political edits compared to all edits") +
  ggtitle("Beneficial Political Edits") +
  xlab("Session") + 
  theme_bw() + 
  ylab("Share of Beneficial Political edits compared to all edits")





# Ratio of HARMFUL political edits on all edits by Session

nH109 <- length(df$session[df$session==109 & df$politically_motivated_negative == 1]) / length(df$session[df$session==109])
nH110 <- length(df$session[df$session==110 & df$politically_motivated_negative == 1]) / length(df$session[df$session==110])
nH111 <- length(df$session[df$session==111 & df$politically_motivated_negative == 1]) / length(df$session[df$session==111])

nH112 <- length(df$session[df$session==112 & df$politically_motivated_negative == 1]) / length(df$session[df$session==112])
nH113 <- length(df$session[df$session==113 & df$politically_motivated_negative == 1]) / length(df$session[df$session==113])
nH114 <- length(df$session[df$session==114 & df$politically_motivated_negative == 1]) / length(df$session[df$session==114])


data_Ratio_Sessions_H <- data.frame(Session = c("109th", "110th","111th", "112th","113th", "114th"),
                                    Share = c(nH109, nH110, nH111, nH112, nH113, nH114))

#Figure4j3_Categories
Figure4j3_Categories <- ggplot(data_Ratio_Sessions_H , aes(x= Session, y=Share)) +
  geom_bar(stat="identity", position=position_dodge(), colour="white", fill = "orange1") + 
  #ggtitle("Share of Harmful Political edits compared to all edits") +
  ggtitle("Harmful Political Edits") +
  xlab("Session") + 
  theme_bw() + 
  ylab("Share of Harmful Political edits compared to all edits")


#Figure4j_Categories:
grid.arrange(Figure4j1_Categories , Figure4j2_Categories , Figure4j3_Categories  , ncol = 3, nrow = 1,
             top=("Share of certain groups of edits compared to all edits from the Congress IT networt by session"))



#------------------- Further General Statistics ----------------


#Ratio of gender within parties 
table(df_D$sex) /length(df_D$sex)
table(df_R$sex) /length(df_R$sex)

#Ratio of gender within chambers
table(df_H$sex) /length(df_H$sex)
table(df_S$sex) /length(df_S$sex)

#Ratio of gender within competitiveness-groups
table(df_T$sex) /length(df_T$sex)
table(df_B$sex) /length(df_B$sex)

#Ratio of party within competitiveness-groups
table(df_T$party_dual) /length(df_T$party_dual)
table(df_B$party_dual) /length(df_B$party_dual)

#Ratio of chamber within competitiveness-groups
table(df_T$Chamber) /length(df_T$Chamber)
table(df_B$Chamber) /length(df_B$Chamber)

#Average number of sessions served in competitiveness groups
BothChambers_Top25 <- BothChambers_Session_109_to_114_Short %>% filter(Competitive_Dummy =="Top25")
length(BothChambers_Top25$pageid_session) / length(unique(BothChambers_Top25$pageid))

BothChambers_Bottom25 <- BothChambers_Session_109_to_114_Short %>% filter(Competitive_Dummy =="Bottom25")
length(BothChambers_Bottom25$pageid_session)/ length(unique(BothChambers_Bottom25$pageid))

#Add Figure4_AA_2_Categories: sAll Edist per MoC 
#Chamber
2.45/2.29 
#Sex
2.58/2.07
#Compt
0.78/0.6
#Party 
2.56/2.42

#ADd Figure4_AB_Categories: Polititcal edits per MoC
#Chamber
1.38/1.27
#Sex
1.46/1.11
#Compt
0.43/0.39
#Party 
1.52/1.26

#Differences in number of categories an edit falls in between Top25 - Bottom25
sumT <- table(df_T$Answer.topic_career_dummy)[2] +table(df_T$Answer.topic_personal_dummy)[2] +table(df_T$Answer.topic_other_dummy)[2] +table(df_T$Answer.topic_views_dummy)[2]
sumB <- table(df_B$Answer.topic_career_dummy)[2] +table(df_B$Answer.topic_personal_dummy)[2] +table(df_B$Answer.topic_other_dummy)[2] +table(df_B$Answer.topic_views_dummy)[2]

sumT/length(df_T$revid)
sumB/length(df_B$revid)

#Differences in size of edits in between Top25 - Bottom25
df_T_stats <- df_T %>% dplyr::select(c("revid", "size"))
df_B_stats <- df_B %>% dplyr::select(c("revid", "size"))

df_T_stats <- na.omit(df_T_stats)   
df_B_stats <- na.omit(df_B_stats)   

mean(df_T_stats$size) #=  16,406.12
mean(df_B_stats$size) #=  14,633.41
16406.12/14633.41 #=  1.121141

#Difference in time served by Chamber 
length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Chamber == "S"]))/ length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber == "S"]))
length(unique(BothChambers_Session_109_to_114_Short$pageid_session[BothChambers_Session_109_to_114_Short$Chamber == "H"]))/ length(unique(BothChambers_Session_109_to_114_Short$pageid[BothChambers_Session_109_to_114_Short$Chamber == "H"]))
# Average number of session served per Senator: 3.444444
# Average number of session served per Senator: 3.250602



######## Ideenspeicher ###########


BothChambers_Session_109_to_114_Short$Competitive_Dummy <- 0

for(i in 1:length(BothChambers_Session_109_to_114_Short$Competitive_Dummy)) {
  if (BothChambers_Session_109_to_114_Short$vote_maxdiff_relative[i] >=  0.431165) {
    BothChambers_Session_109_to_114_Short$Competitive_Dummy[i] = "Bottom25"}
  else if (BothChambers_Session_109_to_114_Short$vote_maxdiff_relative[i] <=   0.158327) {
    BothChambers_Session_109_to_114_Short$Competitive_Dummy[i] = "Top25" 
  }
}


# Merge IDs
BothChambers_Session_109_to_114_Short <- BothChambers_Session_109_to_114_Short %>%  unite(pageid_session_chamber, pageid_session, Chamber, sep = "_", remove = FALSE)
df <- df %>%  unite(pageid_session_chamber, pageid_session, Chamber, sep = "_", remove = FALSE)

Pageid_Competitive <- BothChambers_Session_109_to_114_Short %>% dplyr::select(c( "pageid_session_chamber", "Competitive_Dummy"))
doubles <- Pageid_Competitive %>% group_by(pageid_session_chamber) %>% filter(n()>1) # no doubles




table(df$Competitive_Dummy.y)
length(unique(BothChambers_Session_109_to_114_Short$pageid_session_chamber[BothChambers_Session_109_to_114_Short$Competitive_Dummy.y == "Bottom25"]))
length(unique(BothChambers_Session_109_to_114_Short$pageid_session_chamber[BothChambers_Session_109_to_114_Short$Competitive_Dummy.y == "Top25"]))

table(BothChambers_Session_109_to_114_Short$Competitive_Dummy.y)

colnames(BothChambers_Session_109_to_114_Short)
779/997
345/578



