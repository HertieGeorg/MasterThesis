#10a_Analysis_Skewedness_Edits

library(gridExtra)

# 1.----------- Checking whether data is skewed: visualized probability distribution of numbers of edits seen by each MoC
# See how many MoC do not have any edits from within congress in observed time frame 

data <- BothChambers_Session_109_to_114_Short

colnames(BothChambers_Session_109_to_114_Short)


table(is.na(data$AllCongressEdits_Per_MoC_Session)) #checking Na's
table(data$AllCongressEdits_Per_MoC_Session == 0) #checking zeros


# Visualize Distribution: All Congress-Edits
data %>% 
  ggplot( aes(x = AllCongressEdits_Per_MoC_Session)) +
  geom_histogram(bins = 40) +
  xlab("Number of Edits per Profile") 

# Visualize Distribution without outliers: All Congress-Edits

Zeros_AE <- data %>%  
  filter(AllCongressEdits_Per_MoC_Session < 16) %>% 
  ggplot(aes(x = AllCongressEdits_Per_MoC_Session)) +
  geom_histogram(bins = 16, fill="black") +
  #scale_y_continuous(trans = 'log2')+
  xlab("Number of All Congress Edits")+
  ylab("Frequency") +
  theme_bw() + 
  scale_fill_manual("green") + 
  ggtitle("All Congress Edits") 

# Check for Zero-Inflation in the data
100*sum(data$AllCongressEdits_Per_MoC_Session == 0)/nrow(data) #72.06% zero for All Congress Edits



# Visualize Distribution: Beneficial Political Edits
data %>% 
  ggplot( aes(x = All_Positive_Politically_CongressEdits_Per_MoC_Session)) +
  geom_histogram(bins = 40) +
  xlab("Number of Edits per Profile") 

# Visualize Distribution without outliers: Beneficial Political Congress-Edits
Zeros_BPE <- data %>%  
  filter(All_Positive_Politically_CongressEdits_Per_MoC_Session < 12) %>% 
  ggplot(aes(x = All_Positive_Politically_CongressEdits_Per_MoC_Session)) +
  geom_histogram(bins = 12, fill="black") +
  #scale_y_continuous(trans = 'log2')+
  xlab("Number of Beneficial Political Edits")+
  ylab("Frequency") +
  theme_bw() + 
  scale_fill_manual("green") + 
  ggtitle("Beneficial Political Congress Edits") 

# Check for Zero-Inflation in the data
100*sum(data$All_Positive_Politically_CongressEdits_Per_MoC_Session== 0)/nrow(data) #83.75527% zero for Beneficial Political Congress Edits



#Figure_Exessive_Zeros 
grid.arrange(Zeros_AE ,Zeros_BPE , ncol = 2, nrow = 1)
            # top=("Frequency Distribution of Congress Edits over MoC-Session pairs without outliers"))

#72.06% zero for All Congress Edits
#83.75527% zero for Beneficial Political Congress Edits



# ------ Others ----------

# Now same for: Distributions of Political-Edits 

table(is.na(data$All_Politically_CongressEdits_Per_MoC_Session)) #checking Na's
data$All_Politically_CongressEdits_Per_MoC[is.na(data$All_Politically_CongressEdits_Per_MoC)] = 0 # replacing NAs with 0

# Visualize Distribution: Politically Edits
data %>% 
  ggplot( aes(x = All_Politically_CongressEdits_Per_MoC_Session)) +
  geom_histogram(bins = 15) +
  xlab("Number of Edits per Profile") 

# Visualize Distribution without outliers:  Politically Edits
data %>%  
  filter( All_Politically_CongressEdits_Per_MoC_Session< 17) %>% 
  ggplot(aes(x =  All_Politically_CongressEdits_Per_MoC_Session)) +
  geom_histogram(bins = 17) +
  xlab("Number of Edits per Profile") 

# Number of ZEROS
table(data$All_Politically_CongressEdits_Per_MoC == 0) 
#526 Zeros in Political Edits




# Now same for: Distributions of Non-Political-Edits 

# Column doesnt exist yet, needs to be computed quickly
data$All_Non_Politically_CongressEdits_Per_MoC = data$AllCongressEdits_Per_MoC - data$All_Politically_CongressEdits_Per_MoC

table(is.na(data$All_Non_Politically_CongressEdits_Per_MoC)) #checking whether it worked
table(data$All_Non_Politically_CongressEdits_Per_MoC)


# Visualize Distribution: NON Politically Edits
data %>% 
  ggplot( aes(x = All_Non_Politically_CongressEdits_Per_MoC)) +
  geom_histogram(bins = 25) +
  xlab("Number of Edits per Profile") 

# Visualize Distribution without outliers:  NON Politically Edits
data %>%  
  filter(All_Non_Politically_CongressEdits_Per_MoC < 13) %>% 
  ggplot(aes(x = All_Non_Politically_CongressEdits_Per_MoC)) +
  geom_histogram(bins = 13) +
  xlab("Number of Edits per Profile") 

# Number of ZEROS
table(data$All_Non_Politically_CongressEdits_Per_MoC == 0) 
#532 Zeros in  NON Politically Edits

# Results:
# Distributions of all edtis from within Congress, non-political and political edits each are highly skewed 
# Zeros are pretty exessive








