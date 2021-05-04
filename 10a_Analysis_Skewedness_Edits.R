#10a_Analysis_Skewedness_Edits



# 1.----------- Checking whether data is skewed: visualized probability distribution of numbers of edits seen by each MoC
# See how many MoC do not have any edits from within congress in observed time frame 

data <- Characteristics_Data_MoCs

table(is.na(data$AllCongressEdits_Per_MoC)) #checking Na's
data$AllCongressEdits_Per_MoC[is.na(data$AllCongressEdits_Per_MoC)] = 0 # replacing NAs with 0


# Visualize Distribution: All Congress-Edits
data %>% 
  ggplot( aes(x = AllCongressEdits_Per_MoC)) +
  geom_histogram(bins = 40) +
  xlab("Number of Edits per Profile") 

# Visualize Distribution without outliers: All Congress-Edits
data %>%  
  filter(AllCongressEdits_Per_MoC < 20) %>% 
  ggplot(aes(x = AllCongressEdits_Per_MoC)) +
  geom_histogram(bins = 20) +
  xlab("Number of Edits per Profile") 

# Number of ZEROS
table(data$AllCongressEdits_Per_MoC == 0) 
#373 Zeros in All Congress-Edits




# Now same for: Distributions of Political-Edits 

table(is.na(data$All_Politically_CongressEdits_Per_MoC )) #checking Na's
data$All_Politically_CongressEdits_Per_MoC[is.na(data$All_Politically_CongressEdits_Per_MoC)] = 0 # replacing NAs with 0

# Visualize Distribution: Politically Edits
data %>% 
  ggplot( aes(x = All_Politically_CongressEdits_Per_MoC)) +
  geom_histogram(bins = 15) +
  xlab("Number of Edits per Profile") 

# Visualize Distribution without outliers:  Politically Edits
data %>%  
  filter(All_Politically_CongressEdits_Per_MoC < 17) %>% 
  ggplot(aes(x = All_Politically_CongressEdits_Per_MoC)) +
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








