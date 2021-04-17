# 10_ZINB_Analysis


install.packages("pscl")
install.packages("MASS")
install.packages("boot")
library(pscl)
library(MASS)
library(boot)


#Source: https://stats.idre.ucla.edu/r/dae/zinb/
# https://fukamilab.github.io/BIO202/04-C-zero-data.html
# https://mountainscholar.org/bitstream/handle/10968/244/FANG_ucdenveramc_1639M_10037.pdf?sequence=1&isAllowed=y
  


# Variables included into ZINB-Model: (siehe DAG)

#      DV: 
# Number Congress Edits/Days Served (AllCongressEdits_Per_MoC_Per_DayServed)
#      IV's: 
# Distritcts's Competitiveness
# Age of MoC
# Overall Edits/Days Served
# Popularity
# Internet Access
# (Party)
# (Gender)

df_Zinb <- Characteristics_Data_MoCs


# Last Steps in Data Preparation
df_Zinb$AllCongressEdits_Per_MoC[is.na(df_Zinb$AllCongressEdits_Per_MoC)] <- 0 # Setting all NAs to 0 
table(is.na(df_Zinb$AllCongressEdits_Per_MoC)) # False for 981 -> looking good
table(is.na(df_Zinb$Combined_Mean_vote_maxdiff_relative)) # 9 Nas -> also MoCs from Oversea US Terretories 
names(df_Zinb)[names(df_Zinb) == "Individual lives in household with Internet use (Percent)"] <- "InternetAccess"  # Change name of column that is way too long
df_Zinb$InternetAccess <- as.double(df_Zinb$InternetAccess) # turn character into double 

# Choose variables I need for analysis (include party as well?)
df_Zinb <- df_Zinb %>%  dplyr::select(c(pageid, AllCongressEdits_Per_MoC, sex, Day_Served_Sum, YearBirth, InternetAccess, ViewCategory, Overall_Edits_During_Tenure_Sum, Combined_Mean_vote_maxdiff_relative ))


# Show Correlation of all Independent Variables 
df_Zinb_IV <- df_Zinb %>%  dplyr::select(-c(AllCongressEdits_Per_MoC, sex, pageid))
df_Zinb_IV <- drop_na(df_Zinb_IV) # drop MoCs from US Terretories (as they have NA for InternetAccess and for Vote-Results-Difference) 
corTable <- cor(df_Zinb_IV)
round(corTable , 2)



################### MODELS #########################################

#Source: https://fukamilab.github.io/BIO202/04-C-zero-data.html
library(lattice)
library(MASS)
require(pscl) 
install.packages("lmtest")
library(lmtest)

# Interesting other Sounrce: 2. Zuur, A. F. and Ieno, E. N. 2016. Beginner’s Guide to Zero-Inflated Models with R.
# Another interesting other Source: Coxe, West, Aiken (2009), The Analysis of Count Data: A gentle introduction to poisson regression and its alternatives

#Let’s do a quick check for Zero-Inflation in the data
100*sum(df_Zinb$AllCongressEdits_Per_MoC == 0)/nrow(df_Zinb) #38,02% sind Zeros


#Let’s start with the simplest model, a Poisson GLM: 

## MODEL ONE: Poisson GLM


df_Zinb <- drop_na(df_Zinb) # dropping NAs first 

M1 <- glm(AllCongressEdits_Per_MoC ~ Combined_Mean_vote_maxdiff_relative + sex + YearBirth + InternetAccess + ViewCategory + Overall_Edits_During_Tenure_Sum + Day_Served_Sum, 
          family = 'poisson',
          data = df_Zinb)

summary(M1)
#or
# install.packages("jtools") #library(jtools)
summ(M1, confint = T, digits = 3, vifs = T)     # Regarding Pseudo R-Squared-Values: https://web.archive.org/web/20130701052120/http://www.ats.ucla.edu:80/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm


# Interpretation of Coefficients: 
# eg: ViewCategory =  0.07656, means that an increase of 1 in ViewCategory causes an increase in Edits of exp( 0.07656) = 1.079567

## Check for over/underdispersion in the model

# Calculate the dispersion parameter φ: can be estimated using Pearson’s Chi-squared statistic and the degree of freedom
# Estimation of the dispersion parameter (Source: https://towardsdatascience.com/adjust-for-overdispersion-in-poisson-regression-4b1f52baa2f1)
# When φ is larger than 1, it is overdispersion
dp = sum(residuals(M1,type ="pearson")^2)/M1$df.residual
dp # = 6.191824 which is much higher than 1 -> strong overdispersion
# Alternative Way to Compute Dispersion 
install.packages("AER")
library(AER)
dispersiontest(M1)

# Visual Check of Overdispersion: plotting the estimated variance against the mean
plot(log(fitted(M1)),log((df_Zinb$AllCongressEdits_Per_MoC- fitted(M1))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2), pch=20,col="blue")
abline(0,1) ## 'variance = mean' line
# We can see that the majority of the variance is larger than the mean, which is a warning of overdispersion.

#Dealing with Dispersion: 1. Allow Dispersion Estimation
M1_quasi <- glm(AllCongressEdits_Per_MoC ~ Combined_Mean_vote_maxdiff_relative + sex + YearBirth + InternetAccess + ViewCategory + Overall_Edits_During_Tenure_Sum + Day_Served_Sum, 
          family=quasipoisson,
          data = df_Zinb)

summary(M1_quasi) # Dispersion parameter is the same as calculated manually 


# MODEL TWO: Negative BinomialGLM
library(MASS)
M2_negBinom <- glm.nb(AllCongressEdits_Per_MoC ~ Combined_Mean_vote_maxdiff_relative + sex + YearBirth + InternetAccess + ViewCategory + Overall_Edits_During_Tenure_Sum + Day_Served_Sum, 
          data = df_Zinb)

summary(M2_negBinom )
# Interpretation of regression coefficients of Negative Binommial is identical to standart Poisson Model (exp() usw.)
# Negative BinomialGLM is better fit to data, because ratio of deviance over degrees of freedom is only slightly larger than 1 here (and way better than Poisson)

# Potential References: References: https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# Faraway, Julian J. Extending the linear model with R: generalized linear, mixed effects and nonparametric regression models. CRC press, 2016.
# https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# https://data.princeton.edu/wws509/r/overdispersion

# MODEL THREE: 

# Zero-Inflation: Does data really have more Zeros than Poisson would predict? 


M2_negBinom <- zeroinfl(AllCongressEdits_Per_MoC ~ Combined_Mean_vote_maxdiff_relative + sex + YearBirth + InternetAccess + ViewCategory + Overall_Edits_During_Tenure_Sum + Day_Served_Sum, 
                      data = df_Zinb)


# old ZINB
df_Zinb$AllCongressEdits_Per_MoC_Per_DayServed <- as.integer(df_Zinb$AllCongressEdits_Per_MoC_Per_DayServed) 

df_Zinb  <- drop_na(df_Zinb)
model <- zeroinfl(AllCongressEdits_Per_MoC_Per_DayServed ~ sex + YearBirth + Combined_Mean_vote_maxdiff_relative | OverallEdits_inTenure_perDayServed, data = df_Zinb, dist = "negbin", EM = T)
summary(model)
