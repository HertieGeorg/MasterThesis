# 10_ZINB_Analysis for ALL CONGRESS EDITS 

install.packages("pscl")
install.packages("MASS")
install.packages("boot")
install.packages("lmtest")
install.packages("AER")
install.packages("sandwich")
install.packages("jtools")
install.packages("stargazer")

library(pscl)
library(MASS)
library(boot)
library(lattice)
library(AER)
library(lmtest) #e provides coeftest function 
library(sandwich) # provides vcovHC function for calculating robust standard errors
library(stargazer)
library(jtools)




# ZinB-Model: 

# DV: Counts for all Congress Edits by MoC and Session 
# (as I am taking MoC-per-Session as observation-category, the number of zeros is extremely high,
# even higher compared to taking just MoC over the entire timeframe)
# IV (Count Model): Age, Gender, Party , Session, Chamber, External Edits per MoC/Session, Popularity as Views on Page (over entire timeframe), Competitiveness
# Ad Popularity: variable is problematic as only data from 20XX to 20XX is included which is not the exact same time frame as the edits 
# IV (Zero Inflation Model): ViewCategory, Session, External Edits
# Theory behind what variables to include in Logit-Model predicting the occurence of zeros:
# What factors predict that an MoC is not doing edits at all? 
# 1. ViewCategory: as I expect more popular MoCs to not edit their profiles 
# 2. Session: as there are significant differences between session regarding editing activity 
# 3. External Edits: as I am assuming that a lot of congressional edits are reaction towards external edits 


#Sources: 
#   In general:
# https://stats.idre.ucla.edu/r/dae/zinb/
# https://fukamilab.github.io/BIO202/04-C-zero-data.html
# https://mountainscholar.org/bitstream/handle/10968/244/FANG_ucdenveramc_1639M_10037.pdf?sequence=1&isAllowed=y
  
#   Ad Poisson: 
# https://fukamilab.github.io/BIO202/04-C-zero-data.html
# (Interesting other Sounrce: 2. Zuur, A. F. and Ieno, E. N. 2016. Beginner’s Guide to Zero-Inflated Models with R.)
# (Another interesting other Source: Coxe, West, Aiken (2009), The Analysis of Count Data: A gentle introduction to poisson regression and its alternatives)


#   Ad Stargazer Output: 
# https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf


# Load data
df_Zinb <- BothChambers_Session_109_to_114_Short


# Last Steps in Data Preparation

# 1. Vote_maxdiff_relative
#   Set NAs in competitiveness to mean (which occurs for US terretories. e.g. Puerto Rico, DC, etc.)
summary(df_Zinb$vote_maxdiff_relative)
for(i in 1:length(df_Zinb $pageid)) {
  if (is.na(df_Zinb $vote_maxdiff_relative[i]) ==  TRUE) {
    df_Zinb $vote_maxdiff_relative[i] = 0.32365}
}
#  Make Categorical Variable out of Combined_Mean_vote_maxdiff_relative
#     Get quartiles for Vote_maxdiff_relative
Number_of_Breaks =  c(0,1/4,2/4, 3/4, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff <- as.double(df_Zinb$Category_Vote_MaxDiff)
table(df_Zinb$Category_Vote_MaxDiff) #check whether it worked


# 2. birthyear (reduce to 2 digits, as all are born in the same century) 
table(df_Zinb$birthyear)
df_Zinb$birthyear <- stringr::str_extract(df_Zinb$birthyear, "..$") 
df_Zinb$birthyear <- as.double(df_Zinb$birthyear)

# 3. Session: transform into categorical variable
typeof(df_Zinb$session)
df_Zinb$session <- as.character(df_Zinb$session)


# Show Correlation of numerical Independent Variables 
df_Zinb_IV <- df_Zinb
df_Zinb_IV$sex <- stringr::str_replace_all(df_Zinb_IV$sex, "female", "1")
df_Zinb_IV$sex <- stringr::str_replace_all(df_Zinb_IV$sex, "male", "0")
df_Zinb_IV$sex <- as.double(df_Zinb_IV$sex) 
df_Zinb_IV$party_dual <- stringr::str_replace_all(df_Zinb_IV$party_dual , "D", "1")
df_Zinb_IV$party_dual <- stringr::str_replace_all(df_Zinb_IV$party_dual , "R", "0")
df_Zinb_IV$party_dual <- as.double(df_Zinb_IV$party_dual) 
df_Zinb_IV$Chamber <- stringr::str_replace_all(df_Zinb_IV$Chamber , "H", "1")
df_Zinb_IV$Chamber <- stringr::str_replace_all(df_Zinb_IV$Chamber , "S", "0")
df_Zinb_IV$Chamber <- as.double(df_Zinb_IV$Chamber) 
df_Zinb_IV$session <- as.double(df_Zinb_IV$session) 
df_Zinb_IV <- df_Zinb_IV %>%  dplyr::select(c(birthyear, sex, Chamber, party_dual, session, ExternalEdits_per_MoC_Session ,Category_Vote_MaxDiff, ViewCategory))
corTable <- cor(df_Zinb_IV)
round(corTable , 2)
# birthyear is correlated with session = trivial 
# party is correlated with sex = more men in Republican party
# party is correlated with age = Democrats are older on average 
# session is correlated with External edis = later sessions see a reducion in overall outside -0.12 (and inside -0.09) edits 
# party is correlated with competitiveness = Republicans are winning in more competitive districts 
# External edits is correlated with popularity (views)
# Chamber is highly correlated with popularity (views)





############################    MODELS    ############################

#Let’s do a quick check for Zero-Inflation in the data
100*sum(df_Zinb$AllCongressEdits_Per_MoC_Session == 0)/nrow(df_Zinb) #72.06% sind Zeros

#Let’s start with the simplest model, a Poisson GLM: 

M1 <- glm(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
             ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
          family = 'poisson',
          data = df_Zinb)

#With Robust Standart Erros on legislator-level
M1_Robust <- coeftest(M1, vcov = vcovCL(M1, cluster = df_Zinb$pageid))

tidy(M1_Robust)

tidy(M1)
summ(M1, confint = T, digits = 3, vifs = T)   
# Regarding Pseudo R-Squared-Values: https://web.archive.org/web/20130701052120/http://www.ats.ucla.edu:80/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm

# Interpretation of Coefficients: 
# eg: ViewCategory =  0.07656, means that an increase of 1 in ViewCategory causes an increase in Edits of exp( 0.07656) = 1.079567


## Check for over/underdispersion in the model

# Calculate the dispersion parameter φ: can be estimated using Pearson’s Chi-squared statistic and the degree of freedom
# Estimation of the dispersion parameter (Source: https://towardsdatascience.com/adjust-for-overdispersion-in-poisson-regression-4b1f52baa2f1)
# When φ is larger than 1, it is overdispersion
dp = sum(residuals(M1,type ="pearson")^2)/M1$df.residual
dp # = 5.284253 which is much higher than 1 -> strong overdispersion
# Alternative Way to Compute Dispersion 

dispersiontest(M1)

# Visual Check of Overdispersion: plotting the estimated variance against the mean
plot(log(fitted(M1)),log((df_Zinb$AllCongressEdits_Per_MoC- fitted(M1))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2), pch=20,col="blue")
abline(0,1) ## 'variance = mean' line
# We can see that the majority of the variance is larger than the mean, which is a warning of overdispersion.

#Dealing with Dispersion: 1. Allow Dispersion Estimation
M1_quasi <- glm(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear + 
                  ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
          family=quasipoisson,
          data = df_Zinb)

summary(M1_quasi) # Dispersion parameter is the same as calculated manually 
summ(M1_quasi, confint = T, digits = 3, vifs = T)   


#Robust Standart Erros on legislator-level
M1_quasi_Robust <- coeftest(M1_quasi, vcov = vcovCL(M1_quasi, cluster = df_Zinb$pageid))



# MODEL TWO: Negative BinomialGLM
# A good way to address overdispersion in count data is to use a Negative Binomial Model  


M2_negBinom <- glm.nb(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                        ExternalEdits_per_MoC_Session + session + party_dual + Chamber, data = df_Zinb)

#Robust Standart Erros on legislator-level
M2_negBinom_Robust <- coeftest(M2_negBinom , vcov = vcovCL(M2_negBinom , cluster = df_Zinb$pageid))


summary(M2_negBinom)
# Interpretation of regression coefficients of Negative Binommial is identical to standart Poisson Model (exp() usw.)
# Negative BinomialGLM is better fit to data, because ratio of deviance over degrees of freedom is only slightly larger than 1 here (and way better than Poisson)

# Potential References: References: https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# Faraway, Julian J. Extending the linear model with R: generalized linear, mixed effects and nonparametric regression models. CRC press, 2016.
# https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# https://data.princeton.edu/wws509/r/overdispersion





# MODEL THREE: ZINB

M3_ZeroInfl <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff  + sex + birthyear + ExternalEdits_per_MoC_Session + session + party_dual + Chamber  | session + ExternalEdits_per_MoC_Session ,
                        data = df_Zinb , dist = "negbin")

M3_ZeroInfl2 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                          ExternalEdits_per_MoC_Session + session + party_dual + Chamber |  ExternalEdits_per_MoC_Session,
                         data = df_Zinb , dist = "negbin")

# Waldtest can be used to compare different variable selection 
waldtest(M3_ZeroInfl, M3_ZeroInfl2)

# Running ZINB with robust standard errors:
# Source for standart errors: https://data.library.virginia.edu/understanding-robust-standard-errors/
# Source for code for Standart Errors: https://stackoverflow.com/questions/35372090/clustered-standard-error-for-zero-inflated-negative-binomial-model

# With clustered standard errors by individual MoC
#M3_ZeroInfl_Robust <- coeftest(M3_ZeroInfl, vcov = vcovCL(M3_ZeroInfl, cluster = df_Zinb$pageid))


M1cov <- vcovCL(M1, cluster = df_Zinb$pageid)
M1_robust.se <- sqrt(diag(M1cov))

M1_quasi_cov <- vcovCL(M1_quasi, cluster = df_Zinb$pageid)
M1_quasi_robust.se <- sqrt(diag(M1_quasi_cov))

M2_negBinom_cov <- vcovCL(M2_negBinom, cluster = df_Zinb$pageid)
M2_negBinom_robust.se <- sqrt(diag(M2_negBinom_cov))

M3_ZeroInfl_cov <- vcovCL(M3_ZeroInfl, cluster = df_Zinb$pageid)
M3_ZeroInfl_robust.se <- sqrt(diag(M3_ZeroInfl_cov))
#M3_ZeroInfl_robust.se <- format(M3_ZeroInfl_robust.se, scientific = TRUE)
M3_ZeroInfl_robust.se  <- as.double(M3_ZeroInfl_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)

# Out: AlllEdits(1)_Model_Results.html
stargazer(M1, M1_quasi, M2_negBinom, M3_ZeroInfl, 
          se = list(M1_robust.se, M1_quasi_robust.se, M2_negBinom_robust.se, M3_ZeroInfl_robust.se), 
          column.labels=c("M1","Quasi", "M2_NegBinom", "M3_ZeroInfl"), 
          out="AllEdits(1)_Model_Results.html",  type = "text", 
          title="Comparing Different Model Results (All Congress Edits)", align=TRUE) 


# When using clustered standard errors, Poisson and Quasi-Poisson have the exact same values
#tidy(M1)
#tidy(M1_quasi)
#tidy(M1_Robust)
#tidy(M1_quasi_Robust)






############ Robustness Checks of the ZINB Model using alternative Specifications: #############


# 1. Without influential observations: cut out top 1% of overall Congress edit counts 

quantile(df_Zinb$AllCongressEdits_Per_MoC_Session, prob = seq(0, 1, length = 200), type = 5) #Top 1% = bigger than 10
df_Zinb_AltSpecA <- df_Zinb %>% filter(AllCongressEdits_Per_MoC_Session <= 10)

M3_ZeroInfl_AltSpecA <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                           ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                        data = df_Zinb_AltSpecA , dist = "negbin")
#M3_ZeroInfl_AltSpecA_Robust <- coeftest(M3_ZeroInfl_AltSpecA, vcov = vcovCL(M3_ZeroInfl_AltSpecA, cluster = df_Zinb_AltSpecA$pageid))
M3_ZeroInfl_AltSpecA_cov <- vcovCL(M3_ZeroInfl_AltSpecA, cluster = df_Zinb_AltSpecA$pageid)
M3_ZeroInfl_AltSpecA_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpecA_cov))
M3_ZeroInfl_AltSpecA_robust.se <- as.double(M3_ZeroInfl_AltSpecA_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)





# 2. ZinB model with additional variable "Views"

M3_ZeroInfl_View <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear + ExternalEdits_per_MoC_Session + session + party_dual + Chamber + ViewCategory  | session + ViewCategory + ExternalEdits_per_MoC_Session,
                               data = df_Zinb , dist = "negbin")

#M3_ZeroInfl_View_Robust <- coeftest(M3_ZeroInfl_View , vcov = vcovCL(M3_ZeroInfl_View , cluster = df_Zinb$pageid))
M3_ZeroInfl_View_cov <- vcovCL(M3_ZeroInfl_View, cluster = df_Zinb$pageid)
M3_ZeroInfl_View_cov_robust.se <- sqrt(diag(M3_ZeroInfl_View_cov))
M3_ZeroInfl_View_cov_robust.se <- as.double(M3_ZeroInfl_View_cov_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)






# 3. Excluding MoCs that served in both Chambers 

df_Zinb_Unique <- df_Zinb %>% dplyr::select(c(Chamber, pageid))
df_Zinb_Unique <- unique(df_Zinb_Unique)
doubles <- df_Zinb_Unique %>% group_by(pageid) %>% filter(n()>1) # -> no doubles, each edit appears just once in the dataset
Double_Mocs <- unique(doubles$pageid)
`%notin%` <- Negate(`%in%`)
df_Zinb_Unique <- df_Zinb %>% filter(pageid %notin% Double_Mocs)

M3_ZeroInfl_AltSpec_NoDoubles <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb_Unique , dist = "negbin")

#M3_ZeroInfl_AltSpec_NoDoubles_Robust <- coeftest(M3_ZeroInfl_AltSpec_NoDoubles, vcov = vcovCL(M3_ZeroInfl_AltSpec_NoDoubles, cluster = df_Zinb_Unique$pageid))
M3_ZeroInfl_AltSpec_NoDoubles_cov <- vcovCL(M3_ZeroInfl_AltSpec_NoDoubles , cluster = df_Zinb_Unique$pageid)
M3_ZeroInfl_AltSpec_NoDoubles_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec_NoDoubles_cov))
M3_ZeroInfl_AltSpec_NoDoubles_robust.se <- as.double(M3_ZeroInfl_AltSpec_NoDoubles_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)




# Out: AllEdits(2)_ZinB_Specifications.html

stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpec_NoDoubles, M3_ZeroInfl_View,
          se = list(M3_ZeroInfl_robust.se, M3_ZeroInfl_AltSpecA_robust.se, M3_ZeroInfl_AltSpec_NoDoubles_robust.se, M3_ZeroInfl_View_cov_robust.se), 
          column.labels=c( "Standard", "99%EditsCounts", "UniqueMoCs", "PlusPopularity"), 
          out="AllEdits(2)_ZinB_Specifications.html",  type = "text", 
          title="Zero-inflated models: different specifications(1)", align=TRUE) 





# 4. Other categories for vote_maxdiff_relative 

# 1/2Categories 
Number_of_Breaks =  c(0,1/2, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_2 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_2 <- as.double(df_Zinb$Category_Vote_MaxDiff_2)
table(df_Zinb$Category_Vote_MaxDiff_2) #check whether it worked


# 1/3Categories 
Number_of_Breaks =  c(0,1/3,2/3, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_3 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_3 <- as.double(df_Zinb$Category_Vote_MaxDiff_3)
table(df_Zinb$Category_Vote_MaxDiff_3) #check whether it worked

# 1/5 Categories 
Number_of_Breaks =  c(0,1/5,2/5, 3/5, 4/5, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_5 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_5 <- as.double(df_Zinb$Category_Vote_MaxDiff_5)
table(df_Zinb$Category_Vote_MaxDiff_5) #check whether it worked


# 1/6 Categories 
Number_of_Breaks =  c(0,1/6,2/6, 3/6, 4/6, 5/6, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_6 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_6 <- as.double(df_Zinb$Category_Vote_MaxDiff_6)
table(df_Zinb$Category_Vote_MaxDiff_6) #check whether it worked


# Ad 1/3
M3_ZeroInfl_AltSpec1 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_3 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec1_Robust <- coeftest(M3_ZeroInfl_AltSpec1, vcov = vcovCL(M3_ZeroInfl_AltSpec1, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec1_cov <- vcovCL(M3_ZeroInfl_AltSpec1 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec1_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec1_cov))
M3_ZeroInfl_AltSpec1_robust.se <- as.double(M3_ZeroInfl_AltSpec1_robust.se) 


# Ad 1/5
M3_ZeroInfl_AltSpec2 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_5 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec2_Robust <- coeftest(M3_ZeroInfl_AltSpec2, vcov = vcovCL(M3_ZeroInfl_AltSpec2, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec2_cov <- vcovCL(M3_ZeroInfl_AltSpec2 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec2_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec2_cov))
M3_ZeroInfl_AltSpec2_robust.se <- as.double(M3_ZeroInfl_AltSpec2_robust.se) 


# Ad 1/6
M3_ZeroInfl_AltSpec3 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_6 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec3_Robust <- coeftest(M3_ZeroInfl_AltSpec3, vcov = vcovCL(M3_ZeroInfl_AltSpec3, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec3_cov <- vcovCL(M3_ZeroInfl_AltSpec3 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec3_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec3_cov))
M3_ZeroInfl_AltSpec3_robust.se <- as.double(M3_ZeroInfl_AltSpec3_robust.se) 


# vote_maxdiff_relative
M3_ZeroInfl_AltSpec4 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ vote_maxdiff_relative + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec4_Robust <- coeftest(M3_ZeroInfl_AltSpec4, vcov = vcovCL(M3_ZeroInfl_AltSpec4, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec4_cov <- vcovCL(M3_ZeroInfl_AltSpec4 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec4_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec4_cov))
M3_ZeroInfl_AltSpec4_robust.se <- as.double(M3_ZeroInfl_AltSpec4_robust.se) 


# Ad 1/2
M3_ZeroInfl_AltSpec5 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_2 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec5_Robust <- coeftest(M3_ZeroInfl_AltSpec5, vcov = vcovCL(M3_ZeroInfl_AltSpec5, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec5_cov <- vcovCL(M3_ZeroInfl_AltSpec5 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec5_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec5_cov))
M3_ZeroInfl_AltSpec5_robust.se <- as.double(M3_ZeroInfl_AltSpec5_robust.se) 


# Out: AllEdits(3_Appendix)_ZinB_VoteDiffCategories.html 

stargazer(M3_ZeroInfl_AltSpec5, M3_ZeroInfl_AltSpec1, M3_ZeroInfl, M3_ZeroInfl_AltSpec2, M3_ZeroInfl_AltSpec3, M3_ZeroInfl_AltSpec4,
          se = list(M3_ZeroInfl_AltSpec5_robust.se, M3_ZeroInfl_AltSpec1_robust.se, M3_ZeroInfl_robust.se, M3_ZeroInfl_AltSpec2_robust.se, M3_ZeroInfl_AltSpec3_robust.se, M3_ZeroInfl_AltSpec5_robust.se), 
          column.labels=c("1/2", "1/3", "Standart(1/4)","1/5","1/6", "noCategory"), 
          out="AllEdits(3_Appendix)_ZinB_VoteDiffCategories.html",  type = "text",  
          title="Zero-inflated models: different vote_diff-Categories", align=TRUE) 







# 5. Using different variables for the count-logit-process

    # All but Category_Vote_MaxDiff
M3_ZeroInfl_AltCount <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                          ExternalEdits_per_MoC_Session + session + party_dual + Chamber |  sex + birthyear +
                            ExternalEdits_per_MoC_Session + session + party_dual + Chamber ,
                        data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltCount_Robust <- coeftest(M3_ZeroInfl_AltCount, vcov = vcovCL(M3_ZeroInfl_AltCount, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltCount_cov <- vcovCL(M3_ZeroInfl_AltCount, cluster = df_Zinb$pageid)
M3_ZeroInfl_AltCount_robust.se <- sqrt(diag(M3_ZeroInfl_AltCount_cov))
M3_ZeroInfl_AltCount_robust.se  <- as.double(M3_ZeroInfl_AltCount_robust.se) 


    # Regular +  Category_Vote_MaxDiff
M3_ZeroInfl_AltCount2 <- zeroinfl(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session + Category_Vote_MaxDiff,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltCount2_Robust <- coeftest(M3_ZeroInfl_AltCount2, vcov = vcovCL(M3_ZeroInfl_AltCount2, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltCount2_cov <- vcovCL(M3_ZeroInfl_AltCount2, cluster = df_Zinb$pageid)
M3_ZeroInfl_AltCount2_robust.se <- sqrt(diag(M3_ZeroInfl_AltCount2_cov))
M3_ZeroInfl_AltCount2_robust.se  <- as.double(M3_ZeroInfl_AltCount2_robust.se) 



# Out: AllEdits(4_Appendix)_ZinB_DifferentCounts.html
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltCount, M3_ZeroInfl_AltCount2, 
          se = list(M3_ZeroInfl_robust.se, M3_ZeroInfl_AltCount_robust.se, M3_ZeroInfl_AltCount2_robust.se), 
          column.labels=c("Standart", "NoMaxDiff", "RegPlusMaxDiff"), 
          out="AllEdits(4_Appendix)_ZinB_DifferentCounts.html",  type = "text", 
          title="Zero-inflated models: different variables in logit part", align=TRUE) 



# Out: AllEdits(5_Appendix)_ZinB_DifferentCounts.html
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltCount, M3_ZeroInfl_AltCount2, zero.component = TRUE,
          se = list(M3_ZeroInfl_robust.se, M3_ZeroInfl_AltCount_robust.se, M3_ZeroInfl_AltCount2_robust.se), 
          column.labels=c("Standart", "NoMaxDiff", "RegPlusMaxDiff"), 
          out="AllEdits(4_Appendix)_ZinB_DifferentCounts.html",  type = "text", 
          title="Zero-inflated models: different variables in logit part", align=TRUE) 



############ Ideenspeicher ############## 




# MODELL FOUR: Hurdle regression 
# Source: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf

M4_Hurdle  <- hurdle(AllCongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff + sex + birthyear +
                       ViewCategory + ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session + ViewCategory,
                     data = df_Zinb,  dist = "negbin")
summary(M4_Hurdle)

# With clustered standart errors by individual MoC
M4_Hurdle_Robust <- coeftest(M4_Hurdle , vcov = vcovCL(M4_Hurdle, cluster = df_Zinb$pageid))








# get output in latex-code or html
install.packages("texreg")
library(texreg)
hc <- vcovCL(M3_ZeroInfl, cluster = df_Zinb$pageid)
ct <- coeftest(M3_ZeroInfl_Robust , vcov = hc)
se <- ct[, 2]
pval <- ct[, 4]
htmlreg(list(M3_ZeroInfl, M3_ZeroInfl, M3_ZeroInfl) , override.se = se, override.pvalues = pval)
# alternative: texreg()
htmlreg(M3_ZeroInfl_Robust , file = "mytable.html", override.se = se, override.pvalues = pval)

tidy(M3_ZeroInfl_Robust)





