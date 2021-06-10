# 11a_ZINB_Analysis for ALL CONGRESS EDITS 



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
# 3. External Edits: as I am assuming that a lot of Beneficial Political Edits are reaction towards external edits 


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

df_Zinb$vote_maxdiff_relative <- df_Zinb$vote_maxdiff_relative*100


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
df_Zinb_IV <- df_Zinb_IV %>%  dplyr::select(c(birthyear, sex, Chamber, party_dual, session, ExternalEdits_per_MoC_Session , vote_maxdiff_relative, ViewCategory))
corTable <- cor(df_Zinb_IV)
round(corTable , 2)
# birthyear is correlated with session = trivial 
# party is correlated with sex = more men in Republican party
# party is correlated with age = Democrats are older on average 
# session is correlated with External edis = later sessions see a reducion in overall outside -0.12 (and inside -0.09) edits 
# party is correlated with competitiveness = Republicans are winning in more competitive districts 
# External edits is correlated with popularity (views)
# Chamber is highly correlated with popularity (views)


# Function for converting standard errors into odds ratios
# Taken from: https://www.andrewheiss.com/blog/2016/04/25/convert-logistic-regression-standard-errors-to-odds-ratios-with-r/
# Author: Andrew Heiss

get.or.se <- function(model) {   
  broom::tidy(model) %>%
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    dplyr::select(or.se) %>% unlist %>% unname
} #get.or.se(model)






############################    MODELS    ############################

# Check for Zero-Inflation in the data
100*sum(df_Zinb$All_Positive_Politically_CongressEdits_Per_MoC_Session == 0)/nrow(df_Zinb) #83.75527% sind Zeros

# 1) Poisson GLM: 
M1 <- glm(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative  + sex + birthyear +
             ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
          family = 'poisson',
          data = df_Zinb)

#With Robust Standart Erros on legislator-level
M1_Robust <- coeftest(M1, vcov = vcovCL(M1, cluster = df_Zinb$pageid))
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






# Full Sample Beneifical Political Edits: ZINB

M3_ZeroInfl <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative  + sex + birthyear + 
                          ExternalEdits_per_MoC_Session + session + party_dual + Chamber   | vote_maxdiff_relative  + sex + birthyear + 
                          ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
                        data = df_Zinb , dist = "negbin")
M3_ZeroInfl_Robust <- coeftest(M3_ZeroInfl, vcov = vcovCL(M3_ZeroInfl, cluster = df_Zinb$pageid))
summary(M3_ZeroInfl)


# Dispersion Statistic 
E2 <- resid(M3_ZeroInfl, type = "pearson")
N  <- nrow(df_Zinb)
p  <- length(coef(M3_ZeroInfl)) + 1 # '+1' is due to theta
sum(E2^2) / (N - p) #= 1.225373 -> best value so far 


M3_ZeroInfl_cov <- vcovCL(M3_ZeroInfl, cluster = df_Zinb$pageid)
M3_ZeroInfl_robust.se <- sqrt(diag(M3_ZeroInfl_cov))
M3_ZeroInfl_robust.se  <- as.double(M3_ZeroInfl_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)
M3_ZeroInfl_robust.se_Count <- M3_ZeroInfl_robust.se[1:12]
M3_ZeroInfl_robust.se_Zero <- M3_ZeroInfl_robust.se[13:24]








############ Robustness Checks of the ZINB Model using alternative Specifications: #############


# 1. Without influential observations: cut out top 1% of overall Congress edit counts 

quantile(df_Zinb$All_Positive_Politically_CongressEdits_Per_MoC_Session, prob = seq(0, 1, length = 200), type = 5) #Top 1% = bigger than 10
df_Zinb_AltSpecA <- df_Zinb %>% filter(All_Positive_Politically_CongressEdits_Per_MoC_Session <= 10)

M3_ZeroInfl_AltSpecA <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative  + sex + birthyear +
                           ExternalEdits_per_MoC_Session + session + party_dual + Chamber | vote_maxdiff_relative  + sex + birthyear +
                             ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
                        data = df_Zinb_AltSpecA , dist = "negbin")
#M3_ZeroInfl_AltSpecA_Robust <- coeftest(M3_ZeroInfl_AltSpecA, vcov = vcovCL(M3_ZeroInfl_AltSpecA, cluster = df_Zinb_AltSpecA$pageid))
M3_ZeroInfl_AltSpecA_cov <- vcovCL(M3_ZeroInfl_AltSpecA, cluster = df_Zinb_AltSpecA$pageid)
M3_ZeroInfl_AltSpecA_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpecA_cov))
M3_ZeroInfl_AltSpecA_robust.se <- as.double(M3_ZeroInfl_AltSpecA_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)
M3_ZeroInfl_AltSpecA_robust.se_Count <- M3_ZeroInfl_AltSpecA_robust.se[1:12]
M3_ZeroInfl_AltSpecA_robust.se_Zero <- M3_ZeroInfl_AltSpecA_robust.se[13:24]

# 2. Without influential observations: cut out top 1% of External Edits Counts

quantile(df_Zinb$ExternalEdits_per_MoC_Session, prob = seq(0, 1, length = 200), type = 5) #Top 1% = bigger than 1814.97739
df_Zinb_AltSpecE <- df_Zinb %>% filter(ExternalEdits_per_MoC_Session <= 1815)

M3_ZeroInfl_AltSpecE <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative  + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | vote_maxdiff_relative  + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
                                 data = df_Zinb_AltSpecE , dist = "negbin")

#M3_ZeroInfl_AltSpecA_Robust <- coeftest(M3_ZeroInfl_AltSpecA, vcov = vcovCL(M3_ZeroInfl_AltSpecA, cluster = df_Zinb_AltSpecA$pageid))
M3_ZeroInfl_AltSpecE_cov <- vcovCL(M3_ZeroInfl_AltSpecE, cluster = df_Zinb_AltSpecE$pageid)
M3_ZeroInfl_AltSpecE_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpecE_cov))
M3_ZeroInfl_AltSpecE_robust.se <- as.double(M3_ZeroInfl_AltSpecE_robust.se) 
M3_ZeroInfl_AltSpecE_robust.se_Count <- M3_ZeroInfl_AltSpecE_robust.se[1:12]
M3_ZeroInfl_AltSpecE_robust.se_Zero <- M3_ZeroInfl_AltSpecE_robust.se[13:24]


# Table 1&2 with Exponentiated Coefficents: 
source("12_(Table3&4)_Exponentiated_Coefficients_BP_Edits.R")


  

# Out: AlllEdits(2)_Model_Results_Count.html - Count part: OUTPUT NOT INCLUDED INTO THESIS 
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpecE,  
          se = list(M3_ZeroInfl_robust.se_Count, M3_ZeroInfl_AltSpecA_robust.se_Count, M3_ZeroInfl_AltSpecE_robust.se_Count), 
          dep.var.caption  = c("Count Part (NegBin): Number of Edits"),
          dep.var.labels=c(""), 
          column.labels = c("Full Sample", "99 Percentile of<br>Congress Edits", "99 Percentile of<br>External Edits"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table 2. Modelling the Number of Beneficial Political Edits using ZINB: Different Specifications (non exponentiated results)", model.names = F, 
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are not exponeniated.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          #out="BP_Edits(Table_2_NonExpo)_ZinB_Specifications_Count.html", 
          type = "text")




# Out: AlllEdits(2)_Model_Results_Zero.html - Zero part: OUTPUT NOT INCLUDED INTO THESIS 
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpecE,  zero.component = TRUE,
          se = list(M3_ZeroInfl_robust.se_Zero, M3_ZeroInfl_AltSpecA_robust.se_Zero, M3_ZeroInfl_AltSpecE_robust.se_Zero), 
          dep.var.caption  = c("Inflation Part (logit): Pr of Zero Edits"), 
          dep.var.labels=c(""), 
          column.labels = c("Full Sample", "99 Percentile of<br>Congress Edits", "99 Percentile of<br>External Edits"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table 1. Modeling the likelihood of Beneficial Political Edits using ZINB: Different Specifications (non exponentiated results)", model.names = F, 
          #style = "qje",  #model.numbers  = FALSE
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are not exponeniated.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          #out="BP_Edits(Table_1_NonExpo)_ZinB_Specifications_Zero.html", 
          type = "text")







# 5. Using different variables for the count-logit-process

# All Varialbes
M3_ZeroInfl_AltCount <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ vote_maxdiff_relative + sex + birthyear +
                          ExternalEdits_per_MoC_Session + session + party_dual + Chamber |  sex + birthyear +
                            ExternalEdits_per_MoC_Session + session + party_dual + Chamber + vote_maxdiff_relative ,
                        data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltCount_Robust <- coeftest(M3_ZeroInfl_AltCount, vcov = vcovCL(M3_ZeroInfl_AltCount, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltCount_cov <- vcovCL(M3_ZeroInfl_AltCount, cluster = df_Zinb$pageid)
M3_ZeroInfl_AltCount_robust.se <- sqrt(diag(M3_ZeroInfl_AltCount_cov))
M3_ZeroInfl_AltCount_robust.se  <- as.double(M3_ZeroInfl_AltCount_robust.se) 
M3_ZeroInfl_AltCount_robust.se_Count <- M3_ZeroInfl_AltCount_robust.se[1:12]
M3_ZeroInfl_AltCount_robust.se_Zero <- M3_ZeroInfl_AltCount_robust.se[13:24]



# Just non-characteristics
M3_ZeroInfl_AltCount2 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ vote_maxdiff_relative + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session + vote_maxdiff_relative,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltCount2_Robust <- coeftest(M3_ZeroInfl_AltCount2, vcov = vcovCL(M3_ZeroInfl_AltCount2, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltCount2_cov <- vcovCL(M3_ZeroInfl_AltCount2, cluster = df_Zinb$pageid)
M3_ZeroInfl_AltCount2_robust.se <- sqrt(diag(M3_ZeroInfl_AltCount2_cov))
M3_ZeroInfl_AltCount2_robust.se  <- as.double(M3_ZeroInfl_AltCount2_robust.se) 
M3_ZeroInfl_AltCount2_robust.se_Count <- M3_ZeroInfl_AltCount2_robust.se[1:12]
M3_ZeroInfl_AltCount2_robust.se_Zero <- M3_ZeroInfl_AltCount2_robust.se[13:24]


#  Just Characteristics + External Edits 
M3_ZeroInfl_AltCount3 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ vote_maxdiff_relative + sex + birthyear +
                                    ExternalEdits_per_MoC_Session + session + party_dual + Chamber | ExternalEdits_per_MoC_Session +  sex + birthyear + party_dual + Chamber,
                                  data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltCount2_Robust <- coeftest(M3_ZeroInfl_AltCount2, vcov = vcovCL(M3_ZeroInfl_AltCount2, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltCount3_cov <- vcovCL(M3_ZeroInfl_AltCount3, cluster = df_Zinb$pageid)
M3_ZeroInfl_AltCount3_robust.se <- sqrt(diag(M3_ZeroInfl_AltCount3_cov))
M3_ZeroInfl_AltCount3_robust.se  <- as.double(M3_ZeroInfl_AltCount3_robust.se) 
M3_ZeroInfl_AltCount3_robust.se_Count <- M3_ZeroInfl_AltCount3_robust.se[1:12]
M3_ZeroInfl_AltCount3_robust.se_Zero <- M3_ZeroInfl_AltCount3_robust.se[13:24]




# Out: BP_Edits(3_Appendix)_ZinB_DifferentZeros_Count.html - Count part: OUTPUT NOT INCLUDED INTO THESIS 
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltCount2, M3_ZeroInfl_AltCount3, 
          se = list(M3_ZeroInfl_robust.se_Count, M3_ZeroInfl_AltCount2_robust.se_Count, M3_ZeroInfl_AltCount3_robust.se_Count), 
          dep.var.caption  = c("Count Part (NegBin): Number of Edits"),
          dep.var.labels=c(""), 
          column.labels = c("All Variables", "Non person-specific<br>characteristics", "Person-specific<br>characteristics"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table A3. Modelling the Number of Beneficial Political Edits using ZINB: Variations in the Inflation Part (logit)", model.names = F, 
                   notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are not exponeniated.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          #out="BP_Edits(Table_A3_Appendix)_ZinB_DifferentZeros_Count.html",  
          type = "text")



# Out: BP_Edits(3_Appendix)_ZinB_DifferentZeros_Zero.html - Zero part: OUTPUT NOT INCLUDED INTO THESIS 
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltCount2, M3_ZeroInfl_AltCount3, zero.component = TRUE,
          se = list(M3_ZeroInfl_robust.se_Zero , M3_ZeroInfl_AltCount2_robust.se_Zero , M3_ZeroInfl_AltCount3_robust.se_Zero ), 
          dep.var.caption  = c("Inflation Part (logit): Pr of Zero Edits"), 
          dep.var.labels=c(""), 
          column.labels = c("All Variables", "Non person-specific<br>characteristics", "Person-specific<br>characteristics"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table A4. Modeling the likelihood of Beneficial Political Edits using ZINB: Variations in the Inflation Part (logit)", model.names = F, 
          #style = "qje",  #model.numbers  = FALSE
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}", 
                    "\\parbox[t]{\\textwidth}{Coef and SE are not exponeniated.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          #out="BP_Edits(Table_A4_Appendix)_ZinB_DifferentZeros_Zero.html", 
          type = "text")








# One Table with all ZINB-Models and non-exponentiated coefficients: 


# Out: AlllEdits(2)_Model_Results_Count.html - Count part
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpecE, 
          M3_ZeroInfl_AltCount2, M3_ZeroInfl_AltCount3, 
          se = list(M3_ZeroInfl_robust.se_Count, M3_ZeroInfl_AltSpecA_robust.se_Count, M3_ZeroInfl_AltSpecE_robust.se_Count, 
                    M3_ZeroInfl_AltCount2_robust.se_Count, M3_ZeroInfl_AltCount3_robust.se_Count), 
          dep.var.caption  = c("Count Part (NegBin): Number of Edits"),
          dep.var.labels=c(""), 
          column.labels = c("Full Sample", "99 Percentile of<br>Congress Edits", "99 Percentile of<br>External Edits",  
                            "Non person-specific<br>characteristics", "Person-specific<br>characteristics<br>and External Edits"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table A7. Modeling the number of beneficial edits using ZINB: Different specifications", model.names = F, 
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{District competitiveness is computed as the relative difference in votes to the second best candidate.}",
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are not exponeniated.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          out="Table A7 (BP_Edits(Table_A5_Appendix)_ZinB_Specifications_Count).html", 
          type = "text")




# Out: AlllEdits(2)_Model_Results_Zero.html - Zero part
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpecE,
          M3_ZeroInfl_AltCount2, M3_ZeroInfl_AltCount3, zero.component = TRUE,
          se = list(M3_ZeroInfl_robust.se_Zero , M3_ZeroInfl_AltSpecA_robust.se_Zero , M3_ZeroInfl_AltSpecE_robust.se_Zero ,
                    M3_ZeroInfl_AltCount2_robust.se_Zero , M3_ZeroInfl_AltCount3_robust.se_Zero ), 
          dep.var.caption  = c("Inflation Part (logit): Pr of Zero Edits"), 
          dep.var.labels=c(""), 
          column.labels = c("Full Sample", "99 Percentile of<br>Congress Edits", "99 Percentile of<br>External Edits",
                            "Non person-specific<br>characteristics", "Person-specific<br>characteristics<br>and External Edits"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table A6. Modeling the likelihood of beneficial edits using ZINB: Different specifications", model.names = F, 
          #style = "qje",  #model.numbers  = FALSE
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{District competitiveness is computed as the relative difference in votes to the second best candidate.}",
                     "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are not exponeniated.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          out="Table A6 (BP_Edits(Table_A6_Appendix)_ZinB_Specifications_Zero).html",  type = "text")








#########################################################################################################################
####################################          Ideenspeicher            ################################################## 
#########################################################################################################################


# quasi-Poisson to address overdispersion

# 2) quasi-Poisson GLM
M1_quasi <- glm(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative  + sex + birthyear + 
                  ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
                family=quasipoisson,
                data = df_Zinb)

summary(M1_quasi) # Dispersion parameter is the same as calculated manually 
summ(M1_quasi, confint = T, digits = 3, vifs = T)   
#Robust Standart Erros on legislator-level
M1_quasi_Robust <- coeftest(M1_quasi, vcov = vcovCL(M1_quasi, cluster = df_Zinb$pageid))


M1_quasi_cov <- vcovCL(M1_quasi, cluster = df_Zinb$pageid)
M1_quasi_robust.se <- sqrt(diag(M1_quasi_cov))




# Waldtest can be used to compare different variable selection 
waldtest(M3_ZeroInfl, M3_ZeroInfl2)


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




# Alternative Specification: Excluding MoCs that served in both Chambers 

df_Zinb_Unique <- df_Zinb %>% dplyr::select(c(Chamber, pageid))
df_Zinb_Unique <- unique(df_Zinb_Unique)
doubles <- df_Zinb_Unique %>% group_by(pageid) %>% filter(n()>1) 
Double_Mocs <- unique(doubles$pageid)
`%notin%` <- Negate(`%in%`)
df_Zinb_Unique <- df_Zinb %>% filter(pageid %notin% Double_Mocs)

M3_ZeroInfl_AltSpec_NoDoubles <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative + sex + birthyear +
                                            ExternalEdits_per_MoC_Session + session + party_dual + Chamber | vote_maxdiff_relative + sex + birthyear +
                                            ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
                                          data = df_Zinb_Unique , dist = "negbin")

#M3_ZeroInfl_AltSpec_NoDoubles_Robust <- coeftest(M3_ZeroInfl_AltSpec_NoDoubles, vcov = vcovCL(M3_ZeroInfl_AltSpec_NoDoubles, cluster = df_Zinb_Unique$pageid))
M3_ZeroInfl_AltSpec_NoDoubles_cov <- vcovCL(M3_ZeroInfl_AltSpec_NoDoubles , cluster = df_Zinb_Unique$pageid)
M3_ZeroInfl_AltSpec_NoDoubles_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec_NoDoubles_cov))
M3_ZeroInfl_AltSpec_NoDoubles_robust.se <- as.double(M3_ZeroInfl_AltSpec_NoDoubles_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)





# 4. Alternative Specification of: vote_maxdiff_relative 

# 1/2Categories 
Number_of_Breaks =  c(0,1/2, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_2 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_2 <- as.double(df_Zinb$Category_Vote_MaxDiff_2)
df_Zinb$Category_Vote_MaxDiff_2 <- df_Zinb$Category_Vote_MaxDiff_2 / 2 #normalistion 
table(df_Zinb$Category_Vote_MaxDiff_2) #check whether it worked


# 1/3Categories 
Number_of_Breaks =  c(0,1/3,2/3, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_3 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_3 <- as.double(df_Zinb$Category_Vote_MaxDiff_3)
df_Zinb$Category_Vote_MaxDiff_3 <- df_Zinb$Category_Vote_MaxDiff_3 / 3 #normalistion 
table(df_Zinb$Category_Vote_MaxDiff_3) #check whether it worked


# 1/4 Categories
Number_of_Breaks =  c(0,1/4,2/4, 3/4, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_4 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_4 <- as.double(df_Zinb$Category_Vote_MaxDiff_4)
df_Zinb$Category_Vote_MaxDiff_4 <- df_Zinb$Category_Vote_MaxDiff_4 / 4 #normalisation 
table(df_Zinb$Category_Vote_MaxDiff_4) #check whether it worked


# 1/5 Categories 
Number_of_Breaks =  c(0,1/5,2/5, 3/5, 4/5, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_5 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_5 <- as.double(df_Zinb$Category_Vote_MaxDiff_5)
df_Zinb$Category_Vote_MaxDiff_5 <- df_Zinb$Category_Vote_MaxDiff_5 / 5 #normalistion 
table(df_Zinb$Category_Vote_MaxDiff_5) #check whether it worked


# 1/6 Categories 
Number_of_Breaks =  c(0,1/6,2/6, 3/6, 4/6, 5/6, 1) 
xs = quantile(df_Zinb$vote_maxdiff_relative, Number_of_Breaks)
xs[1] = 0 #set lowest break to 0 
df_Zinb <- df_Zinb %>% mutate(Category_Vote_MaxDiff_6 = cut(vote_maxdiff_relative, breaks=xs ))#, labels= LABELS))
df_Zinb$Category_Vote_MaxDiff_6 <- as.double(df_Zinb$Category_Vote_MaxDiff_6)
df_Zinb$Category_Vote_MaxDiff_6 <- df_Zinb$Category_Vote_MaxDiff_6 / 6 #normalistion 
table(df_Zinb$Category_Vote_MaxDiff_6) #check whether it worked


# Ad 1/2
M3_ZeroInfl_AltSpec2 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_2 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec2_Robust <- coeftest(M3_ZeroInfl_AltSpec2, vcov = vcovCL(M3_ZeroInfl_AltSpec2, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec2_cov <- vcovCL(M3_ZeroInfl_AltSpec2 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec2_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec2_cov))
M3_ZeroInfl_AltSpec2_robust.se <- as.double(M3_ZeroInfl_AltSpec2_robust.se) 


# Ad 1/3
M3_ZeroInfl_AltSpec3 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_3 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec3_Robust <- coeftest(M3_ZeroInfl_AltSpec3, vcov = vcovCL(M3_ZeroInfl_AltSpec3, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec3_cov <- vcovCL(M3_ZeroInfl_AltSpec3 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec3_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec3_cov))
M3_ZeroInfl_AltSpec3_robust.se <- as.double(M3_ZeroInfl_AltSpec3_robust.se) 


# Ad 1/4
M3_ZeroInfl_AltSpec4 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_4 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec4_Robust <- coeftest(M3_ZeroInfl_AltSpec4, vcov = vcovCL(M3_ZeroInfl_AltSpec4, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec4_cov <- vcovCL(M3_ZeroInfl_AltSpec4 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec4_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec4_cov))
M3_ZeroInfl_AltSpec4_robust.se <- as.double(M3_ZeroInfl_AltSpec4_robust.se) 


# Ad 1/5
M3_ZeroInfl_AltSpec5 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_5 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec5_Robust <- coeftest(M3_ZeroInfl_AltSpec5, vcov = vcovCL(M3_ZeroInfl_AltSpec5, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec5_cov <- vcovCL(M3_ZeroInfl_AltSpec5 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec5_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec5_cov))
M3_ZeroInfl_AltSpec5_robust.se <- as.double(M3_ZeroInfl_AltSpec5_robust.se) 


# Ad 1/6
M3_ZeroInfl_AltSpec6 <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~ Category_Vote_MaxDiff_6 + sex + birthyear +
                                   ExternalEdits_per_MoC_Session + session + party_dual + Chamber | session + ExternalEdits_per_MoC_Session ,
                                 data = df_Zinb , dist = "negbin")
#M3_ZeroInfl_AltSpec6_Robust <- coeftest(M3_ZeroInfl_AltSpec6, vcov = vcovCL(M3_ZeroInfl_AltSpec6, cluster = df_Zinb$pageid))
M3_ZeroInfl_AltSpec6_cov <- vcovCL(M3_ZeroInfl_AltSpec6 , cluster = df_Zinb$pageid)
M3_ZeroInfl_AltSpec6_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec6_cov))
M3_ZeroInfl_AltSpec6_robust.se <- as.double(M3_ZeroInfl_AltSpec6_robust.se) 




# Out: BP_Edits(3_Appendix)_ZinB_VoteDiffCategories.html 

stargazer(M3_ZeroInfl_AltSpec2, M3_ZeroInfl_AltSpec3, M3_ZeroInfl_AltSpec4, M3_ZeroInfl_AltSpec5, M3_ZeroInfl_AltSpec6, M3_ZeroInfl,
          se = list(M3_ZeroInfl_AltSpec2_robust.se, M3_ZeroInfl_AltSpec3_robust.se, M3_ZeroInfl_AltSpec4_robust.se, M3_ZeroInfl_AltSpec5_robust.se, M3_ZeroInfl_AltSpec6_robust.se, M3_ZeroInfl_robust.se), 
          column.labels=c("1/2", "1/3", "1/4","1/5","1/6", "noCategory"), 
          out="BP_Edits(3_Appendix)_ZinB_VoteDiffCategories.html",  type = "text",  
          title="Zero-inflated models: different vote_diff-Categories", align=TRUE) 




# 3. Alternative Specification: ZinB model with additional variable "Views"

# It was not possible to incoporate a variable for popularity based on page-views
# as data was just avaiable from 2009 upwards and excluding sessions 109-111 would reduce the data set so drastically in size 
# that the zinb-models do not work anymore 

# load: Views_per_pageid_session
df_Zinb_Views <- left_join(df_Zinb, Views_per_pageid_session, by = "pageid_session")
df_Zinb_Views <- df_Zinb_Views %>% drop_na(ProfileViewsPerSession09_to_16)

# Using Summary Statistics to Compute Categories for High of Number of Views (1-5)
summary(df_Zinb_Views$ProfileViewsPerSession09_to_16)
# Categories: 1. lower 25%, 2. 26%-50%, 3. 51%-75%, 4. 76%-95% , 5. 96%-100%
df_Zinb_Views$ViewCategoryPerSession = 0
for(i in 1:length(df_Zinb_Views$ProfileViewsPerSession09_to_16)) {
  if (df_Zinb_Views$ProfileViewsPerSession09_to_16[i] <= 34148) {
    df_Zinb_Views$ViewCategoryPerSession[i] = 1   } #firtst quartile
  if (df_Zinb_Views$ProfileViewsPerSession09_to_16[i] > 34148 && df_Zinb_Views$ProfileViewsPerSession09_to_16[i] <= 52212) {
    df_Zinb_Views$ViewCategoryPerSession[i] = 2   } # second quartile
  if (df_Zinb_Views$ProfileViewsPerSession09_to_16[i] > 52212  && df_Zinb_Views$ProfileViewsPerSession09_to_16[i] <=  110466) {
    df_Zinb_Views$ViewCategoryPerSession[i] = 3  }} # third quartile
# Get last 5% - threshold: 
ShortLived_df <- df_Zinb_Views%>% filter( ViewCategoryPerSession == 0) #contains the last quartile -> taking the last two decils (25%*0,1*2 = 5%)
quantile(ShortLived_df$ProfileViewsPerSession09_to_16, prob = seq(0, 1, length = 11), type = 5) # 5% = bigger than 435811.8
# Use 5% threshold to compute last 2 categories: 
for(i in 1:length(df_Zinb_Views$ProfileViewsPerSession09_to_16)) {
  if (df_Zinb_Views$ProfileViewsPerSession09_to_16[i] > 110466 && df_Zinb_Views$ProfileViewsPerSession09_to_16[i] <= 435811 ) {
    df_Zinb_Views$ViewCategoryPerSession[i] = 4    } #forth quartile minus the top 5%
  if (df_Zinb_Views$ProfileViewsPerSession09_to_16[i] > 435811) {
    df_Zinb_Views$ViewCategoryPerSession[i] = 5   }} # last 5%

M3_ZeroInfl_View <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative   + sex + birthyear + 
                               ExternalEdits_per_MoC_Session + session + party_dual + Chamber + ViewCategoryPerSession | vote_maxdiff_relative  + sex + birthyear + 
                               ExternalEdits_per_MoC_Session + session + party_dual + Chamber  ,
                             data = df_Zinb_Views , dist = "negbin")


M4_Hurdle_Views  <- hurdle(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative   + sex + birthyear + 
                             ExternalEdits_per_MoC_Session + session + party_dual + Chamber  + ViewCategoryPerSession  | session   + ViewCategoryPerSession + ExternalEdits_per_MoC_Session ,
                           data = df_Zinb_Views,  dist = "negbin")
summary(M4_Hurdle_Views)
# With clustered standart errors by individual MoC
M4_Hurdle_Views_Robust <- coeftest(M4_Hurdle_Views , vcov = vcovCL(M4_Hurdle_Views, cluster = df_Zinb_Views$pageid))

summary(M3_ZeroInfl_View)
M3_ZeroInfl_View_Rob <- coeftest(M3_ZeroInfl_View, vcov = vcovCL(M3_ZeroInfl_View, cluster = df_Zinb_Views$pageid))
#M3_ZeroInfl_View_Robust <- coeftest(M3_ZeroInfl_View , vcov = vcovCL(M3_ZeroInfl_View , cluster = df_Zinb$pageid))
M3_ZeroInfl_View_cov <- vcovCL(M3_ZeroInfl_View, cluster = df_Zinb_Views$pageid)
M3_ZeroInfl_View_cov_robust.se <- sqrt(diag(M3_ZeroInfl_View_cov))
M3_ZeroInfl_View_cov_robust.se <- as.double(M3_ZeroInfl_View_cov_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)





# Alternative Specification: Excluding MoCs that served in both Chambers 

df_Zinb_Unique <- df_Zinb %>% dplyr::select(c(Chamber, pageid))
df_Zinb_Unique <- unique(df_Zinb_Unique)
doubles <- df_Zinb_Unique %>% group_by(pageid) %>% filter(n()>1) 
Double_Mocs <- unique(doubles$pageid)
`%notin%` <- Negate(`%in%`)
df_Zinb_Unique <- df_Zinb %>% filter(pageid %notin% Double_Mocs)

M3_ZeroInfl_AltSpec_NoDoubles <- zeroinfl(All_Positive_Politically_CongressEdits_Per_MoC_Session ~  vote_maxdiff_relative + sex + birthyear +
                                            ExternalEdits_per_MoC_Session + session + party_dual + Chamber | vote_maxdiff_relative + sex + birthyear +
                                            ExternalEdits_per_MoC_Session + session + party_dual + Chamber,
                                          data = df_Zinb_Unique , dist = "negbin")

#M3_ZeroInfl_AltSpec_NoDoubles_Robust <- coeftest(M3_ZeroInfl_AltSpec_NoDoubles, vcov = vcovCL(M3_ZeroInfl_AltSpec_NoDoubles, cluster = df_Zinb_Unique$pageid))
M3_ZeroInfl_AltSpec_NoDoubles_cov <- vcovCL(M3_ZeroInfl_AltSpec_NoDoubles , cluster = df_Zinb_Unique$pageid)
M3_ZeroInfl_AltSpec_NoDoubles_robust.se <- sqrt(diag(M3_ZeroInfl_AltSpec_NoDoubles_cov))
M3_ZeroInfl_AltSpec_NoDoubles_robust.se <- as.double(M3_ZeroInfl_AltSpec_NoDoubles_robust.se) # for some reason the standard erros are not displayed without this transformation (prob. due to scientific notation)












