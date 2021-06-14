

Coefficient_Names <-  c("Intercept", "District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                        "110th Session", "111th Session", "112th Session", "113th Session", 
                        "114th Session", "Party Affiliation", "Chamber(Senate)")

# Code Taken from: https://rpubs.com/kaz_yos/pscl-2
# Exponentiated coefficients: M3_ZeroInfl
M3_ZeroInfl_expCoef <- exp(coef((M3_ZeroInfl)))
M3_ZeroInfl_expCoef <- matrix(M3_ZeroInfl_expCoef, ncol = 2)
M3_ZeroInfl_expCoef_Mat <- M3_ZeroInfl_expCoef
rownames(M3_ZeroInfl_expCoef_Mat) <- Coefficient_Names
colnames(M3_ZeroInfl_expCoef_Mat) <- c("Count_model","Zero_inflation_model")
M3_ZeroInfl_expCoef_Mat


# Exponentiated coefficients: M3_ZeroInfl
M3_ZeroInfl_AltSpecA_expCoef <- exp(coef((M3_ZeroInfl_AltSpecA)))
M3_ZeroInfl_AltSpecA_expCoef <- matrix(M3_ZeroInfl_AltSpecA_expCoef, ncol = 2)
M3_ZeroInfl_AltSpecA_Mat <- M3_ZeroInfl_AltSpecA_expCoef
rownames(M3_ZeroInfl_AltSpecA_Mat) <- Coefficient_Names
colnames(M3_ZeroInfl_AltSpecA_Mat) <- c("Count_model","Zero_inflation_model")
M3_ZeroInfl_AltSpecA_Mat


# Exponentiated coefficients: M3_ZeroInfl
M3_ZeroInfl_AltSpecE_expCoef <- exp(coef((M3_ZeroInfl_AltSpecE)))
M3_ZeroInfl_AltSpecE_expCoef <- matrix(M3_ZeroInfl_AltSpecE_expCoef, ncol = 2)
M3_ZeroInfl_AltSpecE_Mat <- M3_ZeroInfl_AltSpecE_expCoef
rownames(M3_ZeroInfl_AltSpecE_Mat) <- Coefficient_Names
colnames(M3_ZeroInfl_AltSpecE_Mat) <- c("Count_model","Zero_inflation_model")
M3_ZeroInfl_AltSpecE_Mat


coef_M3_ZeroInfl_Count = M3_ZeroInfl_expCoef[,1]
coef_M3_ZeroInfl_Count <- coef_M3_ZeroInfl_Count -1

coef_M3_ZeroInfl_AltSpecA_Count = M3_ZeroInfl_AltSpecA_expCoef[,1]
coef_M3_ZeroInfl_AltSpecA_Count  <- coef_M3_ZeroInfl_AltSpecA_Count  -1

coef_M3_ZeroInfl_AltSpecE_Count =  M3_ZeroInfl_AltSpecE_expCoef[,1]
coef_M3_ZeroInfl_AltSpecE_Count <- coef_M3_ZeroInfl_AltSpecE_Count - 1


coef_M3_ZeroInfl_Zero =  M3_ZeroInfl_expCoef[,2]
coef_M3_ZeroInfl_Zero <- coef_M3_ZeroInfl_Zero -1

coef_M3_ZeroInfl_AltSpecA_Zero = M3_ZeroInfl_AltSpecA_expCoef[,2]
coef_M3_ZeroInfl_AltSpecA_Zero  <- coef_M3_ZeroInfl_AltSpecA_Zero  -1

coef_M3_ZeroInfl_AltSpecE_Zero = M3_ZeroInfl_AltSpecE_expCoef[,2]
coef_M3_ZeroInfl_AltSpecE_Zero <- coef_M3_ZeroInfl_AltSpecE_Zero - 1




# Exponentiated (and clustered) standard errors: 
# 1. Exponentiated coefficients (which are the same for both the normal and robust model)
# 2. Coefficient variance (which is also the same for both normal and robust as it is calculated based on coefficient)

# Code Taken from: https://www.andrewheiss.com/blog/2016/04/25/convert-logistic-regression-standard-errors-to-odds-ratios-with-r/
# Author: Andrew Heiss

# M3_ZeroInfl
var.diag = diag(vcovCL(M3_ZeroInfl, cluster = df_Zinb$pageid)) #24 values 
# already robust, as vcovCL instead of vcov, and clustering using pageid 
or = M3_ZeroInfl_expCoef[]
or.se = sqrt(or^2 * var.diag)
se_M3_ZeroInfl_Count = or.se[,1]
se_M3_ZeroInfl_Zero = or.se[,2]

#Again for AltSpecA
var.diag = diag(vcovCL(M3_ZeroInfl_AltSpecA , cluster = df_Zinb_AltSpecA$pageid)) #24 values 
or = M3_ZeroInfl_AltSpecA_expCoef[]
or.se = sqrt(or^2 * var.diag)
se_M3_ZeroInfl_AltSpecA_Count = or.se[,1]
se_M3_ZeroInfl_AltSpecA_Zero = or.se[,2]


#Again for AltSpecE
var.diag = diag(vcovCL(M3_ZeroInfl_AltSpecE , cluster = df_Zinb_AltSpecE$pageid)) #24 values 
or = M3_ZeroInfl_AltSpecE_expCoef[]
or.se = sqrt(or^2 * var.diag)
se_M3_ZeroInfl_AltSpecE_Count = or.se[,1]
se_M3_ZeroInfl_AltSpecE_Zero = or.se[,2]







# Out: AlllEdits(2)_Model_Results_Count.html - Count part
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpecE,  
          coef = list(coef_M3_ZeroInfl_Count, coef_M3_ZeroInfl_AltSpecA_Count, coef_M3_ZeroInfl_AltSpecE_Count),
          se = list(se_M3_ZeroInfl_Count, se_M3_ZeroInfl_AltSpecA_Count, se_M3_ZeroInfl_AltSpecE_Count),
          #se = list(M3_ZeroInfl_robust.se, M3_ZeroInfl_AltSpecA_robust.se, M3_ZeroInfl_AltSpecE_robust.se), 
          dep.var.caption  = c("Count Part (NegBin): Number of Edits"),
          dep.var.labels=c(""), 
          column.labels = c("Full Sample", "99 Percentile of<br>Congress Edits", "99 Percentile of<br>External Edits"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                            "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table 4. Modeling the number of Congress edits using ZINB: Different specifications", model.names = F, 
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{District competitiveness is computed as the relative difference in votes to the second best candidate.}",
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are exponentiated and can be interpretated as percentage change.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          out="Table 4. (AllEdits(Table2_Exp)_ZinB_Specifications_Count).html", 
          omit = c("Constant"),
          type = "text")




# Out: AlllEdits(2)_Model_Results_Zero.html - Zero part
stargazer(M3_ZeroInfl, M3_ZeroInfl_AltSpecA, M3_ZeroInfl_AltSpecE,  zero.component = TRUE,
          coef = list(coef_M3_ZeroInfl_Zero, coef_M3_ZeroInfl_AltSpecA_Zero, coef_M3_ZeroInfl_AltSpecE_Zero),
          se = list(se_M3_ZeroInfl_Zero, se_M3_ZeroInfl_AltSpecA_Zero, se_M3_ZeroInfl_AltSpecE_Zero),
          dep.var.caption  = c("Inflation Part (logit): Pr of Zero Edits"), 
          dep.var.labels=c(""), 
          column.labels = c("Full Sample", "99 Percentile of<br>Congress Edits", "99 Percentile of<br>External Edits"),
          covariate.labels=c("District Competitiveness", "Gender(Male)", "Age", "External Edits", 
                             "110th Session", "111th Session", "112th Session", "113th Session", 
                             "114th Session", "Party Affiliation(R)", "Chamber(Senate)"),
          title="Table 3. Modeling the likelihood of Congressional Edits using ZINB: Different Specifications", model.names = F, 
          #style = "qje",  #model.numbers  = FALSE
          notes.append = F, notes.align = "r",
          notes = c("\\parbox[t]{\\textwidth}{Age is higher for younger legislators (based on year of birth). Independents are allocated to the party they caucus with. }", 
                    "\\parbox[t]{\\textwidth}{District competitiveness is computed as the relative difference in votes to the second best candidate.}",
                    "\\parbox[t]{\\textwidth}{109th session as baseline. Cluster-robust standard errors taken by legislator.}",  
                    "\\parbox[t]{\\textwidth}{Coef and SE are exponentiated and can be interpretated as percentage change.}",
                    "\\parbox[t]{\\textwidth}{ *p<0.1; **p<0.05; ***p<0.01}"),
          align=TRUE, omit.stat=c("aic", "theta" ),
          omit = c("Constant"),
          out="Table 3 (AllEdits(Table1_Exp)_ZinB_Specifications_Zero).html",  type = "text")

