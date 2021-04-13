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
  

zinb <- Characteristics_Data_MoCs

# Subsitute NAs to 0s for "AllCongressEdits_Per_MoC"
zinb$AllCongressEdits_Per_MoC[is.na(zinb$AllCongressEdits_Per_MoC) == T] = 0
table(is.na(zinb$AllCongressEdits_Per_MoC))

zinb$sex[is.na(zinb$sex) == T] = 0
zinb$YearBirth[is.na(zinb$YearBirth) == T] = 0
zinb$OverallEdits_inTenure_perDayServed[is.na(zinb$OverallEdits_inTenure_perDayServed) == T] = 0


model <- zeroinfl(AllCongressEdits_Per_MoC ~ sex + YearBirth + Combined_Mean_vote_maxdiff_relative | OverallEdits_inTenure_perDayServed, data = zinb, dist = "negbin", EM = F)

?zeroinfl

summary(model)
