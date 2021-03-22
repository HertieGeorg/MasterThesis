# 10_ZINB_Analysis


#install.packages("pscl")
#install.packages("MASS")
#install.packages("boot")
#library(pscl)
#library(MASS)
#library(boot)


Source: https://stats.idre.ucla.edu/r/dae/zinb/
  
  
zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")

zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)