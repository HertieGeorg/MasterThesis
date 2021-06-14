
# This file lists all packages needed and installs them (including the respective libraries)



#02 
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyverse")
install.packages("readr")

library(readxl)
library(dplyr)
library(readr)
library(tidyverse)


#03
library(devtools)
devtools::install_github("saschagobel/legislatoR")
install.packages("legislatoR")
library(legislatoR)


#04
install.packages("ggplot2")
install.packages("rvest")
install.packages("lubridate")
library(rvest)
library(lubridate)
library(ggplot2)

#05 
install.packages("XML")
library(XML)
library(lubridate)


#09
devtools::install_github("strengejacke/sjmisc")
install.packages("sjmisc")
install.packages("data.table")
install.packages("dplyr")
install.packages("formattable")
install.packages("tidyr")
library(fastDummies)
library(tidyr) 
require(data.table) 
library(stringr)
library(hrbrthemes)
library(formattable)
library(viridis)
library(forcats)
library(gridExtra)
library(sjmisc)
library(stargazer)
library(RColorBrewer)


#10
install.packages("pscl")
install.packages("MASS")
install.packages("boot")
library(pscl)
library(MASS)
library(boot)

#11
install.packages("formattable")
install.packages("pscl")
install.packages("MASS")
install.packages("boot")
install.packages("lmtest")
install.packages("AER")
install.packages("sandwich")
install.packages("jtools")
install.packages("stargazer")
install.packages("broom")

library(pscl)
library(MASS)
library(boot)
library(lattice)
library(AER)
library(lmtest) #provides coeftest function 
library(sandwich) # provides vcovHC function for calculating robust standard errors
library(stargazer)
library(jtools)
library(formattable)
library(readr)  
library(broom)
