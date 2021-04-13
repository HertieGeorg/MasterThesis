# DAGs for my Model Specification 

library(dagitty)
library(ggdag)

CausalGraphDrug <- dagify(DV ~ CoD , DS ~ CoD, DV~ P, DV ~ A, DV ~ OE,  DV ~ DS, DV ~ AI,
                          labels = c("DV" = "Number Congress Edits",
                                     "CoD" = "Distritcts's Competitiveness", 
                                     "A" = "Age of MoC",
                                     "OE" = "Overall Edits",
                                     "P" = "Popularity",
                                     "DS" = "Days Served", "IA" = "Internet Access"))

# Oder falls ich Number of Congress Edits und Overall Edits durch Days Served teile:


CausalGraphDrug2 <- dagify(DV ~ CoD ,  DV~ P, DV ~ A, DV ~ OE,   DV ~ AI,
                          labels = c("DV" = "Number Congress Edits",
                                     "CoD" = "Distritcts's Competitiveness", 
                                     "A" = "Age of MoC",
                                     "OE" = "Overall Edits",
                                     "P" = "Popularity",
                                     "IA" = "Internet Access"))



ggdag(CausalGraphDrug, 
      text = TRUE, 
      use_labels = "label") + remove_axes() + remove_grid()



ggdag(CausalGraphDrug2, 
      text = TRUE, 
      use_labels = "label") + remove_axes() + remove_grid()


# Dependent Variable: Number of Edits of a MoC-Profile Coming from withing Congress in observed time frame 
# "Popularity", measured in Average Views per Day on Wikipedia Profile 
# "Overall Edits", all edits on a MoC-Profile during observed time frame, not just the ones coming from within Congress 



# Simons Variables
party affiliation -> no, internet access instead (maybe show high correlation between them), there is not reason why party should have an effect (maybe look for one, z.B. differences in campaiging approaches between parties)
gender -> yes, following Simons Argumentation: "In a similar vein,
#we follow studies indicating that gender differences exist in online campaigning approaches(Druckman et al., 2007, p. 437)
#as well as in terms of Internet use (Korupp & Szydlik, 2005; Schleife, 2010).
#As predictions are in part contrary, however, we again have no clear 
#expetations as to the direction of a possible effect."


age -> yes, use of internet of individual MoC (CODE DONE)
popularity -> yes, cost of being detected  (CODE NOT DONE)
electoral system and cycle -> yes, competitiveness of districts (CODE DONE)
regional digital disparities -> yes, internet access (CODE NOT DONE)





