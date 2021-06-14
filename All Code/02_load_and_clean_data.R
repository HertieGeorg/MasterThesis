# This file loads, cleans and combines the used data
# Data: M-Turk Classification of Edits 
# Legislator Database: characteristics of MPs 

# M-Turk Data

# Load mturk data 
mturk_df  <- read_csv("Data/Batch_200780_batch_results.csv")

# Load Excel with alternative variabl names 
varnames <-  read_excel("Data/codebook_edits_classification.xlsx")

# Drop unused variables 
mturk_df <- dplyr::select(mturk_df, -c(HITId,  HITTypeId, MaxAssignments, AssignmentId, AcceptTime, SubmitTime, WorkTimeInSeconds, Title, Description, Keywords, Reward, CreationTime, RequesterAnnotation, AssignmentDurationInSeconds, AutoApprovalDelayInSeconds, Expiration, NumberOfSimilarHITs, LifetimeInSeconds, AssignmentStatus, AutoApprovalTime, ApprovalTime, RejectionTime, RequesterFeedback, LifetimeApprovalRate, Last30DaysApprovalRate, Last7DaysApprovalRate, Approve, Reject))


# Keep only Angie's and Lorenzo's classifications
mturk_df <- mturk_df %>% filter( WorkerId == "A1TWO417PJY35G" | WorkerId == "AEW8FLMJ3C4U8")

mturk_df$id <- as.numeric(as.factor(mturk_df$Input.website_url))

# Extract names of MPs from Wikipedia_URL using Regular Expressions
# Source: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

# e.g.: https://en.wikipedia.org/w/index.php?title=John_Conyers&diff=prev&oldid=121788295

mturk_df$Name <- str_extract(mturk_df$Input.website_url, "=(.+)&diff")

# Wikipedia Names 
mturk_df$Name <- stringr::str_replace_all(mturk_df$Name, "=", "")
mturk_df$Name <- stringr::str_replace_all(mturk_df$Name, "&diff", "")
# Clean Names
mturk_df$Name_clean <- stringr::str_replace_all(mturk_df$Name, " \\((.+)", " ")
mturk_df$Name_clean <- stringr::str_replace_all(mturk_df$Name_clean, "_", " ")

head(mturk_df$Name)






