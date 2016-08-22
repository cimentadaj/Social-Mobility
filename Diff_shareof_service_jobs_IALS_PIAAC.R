
# this script computes the difference in the share of service class jobs between the IALS survey and the PIAAC survey.
# It builds on the scripts outlined here:

## To run this you have to run the scripts
# /Users/cimentadaj/Google Drive/Gosta project/ials/data/IALS - Cleaning and preparing dataset.R

# and 

# /Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Analysis_PIAAC_survey.R

library(dplyr)
c1 <- as.data.frame(cbind(name=vec1,vec2))
c2 <- as.data.frame(cbind(name=p1,p2))

c3 <- inner_join(c1,c2, by="name") %>% mutate(vec2 = as.numeric(as.character(vec2)), p2 = as.numeric(as.character(p2)), diff = p2 - vec2)
c3[,2:4] <- lapply(c3[,2:4], round, 2)
names(c3) <- c("Names","IALS%","PIAAC%","Difference")


