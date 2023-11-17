#Script file that changes an SPSS file (.sav) to a .csv file 

library(haven)
library(dplyr)
library(tidyverse)
Internet_naming_default_gig <- read_sav("filename.sav", .name_repair = "check_unique") 

Internet_naming_default_gig %>%
  pivot_longer(contains("Q"))

write.csv(Internet_naming_default_gig, file = "filename.csv", row.names = F) 

Internet_naming_default_gig[1,]

#Check that the SPSS file works and contains something to be transformed.
library(foreign)

data<-read.spss("filename.sav", to.data.frame = T)
