
###################################
###################################
#####  REPLICATION FILE FOR: ######
###  MY HISTORY OR OUR HISTORY? ###
###################################
###################################

# ========================================================================= #
# - Script: Cleaning Raw Data
# - Author: Nicholas Haas (nick.haas@ps.au.dk) 
#           Emmy Lindstam (emmy.lindstam@ie.edu)
# ========================================================================= #

# Clear environment
rm(list = ls())

# Load packages and data for analysis:
# source(here::here("00 - PACKAGES.R")) THIS WAS ORIGINAL SCRIPT FROM REP PACKAGE I HAVE TO ADAPT FOR MY COMPUTER
source(here::here("Class 2", "project", "00 - PACKAGES.R"))
# dataS <- read.csv(here("data/DataforCleaning.csv"), row.names=1) # this was original script i have to adapt
dataS <- read.csv(here("Class 2", "project", "DataForCleaning.csv"), row.names=1)

############################
# Code treatment variable: #    
############################

# Overview of observations in the different treatment groups
table(dataS$Treat)

# Code as factor for analysis 
dataS$Treat <- as.factor(dataS$Treat)

#######################
# GROUP COMPOSITION   #
#######################

# Group composition: respondents were randomly assigned to value between 1-10 where 1 & 2 = Muslim majority, 3-6 = Hindu majority and 7-10 = same religion/homogeneous.
table(dataS$Composition) 

# Make new variable for composition (0 = same religion, 1 = Hindu majority, 2 = Muslim majority)
dataS <- dataS %>% mutate(
  Comp = NA,
  Comp = ifelse(Composition==1,2,Comp),
  Comp = ifelse(Composition==2,2,Comp),
  Comp = ifelse(Composition==3,1,Comp),
  Comp = ifelse(Composition==4,1,Comp),
  Comp = ifelse(Composition==5,1,Comp),
  Comp = ifelse(Composition==6,1,Comp),
  Comp = ifelse(Composition==7,0,Comp),
  Comp = ifelse(Composition==8,0,Comp),
  Comp = ifelse(Composition==9,0,Comp),
  Comp = ifelse(Composition==10,0,Comp))

table(dataS$Comp)

# Make dummy variable for mixed vs. homogeneous groups (0 = homogeneous group, 1 = mixed group)
dataS <- dataS %>% mutate(
  Comp2 = NA,
  Comp2 = ifelse((Comp ==1),1,Comp2),
  Comp2 = ifelse((Comp ==2),1,Comp2),
  Comp2 = ifelse((Comp ==0),0,Comp2))

table(dataS$Comp2)

############################
# Outcome Variables:       #
############################

# SUPPLY - Willingness to be Group Representative # ----------------------

# Overview
table(dataS$Willing)

# Recode willingness to lead  so that 4 = maximum willingness (more intuitive interpretation) 
dataS$WillingR <- 5-dataS$Willing
table(dataS$WillingR)

# DEMAND - Ranking of other group members # ---------------------
table(dataS$DemandHM)
table(dataS$DemandHH)

# Recode so that 3 = most preferred and 1 = least preferred (more intuitive interpretation)
dataS$DemandHM <- 4 - dataS$DemandHM
dataS$DemandHH <- 4 - dataS$DemandHH
dataS$DemandMM <- 4 - dataS$DemandMM
dataS$DemandMH <- 4 - dataS$DemandMH

############################
# Mechanisms               #
############################

# Overview: the variable includes what words a respondent picked in the task of selecting what groups and people have most contributed to the Indian nation.
table(dataS$words)

# Make a function to count the number of Hindu and Muslim words respondents picked from the pool of words.
countWords <- function(stringvar, grouping) {
  str_split(stringvar, ",", simplify = T) %>% 
    as_tibble() %>%
    mutate_all(match, table = grouping, nomatch = 0) %>%
    apply(1, function(x) sum(x > 0))
}

# Make variables for (1) Hindu words and (2) Muslim words based on pre-registration (0 = minimum, 5 = maximum):
dataS$hinducontr <- countWords(dataS$words, as.character(c(2, 4, 6, 8, 12, 17, 18)))
dataS$muslimcontr <- countWords(dataS$words, as.character(c(5,9,10,11)))

# Overview
table(dataS$hinducontr)
table(dataS$muslimcontr)

############################
# Other Outcome Variables: #
############################

# Make an index for overall candidate evaluation based on all the separate dimensions:
dataS$Candidate <- idx_invcov(dataS$MLA_Follow_1, dataS$MLA_Follow_2, dataS$MLA_Follow_3, dataS$MLA_Follow_4, dataS$MLA_Follow_5, dataS$MLA_Follow_6, dataS$MLA_Follow_7)
hist(dataS$Candidate)

#############################
# Pre-treatment Controls:   #
#############################

# Pre-treatment controls include:
# 1. Gender (2 = male, 3 = female) #
table(dataS$Gender)

# 2. Age (avg. 33.36 years) #
summary(dataS$Age)

# 3. Education #
table(dataS$Education)
# Make dummy for education above 12th grade:
dataS$EducationHigh <- as.numeric(dataS$Education > 13)
table(dataS$EducationHigh)

# 4. State (5 = Delhi, 9 = Uttar Pradesh, 24 = Gujarat, 27 = Maharashtra) # 
table(dataS$State)
dataS$State <- as.factor(dataS$State)

# 5. Group Consciousness #
table(dataS$Group_Common_1)
table(dataS$Group_Common_2)
table(dataS$Group_Common_5)
table(dataS$Group_Common_6)
table(dataS$Group_Common_7)
table(dataS$Group_Common_8)

# Make index for group consciousness based on the variables: (1) perceived group commonality, (2) perceived discrimination and (3) willingness to act on group's behalf:

# Group commonality with Muslims (5 = a lot harder to get along with Muslims)
table(dataS$Group_Common_4)
# Recode so that 5 = a lot easier 
dataS$Group_Common_4R <- 6 - dataS$Group_Common_4
# Make variable range from 0-1
dataS$Group_Common_4RN <- (dataS$Group_Common_4R - min(dataS$Group_Common_4R))/(max(dataS$Group_Common_4R)-min(dataS$Group_Common_4R))

# Group commonality with Hindus (5 = a lot harder to get along with Hindus)
table(dataS$Group_Common_3)
# Recode so that 5 = a lot easier 
dataS$Group_Common_3R <- 6 - dataS$Group_Common_3
# Make variable range from 0-1
dataS$Group_Common_3RN <- (dataS$Group_Common_3R - min(dataS$Group_Common_3R))/(max(dataS$Group_Common_3R)-min(dataS$Group_Common_3R))

# Create a variable for closeness to own group 
dataS <- dataS %>% mutate(
  Closeness = NA,
  Closeness = ifelse(Religion==2,Group_Common_4RN,Closeness),
  Closeness = ifelse(Religion==1,Group_Common_3RN,Closeness))

table(dataS$Closeness)

# Perceived discrimination of Muslims (4 = a lot)
table(dataS$Perc_Disc_2)
# Make variable range from 0-1
dataS$Perc_Disc_2N <- (dataS$Perc_Disc_2 - min(dataS$Perc_Disc_2))/(max(dataS$Perc_Disc_2)-min(dataS$Perc_Disc_2))

# Perceived discrimination of Hindus (4 = a lot)
table(dataS$Perc_Disc_1)
# Make variable range from 0-1
dataS$Perc_Disc_1N <- (dataS$Perc_Disc_1 - min(dataS$Perc_Disc_1))/(max(dataS$Perc_Disc_1)-min(dataS$Perc_Disc_1))

# Create a variable for perceived discrimination of own group
dataS <- dataS %>% mutate(
  PercDiscr = NA,
  PercDiscr  = ifelse(Religion==2,Perc_Disc_2N,PercDiscr),
  PercDiscr  = ifelse(Religion==1,Perc_Disc_1N,PercDiscr))

table(dataS$PercDiscr)

# Willingness to act for own group (5 = strongly agree)
table(dataS$Willing_Act)
# Make variable range from 0-1
dataS$Willing_ActN <- (dataS$Willing_Act - min(dataS$Willing_Act))/(max(dataS$Willing_Act)-min(dataS$Willing_Act))

### Create index for group consciousness: ###
dataS$GroupCon <- idx_invcov(dataS$Closeness, dataS$PercDiscr, dataS$Willing_ActN)
hist(dataS$GroupCon)

# 6. Survey round (1 = first data collection, 2 = later collection) # 
table(dataS$Round)

######################################
# Other variables used in Appendix:  #
###################################### 

### 1. Voted BJP ###

# Make variable indicating whether or not respondent voted for the BJP
dataS$BJPvote <- as.numeric(dataS$Party_Which==2)
table(dataS$BJPvote)

# 2. Urban/Rural #
table(dataS$UrbanRural)

# Make dummy indicating big city
dataS <- dataS %>% mutate(
  city = NA,
  city = ifelse(UrbanRural==1,0,city),
  city = ifelse(UrbanRural==2,0,city),
  city = ifelse(UrbanRural==3,0,city),
  city = ifelse(UrbanRural==4,1,city),
  city = ifelse(UrbanRural==5,1,city))

table(dataS$city)

# 3. Caste identity

# Rename variable 
table(dataS$Caste.Category)
dataS <- rename(dataS, CasteCategory = Caste.Category)

# Dummy variable taking on value of 1 if respondent is from a lower caste
dataS <- dataS %>% mutate(
  Caste = NA,
  Caste = ifelse(CasteCategory==1,1,Caste),
  Caste = ifelse(CasteCategory==2,1,Caste),
  Caste = ifelse(CasteCategory==3,1,Caste),
  Caste = ifelse(CasteCategory==4,0,Caste),
  Caste = ifelse(CasteCategory==5,0,Caste),
  Caste = ifelse(CasteCategory==6,0,Caste),
  Caste = ifelse(CasteCategory==7,0,Caste))

table(dataS$Caste)

#4. School Type

# Rename variable 
table(dataS$School.Type)
dataS <- rename(dataS, SchoolType = School.Type)


###################################
# Save cleaned Dataset as Rdata   #
###################################

# Save data as Rdata and csv: 
# save(dataS, file="Data/DataForAnalysis.Rdata") # original script i have to adapt for my own
# write.csv(dataS, file="Data/DataForAnalysis.csv") # original script i have to adapt for my own

save(dataS, file = here("Class 2", "project", "DataForAnalysis.Rdata"))
write.csv(dataS, file = here("Class 2", "project", "DataForAnalysis.csv"))


