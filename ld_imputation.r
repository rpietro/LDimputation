#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()
library(plyr)
library(epicalc)
library(psych)

###########################
#import both data sets, only containing the actual variables that we are going to use
###########################

NIS_full <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/LD_imputation/NIS2004.csv")
str(NIS_full)
SEER_full <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/LD_imputation/SEER2004.csv")
str(SEER_full)
View(SEER_full)

#assigning unique identifiers to facilitate data visualization
NIS_full$nis_id <- c(1:97372)
SEER_full$seer_id <- c(1:249861)

###########################
#apply different inclusion/exclusion criteria into seer -- this will be one of the parameters we will be testing
###########################

# PAY1/PAY2 - "medicare"
#NIS has all GI tumors and so has to subset

#later will use select to pick specific variables
SEER2004  <- subset(SEER_full, SEER_full$yrdxL==2004)
NIS2004  <- NIS_full
#View(SEER2004)

#extracting a sample to make experiments faster
NIS <- NIS2004[sample(1:nrow(NIS2004), 100,replace=FALSE),]
SEER <- SEER2004[sample(1:nrow(SEER2004), 100,replace=FALSE),]
#View(NIS)

str(SEER)
str(NIS)


#########################
#matching variables will be recoded to a common name, later on in compliance with caDSR. in parallel with this section, we can try to do something using the tm package so that later on we can be prepared to do this using NLP
########################
names(SEER)
names(NIS)

#gender and geographic region are the only ones truly working

#recoding gender
NIS$gender <- NA
NIS$gender[NIS$FEMALE==1] <- "female"
NIS$gender[NIS$FEMALE==0] <- "male"  
NIS$gender  <- as.factor(NIS$gender)
summary(NIS$gender)

SEER$gender <- NA
SEER$gender[SEER$S_SEX==2]  <- "female" 
SEER$gender[SEER$S_SEX==1]  <- "male"
SEER$gender  <- as.factor(SEER$gender)
summary(SEER$gender)

#recoding geographic region
NIS$region <- NA
NIS$region[NIS$HOSP_REGION==1]  <-  "northeast"
NIS$region[NIS$HOSP_REGION==2]  <-  "midwest"
NIS$region[NIS$HOSP_REGION==3]  <-  "south"
NIS$region[NIS$HOSP_REGION==4]  <-  "west"
NIS$region  <- as.factor(NIS$region)
summary(NIS$region)

SEER$region <- round(SEER$geography)
SEER$region <- NA
SEER$region[SEER$geography=="Northeast"]  <-  "northeast"
SEER$region[SEER$geography=="Midwest"]  <-  "midwest"
SEER$region[SEER$geography=="South"]  <-  "south"
SEER$region[SEER$geography=="West"]  <-  "west"
SEER$region  <- as.factor(SEER$region)
summary(SEER$region)


# #recoding age - age for SEER is too low
# NIS$age_cat <- NA
# NIS$age_cat[NIS$AGE<=60]  <-  "<60"
# NIS$age_cat[NIS$AGE>60 & NIS$AGE<=70]  <-  "60 to 70"
# NIS$age_cat[NIS$AGE>70]  <- ">70"
# NIS$age_cat  <- as.factor(NIS$age_cat)
# summary(NIS$age_cat)
# 
# SEER$age_cat <- NA
# SEER$age_cat[SEER$age<=60]  <-  "<60"
# SEER$age_cat[SEER$age>60 & SEER$age<=70]  <-  "60 to 70"
# SEER$age_cat[SEER$age>70]  <- ">70"
# SEER$age_cat  <- as.factor(SEER$age_cat)
# summary(SEER$age_cat)
# 
# #recoding race
# NIS$RACE <- round(NIS$RACE)
# NIS$race <- NA
# NIS$race[NIS$RACE==1]  <-  "white"
# NIS$race[NIS$RACE==2]  <-  "black"
# NIS$race[NIS$RACE==3]  <-  "hispanic"
# NIS$race[NIS$RACE==4]  <-  "asian"
# NIS$race[NIS$RACE==5]  <-  "native"
# NIS$race[NIS$RACE==6]  <-  "other"
# NIS$race  <- as.factor(NIS$race)
# summary(NIS$race)
# 
# #below is missing the variable for hispanic
# #View(SEER)
# class(SEER$SRACE)
# levels(SEER$SRACE)
# SEER$SRACE <- round(SEER$SRACE)
# SEER$SRACE <- NA
# SEER$SRACE[SEER$SRACE==1]  <-  "white"
# SEER$SRACE[SEER$SRACE==2]  <-  "black"
# SEER$SRACE[SEER$SRACE==3]  <-  "native"
# SEER$SRACE[SEER$SRACE==4]  <-  "asian"
# SEER$SRACE[SEER$SRACE==5]  <-  "asian"
# SEER$SRACE[SEER$SRACE==6]  <-  "asian"
# SEER$SRACE[SEER$SRACE==7]  <-  "asian"
# SEER$SRACE[SEER$SRACE==8]  <-  "asian"
# SEER$SRACE[SEER$SRACE==9]  <-  "asian"
# SEER$SRACE[SEER$SRACE==10]  <-  "asian"
# SEER$SRACE[SEER$SRACE==11]  <-  "asian"
# SEER$SRACE[SEER$SRACE==12]  <-  "asian"
# SEER$SRACE[SEER$SRACE==13]  <-  "asian"
# SEER$SRACE[SEER$SRACE==14]  <-  "asian"
# SEER$SRACE[SEER$SRACE==20]  <-  "asian"
# SEER$SRACE[SEER$SRACE==21]  <-  "asian"
# SEER$SRACE[SEER$SRACE==22]  <-  "asian"
# SEER$SRACE[SEER$SRACE==25]  <-  "asian"
# SEER$SRACE[SEER$SRACE==26]  <-  "asian"
# SEER$SRACE[SEER$SRACE==27]  <-  "asian"
# SEER$SRACE[SEER$SRACE==28]  <-  "asian"
# SEER$SRACE[SEER$SRACE==30]  <-  "asian"
# SEER$SRACE[SEER$SRACE==31]  <-  "asian"
# SEER$SRACE[SEER$SRACE==32]  <-  "asian"
# SEER$SRACE[SEER$SRACE==96]  <-  "asian"
# SEER$SRACE[SEER$SRACE==97]  <-  "asian"
# SEER$SRACE[SEER$SRACE==98]  <-  "other"
# SEER$SRACE[SEER$SRACE==99]  <-  "other"
# 
# SEER$SRACE  <- as.factor(SEER$SRACE)
# summary(SEER$SRACE)


#merge both data sets using seer as the basis -- this will actually create duplicate entries for seer when there is more than one matching subject as well as some missing values when there is no match -- this is ok

#joining datasets
combined_full <- join(SEER, NIS, by = c("gender", "region"))
View(combined_full)
describe(combined_full)
#ordering columns and only picking the important ones
combined_unsort <- combined_full[c("seer_id","nis_id", "gender", "region")]
View(combined_unsort)
#shorting by seer_id
combined <- combined_unsort[order(combined_unsort$seer_id) , ]

#calculate rates of missing rates over the total sample (equivalent to absence of a match) as well as number of matches for each subject in NIS

#from here on we will primarily use ddply (plyr) and ggplot(ggplot2) to calculate different summary metrics or predictive methods to determine the ideal matching principle, namely (1) how closely the inclusion/exclusion criteria should be matched between databases, (2) how many variables we should have in terms of matching, and (3) what kinds of prediction models would generate the best results

#once the matching is working, we will then do the same analysis but now using RDF, simulating them under two different sparql endpoints


#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################
