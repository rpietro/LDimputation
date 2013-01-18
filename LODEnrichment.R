#######################################################################################
#LODEnrichment.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################

###Link to the paper

#######################################################################################
#SETTING ENVIRONMENT
#######################################################################################
#command below will install all packages and is only run once. remove the #if this is the first time you are running the code on RStudio, and then you can add the hash tag again
##Loading packages
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors"), library, character.only=T)
library(plyr)
library(epicalc)
library(psych)
library(sqldf)
library(Hmisc)

#####################################################################################
#IMPORTING DATA
#####################################################################################
#Importing data from .CSV files
nhanes <- read.csv("~/Google Drive/RoR Duke/Applied Projects/LDimputation/joao/nhanes.csv")
nhanes$nhanes_id<-c(1:length(nhanes$gender))
viewData(nhanes)
names(nhanes)
head(nhanes)
str(nhanes)
summary(nhanes)

seer <- read.csv("~/Google Drive/RoR Duke/Applied Projects/LDimputation/joao/seer.csv")
seer$seer_id<-c(1:length(seer$gender))
viewData(seer)
names(seer)
head(seer)
str(seer)
summary(seer)

#####################################################################################
#Define and recode mtching variables
#####################################################################################
##Function that will show if the matching variables have the same code (name)
#identical(names(seer),names(nhanes))

#####################################################################################
#Matching datasets
#####################################################################################
##MERGE datasets - Use MERGE Functions, SQLDF or SQLLITE. HAVE NO IDEA QHAT WILL HAPPEN HERE
#matcheddataset<-merge(seer[1:300,], nhanes[1:300,], 
#                      by=c("age_group","gender","marital_status", "race"), incomparables=NA)
#str(matcheddataset2) #Show data characteristics
#head(matcheddataset) #Show first 6 observations in the dataset
#viewData(matcheddataset) #Show data set in spreadsheet form
#attach(matcheddataset) #Load dataset

#Merging dataset with SQLDF function
matcheddataset2  <- sqldf("SELECT * FROM seer JOIN nhanes USING(age_group,gender,
marital_status,race)")
str(matcheddataset2)
head(matcheddataset2)
viewData(matcheddataset2)
attach(matcheddataset2)

##ELIAS< Eu tentei essa outra função também, mas não está rodando, será que voce consegue ver
# qual o erro aqui?
#matcheddatasetline <- "select
#                seer.*
#              , nhanes.hyper
#              from seer
#                left join nhanes
#                on seer.age_group = nhanes.age_group
#                on seer.gender = nhanes.gender
#                on seer.marital_status = nhanes.marital_status
#                on seer.race = nhanes.race"
#matcheddataset2 <- sqldf(matcheddatasetline,stringsAsFactors = FALSE)
#####################################################################################
#Create Enrichment variable at the targeted dataset
#####################################################################################
##GET MATCHEDE GROUPS AND IMPUTE MEAN for numerica, MEdian for oridinal (more than 2) and PLYR for dichotomoous
#Plyr - http://goo.gl/LM0b
#ddply(dd, c("dim1","dim2"), function(df)c(mean(df$v1),mean(df$v2),mean(df$v3),sd(df$v1),sd(df$v2),sd(df$v3)))
#http://goo.gl/Qi1Qb - Allign datasets after machting through sql

#enrichHyper<-sqldf("select seer_id, Mode(hyper_x) from matcheddataset group by seer_id")
#str(enrichHyper)
#head(enrichHyper)
#names(enrichHyper)
#quantile(enrichHyper$'Mode(hyper_x)', na.rm=TRUE, probs=seq(0,1,0.333))

#Creating "Enriched data vector" through sqldf function, here using Mode statistics 
enrichHyper1<-sqldf("select seer_id, Mode(hyper) from matcheddataset2 group by seer_id")
str(enrichHyper)
head(enrichHyper)
names(enrichHyper)
#quantile(enrichHyper$'Mode(hyper_x)', na.rm=TRUE, probs=seq(0,1,0.333))

#####################################################################################
#Plot enrichment variable against original variable
#####################################################################################
##PLOT IMPUTED Vs. ORIGINAL TO SE WHETER IT WORKED OR NOT
#tryout<-data.frame(enrichHyper$'Mode(hyper_x)',seer)

## Try 1 - Run Agreement Plots and Tests
#obj3<-ckappa(Inter3)
#Kappa.test(x, y=NULL, conf.level=0.95) #fmsb package
#kappa2(ratings, weight = c("unweighted", "equal", "squared"), sort.levels = FALSE)

#data(DCLHb);
#ua <- unified.agreement(dataset=DCLHb, var=c("HEMOCUE1","HEMOCUE2","SIGMA1","SIGMA2"), k=2, m=2, CCC_a_intra=0.9943, CCC_a_inter=0.9775, CCC_a_total=0.9775, CP_a=0.9, tran=1, TDI_a_intra=75, TDI_a_inter=150, TDI_a_total=150, error="const", dec=1, alpha=0.05);
#summary(ua);

#data(DCLHb);
#HemocueAve <- apply(DCLHb[,c("HEMOCUE1","HEMOCUE2")],1,mean);
#SigmaAve <- apply(DCLHb[,c("SIGMA1","SIGMA2")],1,mean);
#agr=agreement(y=HemocueAve,x=SigmaAve,V_label="Hemacue",H_label="Sigma",min=0,max=2000,by=250,CCC_a=0.9775,CP_a=0.9,TDI_a=150,error="const",target="random",dec=3,alpha=0.05);
#summary(agr);
#plot(agr);

#mosaic(Year ~ Responses | Time, data = datamosaic,highlighting_direction = "right")

#agreementplot(t(obj1$table), main = "Agreement between Before and After AO Training
#              ",xlab_rot=90, ylab_rot=0, 
#              xlab_just="right", ylab_just="right")
### Try 2 - Analyse data distribution

#dotchart(diff$x...y, groups=d,pch=25,ylim=c(-6,6),xlab="Difference AO (Before and After)",
#         color=diff$colour) #calling plot

#Frequencies for both data per matchign data group

## Try 3 - Prediction models with hyper as predictor and outcome
#Logistic regression with all the matching data as predictors
#qvcXsocio31<-glm(test$qvc50cat ~ diagn+test$idadeinicio+duracaocrises
#                 ,family=binomial, data=test)
#summary(qvcXsocio3)
#anova(qvcXsocio3,test="Chisq")
#lrtest(qvcXsocio3,qvcXsocio31)
#summary(qvcXsocio3) # display results
#coef(qvcXsocio31)
#confint(qvcXsocio31) # 95% CI for the coefficients
#exp(coef(qvcXsocio31)) # exponentiated coefficients
#exp(confint(qvcXsocio3)) # 95% CI for exponentiated coefficients
#predict(qvcXsocio3, type="response") # predicted values
#residuals(qvcXsocio3, type="deviance") # residuals

