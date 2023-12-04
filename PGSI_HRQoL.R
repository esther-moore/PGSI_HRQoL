# #Install all packages that are used. 
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("naniar")
# #install.packages("devtools")
# devtools::install_github("AlineTalhouk/Amisc")
# #install.packages("tableone")
# #install.packages("MatchIt")
#install.packages("MASS")
#install.packages("stargazer")

#Load all packaged used
library(haven)
library(tidyverse)
library(naniar)
library(Amisc)
library(tableone)
library(MatchIt)
library(MASS)
library(stargazer)

#download the HSE 2018 from UK Data Service: NatCen Social Research, University College London, Department of Epidemiology and Public Health. (2022). Health Survey for England, 2018. [data collection]. 2nd Edition. UK Data Service. SN: 8649, DOI: http://doi.org/10.5255/UKDA-SN-8649-2
setwd() #set to where the data is saved on your device
getwd()

HSE_2018 <-read_dta("hse_2018_eul_22052020.dta")


#re-coding missing values etc to NA
missing<- c(-1, -8, -9, -90)

HSE_2018<-HSE_2018 %>%
  replace_with_na(replace = list(PGSIsc = missing,
                                 Mobil17=missing, SelfCa17=missing, UsualA17=missing,Pain17=missing, Anxiet17=missing,origin2=missing, Sex=missing,
                                 topqual3=missing, nssec8=missing, dnoft3=missing, cigsta3=missing, Age35g=missing,
                                 eqv5=missing, IllAff9=missing, ag16g10=missing, marstatD=missing))


############################Calculating the EQ-5D score#########################
#the calculation of the EQ5D score is based on the health profile (the answers to the 5 domains are in the HSE but not the score)
#The HSE contains the EQ-5D-5L but there is only a value set for England using the EQ-5D-3L
#The DSU Excel macro to do the whole dataset at once: https://www.sheffield.ac.uk/nice-dsu/methods-development/mapping-eq-5d-5l-3l 
#The following age categories are needed for this function, 1=<35, 2=35-45, 3=45-55, 4=55-65, 5=65+

HSE_2018<-HSE_2018 %>%
  mutate(age = ifelse(ag16g10==1|ag16g10==2, "1",
                      ifelse(ag16g10==3, "2",
                             ifelse(ag16g10==4, "3",
                                    ifelse(ag16g10==5, "4",
                                           ifelse(ag16g10==6|ag16g10==7, "5",
                                                  NA))))))

#excel macro requires male to be 1 and female to be 0.
HSE_2018<- HSE_2018%>%
  mutate(Sex=ifelse(Sex==1, "1",
                    ifelse(Sex==2, "0",
                           NA)))


#Need complete data for the 5 domains of the EQ-5D and for age and sex
HSE_2018 <- HSE_2018 %>% drop_na(Sex, age, Mobil17, SelfCa17, UsualA17, Pain17, Anxiet17)


#selecting the variable which are needed for the EQ-5D calculation
#mapping_EQ_5D<- mapping_EQ_5D %>%
# dplyr::select(Seriala, age, Sex, Mobil17, SelfCa17, UsualA17, Pain17, Anxiet17)

#write.csv(mapping_EQ_5D, "mapping_EQ_5D.csv", row.names=F) #this csv file is written out and used with the DSU excel macro to get the EQ-5D scores

mapped_scores <-read.csv("HSE_with_EQ5Dsc.csv") #read in the dataset which has been produced using the DSU excel macro

HSE_2018 <-merge(x = HSE_2018, y = mapped_scores, by = "Seriala", all.x = TRUE) #Merge the HSE and the new data set with scores together using the individual identifier Seriala


#creating new variable for "reference" PGSIsc=0 and "affected" PGSIsc=+1
HSE_2018<-HSE_2018 %>%
  mutate(affected = ifelse(PGSIsc==0, 0,
                           ifelse(PGSIsc > 0, 1, NA)))

#creating new variable for each category, recreational, low, moderate and high risk
HSE_2018<-HSE_2018 %>%
  mutate(PGSI_category = ifelse(PGSIsc==0, "NG/NPG",
                                ifelse(PGSIsc>0 & PGSIsc<=2, "low",
                                       ifelse(PGSIsc>=3 & PGSIsc <=7, "moderate",
                                              ifelse(PGSIsc>7, "high",NA)))))

#creating new variable to represent the harms aspects of the PGSI questions
#items 2 and 3 removed as per the Canadian Guidelines paper

HSE_2018 <- HSE_2018 %>%
  mutate(PGSI_harms = (PGSI1 + PGSI4 + PGSI5 + PGSI6 + PGSI7 + PGSI8 +PGSI9))


#creating new variables for comorbidities to match those used by Browne et al. 

HSE_2018<-HSE_2018 %>%
  mutate(cig_per_day = ifelse(cigst2==1, "<10",
                              ifelse(cigst2==1 | cigst2==2 | cigst2==3, "+10",
                                     ifelse(cigst2==5, "non-smoker"
                                            ,NA))))
HSE_2018 <- HSE_2018 %>%
  mutate(mental_disorder =ifelse(mentald==3, "yes",
                                 ifelse(mentald==1 | mentald==2, "no",
                                        NA)))

HSE_2018<- HSE_2018 %>%
  mutate(disability_allowance= ifelse(AttDisb1==1 | AttDisb2==1 | AttDisb3==1 | AttDisb4==1 | AttDisb5==1, "yes",
                                      ifelse(AttDisb96==1, "no",
                                             NA)))

#age categories for descriptive statistics. Age categories in the HSE are in 5 year age bands so these are reduced to avoid very large tables
HSE_2018<-HSE_2018%>%
  mutate(age = ifelse(Age35g>6&Age35g<=9, "16-29",
                      ifelse(Age35g>9&Age35g<=12, "30-44",
                             ifelse(Age35g>12&Age35g<=15, "45-59",
                                    ifelse(Age35g>15&Age35g<=18, "60-74",
                                           ifelse(Age35g>17&Age35g<=22, "75+",
                                                  NA))))))
#removing those who do not have a value for the "affected" variable as this is needed for the propensity score matching
HSE_2018 <- HSE_2018 %>% drop_na(affected)

#keeping only the variables needed 

HSE_2018 <- HSE_2018 %>% dplyr::select(Age35g, age, Sex.x, origin2, nssec8, topqual3, urban14bR, eqv5,
                                                 mental_disorder, disability_allowance,
                                                 dnoft3, cig_per_day, PGSI_category, PGSI_harms, PGSIsc, EQ5Dsc, affected, marstatD, qimd, IllAff9, 
                                                 Mobil17.x, SelfCa17.x, UsualA17.x, Pain17.x, Anxiet17.x)

HSE_2018 <- rename(HSE_2018, Sex = Sex.x)

#These are the variables which are considered for inclusion in the propensity score matching model
#Complete data is needed for these variables to be able to match using them

EQ5D_complete <- HSE_2018 %>% drop_na(Age35g, Sex, origin2, nssec8, topqual3,urban14bR,eqv5, IllAff9, marstatD)


#making sure the variables are the correct type
numeric_variables <-c("EQ5Dsc", "PGSIsc", "PGSI_harms")
HSE_2018[numeric_variables] <- lapply(HSE_2018[numeric_variables],as.numeric)

factor_variables <- c("Age35g", "Sex", "origin2", "nssec8", "topqual3", "urban14bR", "eqv5", "mental_disorder", "disability_allowance", "dnoft3", "cig_per_day", "PGSI_category", "affected", "marstatD", "qimd", "IllAff9")
HSE_2018[factor_variables] <- lapply(HSE_2018[factor_variables],as.factor)

#releveling variables to get correct reference groups
HSE_2018 <- within(HSE_2018, Sex <- relevel(factor(Sex), ref = "0"))
HSE_2018 <- within(HSE_2018, Age35g <- relevel(factor(Age35g), ref = "7"))
HSE_2018 <- within(HSE_2018, origin2 <- relevel(factor(origin2), ref = "1"))
HSE_2018 <- within(HSE_2018, nssec8 <- relevel(factor(nssec8), ref = "1"))
HSE_2018 <- within(HSE_2018, topqual3 <- relevel(factor(topqual3), ref = "1"))
HSE_2018 <- within(HSE_2018, urban14bR <- relevel(factor(urban14bR), ref = "1"))
HSE_2018 <- within(HSE_2018, eqv5 <- relevel(factor(eqv5), ref = "1"))
HSE_2018 <- within(HSE_2018, dnoft3 <- relevel(factor(dnoft3), ref= "8"))
HSE_2018 <- within(HSE_2018, cig_per_day <- relevel(factor(cig_per_day), ref= "non-smoker"))

#########Table 1 for demographic description of sample before matching##########

Table1<-Amisc::describeBy(data = HSE_2018, var.names = c("Sex", "age", "origin2", "topqual3", "nssec8", "marstatD", "eqv5", "urban14bR", "IllAff9"), by1 = "affected",
                          dispersion = "sd", Missing = TRUE, stats = "non-parametric")

write.csv(Table1, "table1_update.csv", row.names=F)

###################STEP 1: specifying the propensity model######################

#selection of the variables using the stepAIC function
pscores.modelx <- glm(affected ~ Age35g +Sex +origin2 + nssec8 + topqual3+urban14bR + eqv5 + marstatD + IllAff9 ,family = binomial("logit"),data = EQ5D_complete)
summary(pscores.modelx)

step_pscore.modelx <- stepAIC(pscores.modelx, trace = TRUE, direction= "backward")

stargazer(pscores.modelx, step_pscore.modelx, type = "text")

#based on backwards selection, age, sex, topqual should be used to match participants.

#############STEP 2: running the propensity model using a logit model###########
missing_topqual <- sum(is.na(HSE_2018$topqual3))


vars<- c("Age35g", "Sex", "topqual3")
HSE_2018<- drop_na(HSE_2018, any_of(vars)) #need complete data for the variables which are used for matching

pscores.model <- (glm(affected ~ Age35g +Sex + topqual3 ,family = binomial("logit"),data = HSE_2018))
summary(pscores.model) 

m.out <- matchit(affected ~ Age35g + Sex + topqual3, data=HSE_2018, method = "nearest", ratio=1)
summary(m.out)


#plot(m.out, type= "jitter") #produces graph to assess matching- commented out as it stops the code from running past this point


EQ5D_matched <- match.data(m.out)

#write.csv(EQ5D_matched, file="matched_EQ5D.csv")

#########Table 1a for demographic description of sample after matching##########

Table1a<-Amisc::describeBy(data = EQ5D_matched, var.names = c("Sex", "age", "origin2", "topqual3", "nssec8", "marstatD", "eqv5", "urban14bR", "IllAff9"), by1 = "affected",
                          dispersion = "sd", Missing = TRUE, stats = "non-parametric")

write.csv(Table1a, "table1a_update.csv", row.names=F)


#####################Mean EQ-5D score for entire sample########################

mean_eq5d <- mean(EQ5D_matched$EQ5Dsc)

###############STEP 3: "causal" models using matched data sets##################

#1) PGSI score
#do OLS model on matched dataset with no covariates 

cor(EQ5D_matched$EQ5Dsc, EQ5D_matched$PGSIsc)


one_a <-lm(EQ5Dsc~PGSIsc, data = EQ5D_matched)

summary(one_a)

#OLS model with covariates 

one_b <- lm(EQ5Dsc ~ PGSIsc + mental_disorder + disability_allowance + dnoft3
                    + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)
summary(one_b)

#2) PGSI categories 
EQ5D_matched <- within(EQ5D_matched, PGSI_category <- relevel(factor(PGSI_category), ref = "NG/NPG"))

two_a <-lm(EQ5Dsc~PGSI_category, data = EQ5D_matched)

summary(two_a)

#OLS model with covariates 

two_b <- lm(EQ5Dsc ~ PGSI_category + mental_disorder + disability_allowance + dnoft3
                                  + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(two_b)



#3) PGSI harms

three_a<-lm(EQ5Dsc~PGSI_harms, data = EQ5D_matched)

summary(three_a)

#harms with covariates


three_b<- lm(EQ5Dsc ~ PGSI_harms + mental_disorder + disability_allowance + dnoft3
                              + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(three_b)


stargazer(one_a, one_b, two_a, two_b, three_a, three_b, type = "html",
          out="table4.html", align=TRUE,  title="Regression results for EQ-5D analysis_formatted",
          column.labels=c("PGSI score (no covariates)", "PGSI score", "PGSI categories (no covariates)", "PGSI categories", "PGSI harms (no covariates)", "PGSI harms"),
          single.row=TRUE,
          order=c("PGSIsc", "PGSI_categorylow", "PGSI_categorymoderate", "PGSI_categoryhigh", "PGSI_harms", "mental_disorder", "disability_allowance", "dnoft3", "cig_per_day", "Age35g", "Sex", "origin2"),
          covariate.labels=c("PGSI score","PGSILow Risk", "Moderate Risk", "High Risk", "7-item PGSI derived harm variable", "Long-term mental disorder (ref=no)", "Disability Allowance (ref=no)", "Almost every day",
                             "Five or six days a week", "Three or four days a week", "Once or twice a week", "Once or twice a month", "Once every couple of months",
                             "Once or Twice a Year",">=10 per day","<10 per day",  "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
                             "85+", "Sex (ref=female)", "Black", "Asian", "Mixed", "Other" ),
          star.cutoffs = c(0.05, 0.001),
          notes        = "*p<0.05", "**p<0.001",  
          notes.append = FALSE,
          digits = 3,
          model.numbers= TRUE)


# generate a summary table with comorbidities


EQ5D_matched$dnoft3 <- factor(EQ5D_matched$dnoft3)
Table_3 <- Amisc::describeBy(
  data = EQ5D_matched,
  var.names = c("dnoft3", "mental_disorder", "cig_per_day", "disability_allowance"),
  by1 = "PGSI_category",
  dispersion = "sd",
  Missing = TRUE,
  stats = "non-parametric",
)

write.csv(Table_3, file = "table3.csv", row.names = FALSE)

#mean EQ-5D score across the PGSI categories to add to Table 3

EQ5D_summary_table <- Amisc::describeBy(
  data = EQ5D_matched,
  var.names = c("EQ5Dsc"),
  by1 = "PGSI_category",
  dispersion = "sd",
  Missing = TRUE,
  stats = "parametric",
  digits = 3,
)


########################Analysis of EQ-5D domain scores#########################

###PGSI score### 

#OLS model for each domain (5) with covariates 

#mobility
a4 <- lm(Mobil17.x ~ PGSIsc + mental_disorder + disability_allowance + dnoft3
                          + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(a4)

#Self-care
b4 <- lm(SelfCa17.x ~ PGSIsc + mental_disorder + disability_allowance + dnoft3
                          + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(b4)

#Usual activities 
c4 <- lm(UsualA17.x ~ PGSIsc + mental_disorder + disability_allowance + dnoft3
                       + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(c4)

#Pain
d4 <- lm(Pain17.x ~ PGSIsc + mental_disorder + disability_allowance + dnoft3
                      + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(d4)

#Anxiety/depression
e4 <- lm(Anxiet17.x ~ PGSIsc + mental_disorder + disability_allowance + dnoft3
                         + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(e4)

###PGSI categories###

#OLS model for each domain (5) with covariates 
EQ5D_matched <- within(EQ5D_matched, PGSI_category <- relevel(factor(PGSI_category), ref = "NG/NPG"))


#mobility
a5 <- lm(Mobil17.x ~ PGSI_category + mental_disorder + disability_allowance + dnoft3
                             + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(a5)

#Self-care
b5 <- lm(SelfCa17.x ~ PGSI_category + mental_disorder + disability_allowance + dnoft3
                             + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(b5)

#Usual activities 
c5 <- lm(UsualA17.x ~ PGSI_category + mental_disorder + disability_allowance + dnoft3
                          + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(c5)

#Pain
d5 <- lm(Pain17.x ~ PGSI_category + mental_disorder + disability_allowance + dnoft3
                         + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(d5)

#Anxiety/depression
e5 <- lm(Anxiet17.x ~ PGSI_category + mental_disorder + disability_allowance + dnoft3
                            + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(e5)


###PGSI-derived harm### 

#OLS model for each domain (5) with covariates 

#mobility
a6 <- lm(Mobil17.x ~ PGSI_harms + mental_disorder + disability_allowance + dnoft3
                         + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(a6)

#Self-care
b6 <- lm(SelfCa17.x ~ PGSI_harms + mental_disorder + disability_allowance + dnoft3
                         + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(b6)

#Usual activities 
c6 <- lm(UsualA17.x ~ PGSI_harms + mental_disorder + disability_allowance + dnoft3
                      + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(c6)

#Pain
d6 <- lm(Pain17.x ~ PGSI_harms + mental_disorder + disability_allowance + dnoft3
                     + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(d6)

#Anxiety/depression
e6 <- lm(Anxiet17.x ~ PGSI_harms + mental_disorder + disability_allowance + dnoft3
                        + Age35g + Sex + origin2 + cig_per_day, data=EQ5D_matched)

summary(e6)

stargazer::stargazer(a4, b4, c4, d4, e4,
                     type ="html",
                     out="domains_PGSI_score.html", align=TRUE,  title="Regression results for EQ-5D domain analysis-PGSIscore",
                     dep.var.labels=c("Mobility", "Self-care", "Usual Activities", "Pain", "Anxiety/Depression"),
                     single.row=TRUE,
                     order=c("PGSIsc", "mental_disorder", "disability_allowance", "dnoft3", "cig_per_day", "Age35g", "Sex", "origin2"),
                     covariate.labels=c("PGSI score", "Long-term mental disorder (ref=no)", "Disability Allowance (ref=no)", "Almost every day",
                                        "Five or six days a week", "Three or four days a week", "Once or twice a week", "Once or twice a month", "Once every couple of months",
                                        "Once or Twice a Year",">=10 per day", "<10 per day", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
                                        "85+", "Sex (ref=female)", "Black", "Asian", "Mixed", "Other"),
                     star.cutoffs = c(0.05, 0.001),
                     notes        = "*p<0.05", "**p<0.001", 
                     notes.append = FALSE,
                     digits = 3,
                     model.numbers= TRUE)

stargazer::stargazer(a5, b5, c5, d5, e5,
                     type ="html",
                     out="domains_PGSI_cat.html", align=TRUE,  title="Regression results for EQ-5D domain analysis-PGSI cat",
                     dep.var.labels=c("Mobility", "Self-care", "Usual Activities", "Pain", "Anxiety/Depression"),
                     single.row=TRUE,
                     order=c("PGSI_categorylow", "PGSI_categorymoderate", "PGSI_categoryhigh", "mental_disorder", "disability_allowance", "dnoft3", "cig_per_day", "Age35g", "Sex", "origin2"),
                     covariate.labels=c("Low Risk", "Moderate Risk", "High Risk", "Long-term mental disorder (ref=no)", "Disability Allowance (ref=no)", "Almost every day",
                                        "Five or six days a week", "Three or four days a week", "Once or twice a week", "Once or twice a month", "Once every couple of months",
                                        "Once or Twice a Year",">=10 per day", "<10 per day", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
                                        "85+", "Sex (ref=female)", "Black", "Asian", "Mixed", "Other"),
                     star.cutoffs = c(0.05, 0.001),
                     notes        = "*p<0.05", "**p<0.001", 
                     notes.append = FALSE,
                     digits = 3,
                     model.numbers= TRUE)

stargazer::stargazer(a6, b6, c6, d6, e6,
                     type ="html",
                     out="domains_PGSI_harm.html", align=TRUE,  title="Regression results for EQ-5D domain analysis- PGSI Harms",
                     dep.var.labels=c("Mobility", "Self-care", "Usual Activities", "Pain", "Anxiety/Depression"),
                     single.row=TRUE,
                     order=c("PGSI_harms", "mental_disorder", "disability_allowance", "dnoft3", "cig_per_day", "Age35g", "Sex", "origin2"),
                     covariate.labels=c("7-item PGSI derived harm varibale", "Long-term mental disorder (ref=no)", "Disability Allowance (ref=no)", "Almost every day",
                                        "Five or six days a week", "Three or four days a week", "Once or twice a week", "Once or twice a month", "Once every couple of months",
                                        "Once or Twice a Year",">=10 per day", "<10 per day", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
                                        "85+", "Sex (ref=female)", "Black", "Asian", "Mixed", "Other"),
                     star.cutoffs = c(0.05, 0.001),
                     notes        = "*p<0.05", "**p<0.001", 
                     notes.append = FALSE,
                     digits = 3,
                     model.numbers= TRUE)


#######Numbers in each category in models with covariates (dropping NAs)#######

test <- drop_na(EQ5D_matched, mental_disorder, disability_allowance, dnoft3
                ,Age35g, Sex, origin2, cig_per_day)


Table_3a <- Amisc::describeBy(
  data = test,
  var.names = c("dnoft3", "mental_disorder", "cig_per_day", "disability_allowance"),
  by1 = "PGSI_category",
  dispersion = "sd",
  Missing = TRUE,
  stats = "non-parametric",
)
