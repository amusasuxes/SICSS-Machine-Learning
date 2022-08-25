
#load relevant libraries
library(tidyverse)
library(readxl)
library(stargazer)
library(compareGroups)
library(mosaic)
library(themis)
library(caret)
library(naniar)
library(tidyverse)
library(xgboost)
library(openxlsx)
library(xtable)
library(tableHTML)
library(RColorBrewer)
library(rgeoboundaries)
library(pROC)

#import Nigeria dataset
wvs_group_work <- read_xlsx("C:/Users/HP/Desktop/ML Group Project/Data Set/F00010957-WVS_Wave_7_Nigeria_Excel_v2.0.xlsx")
View(wvs_group_work)

# Data Cleaning and Wrangling
# rename selected variables
# recode selected variables
wvs_group_renamed <-wvs_group_work %>%
  #renaming
  dplyr::rename(Sex = "Q260: Sex",
                Age = "Q262: Age",
                Mar_stat = "Q273: Marital status",
                Edu_level = "Q275: Highest educational level: Respondent [ISCED 2011]",
                Empl_status = "Q279: Employment status",
                Religiosity = "Q173: Religious person",
                Wife_beating = "Q189: Justifiable: For a man to beat his wife",
                Freedom = "Q149: Freedom and Equality - Which more important",
                Parity = "Q274: How many children do you have",
                Mother_work = "Q28: Pre-school child suffers with working mother",
                Men_pol_leader = "Q29: Men make better political leaders than women do",
                Uni_gender = "Q30: University is more important for a boy than for a girl",
                Men_business = "Q31: Men make better business executives than women do",
                HWife = "Q32: Being a housewife just as fulfilling",
                Men_job = "Q33: Jobs scarce: Men should have more right to a job than women",
                Women_income = "Q35: Problem if women have more income than husband",
                Pol_Interest = "Q199: Interest in politics",
                Relig_Import = "Q6: Important in life: Religion",
                Fin_Satis = "Q50: Satisfaction with financial situation of household",
                Ppl_Trust = "Q57: Most people can be trusted",
                Inc_Equal = "Q106: Income equality vs larger income differences",
                H_Urb= "H_URBRURAL: Urban-Rural",
                Women_LCorrupt = "Q119: Degree of agreement: On the whole, women are less corrupt than men",
                ISO = "N_REGION_ISO: Region ISO 3166-2",
                Ath_Part ="Q95: Active/Inactive membership: sport or recreational org",
                Male_Pgrp = "Q103: Active/Inactive membership: self-help group, mutual aid group",
                Sexual_MContent = "Q67: Confidence: Television",
                Mother_edu = "Q277: Highest educational level: Respondent´s Mother [ISCED 2011]",
                Father_edu = "Q278: Highest educational level: Respondent´s Father [ISCED 2011]",
                IPV_mothers = "Q138: Frequency in your neighborhood: Sexual harassment",
                Interv_progs = "Q101: Active/Inactive membership: charitable/humanitarian organization",
                Relig_Attitude = "Q94: Active/Inactive membership: church or religious org",
                Homo_Just = "Q182: Justifiable: Homosexuality",
                Prost_Just = "Q183: Justifiable: Prostitution",
                Abortion_Just = "Q184: Justifiable: Abortion",
                Divorce_Just = "Q185: Justifiable: Divorce",
                Premarsex_Just = "Q186: Justifiable: Sex before marriage",
                Casual_sex_Just = "Q193: Justifiable: Having casual sex",
                Child_manners = "Q7: Important child qualities: good manners",
                Child_tolerance = "Q12: Important child qualities: tolerance and respect for other people",
                Child_selfless = "Q16: Important child qualities: unselfishness",
                Econ_compete = "Q109: Competition good or harmful",
                Child_beating = "Q190: Justifiable: Parents beating children",
                Violence_Just = "Q191: Justifiable: Violence against other people") %>%
  select(Age,Sex,Mar_stat,Edu_level,Empl_status,Religiosity,Wife_beating,Freedom,Parity,Mother_work,
         Men_pol_leader,Uni_gender,Men_business,HWife,Men_job,Women_income,Pol_Interest,Relig_Import, 
         Fin_Satis, Ppl_Trust, Inc_Equal, H_Urb, Women_LCorrupt, ISO, Ath_Part, Male_Pgrp, Sexual_MContent,
         Mother_edu, Father_edu, IPV_mothers, Interv_progs, Relig_Attitude, Homo_Just, Prost_Just,
         Abortion_Just, Divorce_Just, Premarsex_Just, Casual_sex_Just, Child_manners, Child_tolerance,
         Child_selfless, Econ_compete, Child_beating, Violence_Just) %>%
  mutate(Age = derivedFactor( "18-34" = (Age >=18 & Age <= 34), 
                              "35-54" =(Age >= 35 & Age <=54), 
                              "55+" = (Age >=55),  .default=NA),
         Mar_stat = derivedFactor("Married" = (Mar_stat == 1 | Mar_stat == 2),
                                  "Divorced" = (Mar_stat == 3| Mar_stat == 4),
                                  "Never married" = (Mar_stat == 5|Mar_stat == 6), .default=NA),
         Edu_level = derivedFactor("No education" = (Edu_level==0),
                                   "primary"= (Edu_level==1), 
                                   "secondary"=(Edu_level==2| Edu_level==3| Edu_level==4 | Edu_level==5), 
                                   "tertiary"= (Edu_level== 6 | Edu_level==7 | Edu_level==8), .default=NA),
         Empl_status = ifelse((Empl_status<=3), "Employed", "Unemployed")%>% as.factor(),
         Religiosity = derivedFactor("Religious" =(Religiosity == 1),
                                     "Not Religious" = (Religiosity == 2),
                                     "Atheist" = (Religiosity == 3), .default=NA),
         Freedom = ifelse((Freedom ==1), "Freedom", "Equality")%>% as.factor(),
         Parity = ifelse((Parity>=1), "Yes", "No")%>% as.factor(),
         Pol_Interest = ifelse((Pol_Interest <= 2), "Yes", "No")%>% as.factor(), 
         Relig_Import = ifelse((Relig_Import <= 2), "Yes", "No")%>% as.factor(),
         Fin_Satis = ifelse((Fin_Satis >= 6), "Satisfied", "Not Satisfied")%>% as.factor(),
         Ppl_Trust = ifelse((Ppl_Trust == 1), "Yes", "No")%>% as.factor(),
         Inc_Equal = ifelse((Inc_Equal <= 5), "Yes", "No")%>% as.factor(),
         H_Urb = ifelse ((H_Urb==1), "urban", "rural") %>% as.factor(),
         Rev_Women_LCorrupt = (5- Women_LCorrupt),
         Sex = ifelse ((Sex==1), "Male", "Female") %>% as.factor(),
         Ath_Part = ifelse((Ath_Part >= 1), "Belong", "Don't belong") %>% as.factor(),
         Male_Pgrp = ifelse((Male_Pgrp >= 1), "Belong", "Don't belong") %>% as.factor(),
         Sexual_MContent = ifelse((Sexual_MContent <= 2), "Yes", "No") %>% as.factor(),
         Mother_edu = derivedFactor("No education" = (Mother_edu==0),
                                    "primary"= (Mother_edu==1), 
                                    "secondary"=(Mother_edu==2| Mother_edu==3| Mother_edu==4 | Mother_edu==5), 
                                    "tertiary"= (Mother_edu== 6 | Mother_edu==7 | Mother_edu==8), .default=NA),
         Father_edu = derivedFactor("No education" = (Father_edu==0),
                                    "primary"= (Father_edu==1), 
                                    "secondary"=(Father_edu==2| Father_edu==3| Father_edu==4 | Father_edu==5), 
                                    "tertiary"= (Father_edu== 6 | Father_edu==7 | Father_edu==8), .default=NA),
         IPV_mothers = ifelse((IPV_mothers <=2), "Yes", "No") %>% as.factor(),
         Interv_progs = ifelse((Interv_progs >= 1), "Belong", "Don't belong") %>% as.factor(),
         Relig_Attitude = ifelse((Relig_Attitude >= 1), "Belong", "Don't belong") %>% as.factor(),
         Homo_Just = ifelse((Homo_Just >5), "Yes", "No") %>% as.factor(),
         Prost_Just = ifelse((Prost_Just >5), "Yes", "No") %>% as.factor(),
         Abortion_Just = ifelse((Abortion_Just >5), "Yes", "No") %>% as.factor(),
         Divorce_Just = ifelse((Divorce_Just >5), "Yes", "No") %>% as.factor(),
         Premarsex_Just = ifelse((Premarsex_Just >5), "Yes", "No") %>% as.factor(),
         Casual_sex_Just = ifelse((Casual_sex_Just >5), "Yes", "No") %>% as.factor())

# cleaning the GEA variables
wvs_group_renamed <- wvs_group_renamed %>%
  replace_with_na((replace=list(Mother_work=c(-5, -2, -1),
                                Uni_gender=c(-5, -2, -1),
                                Men_pol_leader=c(-5, -1),
                                Men_business=c(-1),
                                HWife=c(-5, -1),
                                Men_job=c(-2, -1),
                                Women_income=c(-5, -2, -1),
                                Women_LCorrupt=c(-5, -1)
  )
  ))
# derive GEA score
wvs_group_renamed <- wvs_group_renamed %>%
  mutate(GEA_Score = rowSums(across(c(Mother_work, Men_pol_leader, Uni_gender,
                                      Men_business, HWife, Men_job, Women_income,
                                      Women_LCorrupt)))
  )

# extract GEA variables for inspection
gea_vars <- wvs_group_renamed %>%
  dplyr::select(Mother_work, Men_pol_leader, Uni_gender,
                Men_business, HWife, Men_job, Women_income,
                Women_LCorrupt, GEA_Score)


# get range and median of GEA score : use 21 as cut off
gea_range <- c(8:34)
summary(gea_range)

## dichotomize GEA_score or recode to two categories
wvs_group_renamed <- wvs_group_renamed %>%
  mutate(g_equitable=ifelse(GEA_Score>21, "equitable", "not_equitable") %>% as.factor())

# frequency tables
# selecting predictors
predictors <- wvs_group_renamed %>%
  dplyr::select(Age, Sex, Mar_stat, Edu_level, Empl_status, Religiosity, Freedom, Parity,
                Pol_Interest, Relig_Import, Fin_Satis, Ppl_Trust, Inc_Equal, H_Urb, Ath_Part,
                Male_Pgrp, Sexual_MContent, Mother_edu, Father_edu, IPV_mothers, Interv_progs,
                Relig_Attitude, Homo_Just, Prost_Just, Abortion_Just, Divorce_Just,Premarsex_Just,
                Casual_sex_Just, g_equitable)

#Drop missing values
predictors_dropna <- drop_na(predictors)

## Frequency table/ Summary Statistics
sumStat <- descrTable(predictors)
sumStat

#Plotting Barchart of g_equitable
ggplot(predictors_dropna, aes(fct_infreq(g_equitable))) + geom_bar() + coord_flip() + theme_classic()

# cross tabulation of g_equitable against categorical variables
crosstab <- compareGroups(g_equitable ~., data=predictors, byrow=TRUE)
createTable(crosstab)

#Select significant Explanatory Variables and the Outcome variable (g_equitable)
sig_predictors <- predictors%>% dplyr::select(Sex, Mar_stat,Edu_level,Freedom,
                                              Pol_Interest, Ppl_Trust, Inc_Equal, Sexual_MContent, Mother_edu,
                                              Father_edu, Relig_Attitude, Abortion_Just, Premarsex_Just, g_equitable)

#Set seed
set.seed(1234)

#Drop missing values
sig_predictors_dropna <- drop_na(sig_predictors)

#shuffle to randomise dataset; shuffle is found in mosaic library
shuff_predictors <- shuffle(sig_predictors_dropna)

#Remove orig.id from data frame
shuff_predictors <- shuff_predictors %>% dplyr::select(-c(orig.id))

#partitioning data and creating test and train data
data_index <- createDataPartition(shuff_predictors$g_equitable, p=0.7, list = FALSE)
train_data <- shuff_predictors[data_index,]
test_data <- shuff_predictors[-data_index,]

#Training the model
fitControl <- trainControl(method= "repeatedcv",
                           number = 10,
                           repeats = 5,
                           sampling="smote",
                           search = "random",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
#randomforest
set.seed(1234)
model_rf <- train(g_equitable~.,
                  data=train_data,
                  method="rf", tuneLength=10,
                  trControl= fitControl, metric= "ROC")
pred_rf <- predict(model_rf, test_data)
cm_rf <- confusionMatrix(pred_rf, test_data$g_equitable, positive = "equitable")
cm_rf

#svm
set.seed(1234)
model_svm <-train(g_equitable~.,
                  train_data,
                  method="svmRadial",
                  trControl= fitControl,tuneLength=10,metric= "ROC")
pred_svm <- predict(model_svm, test_data)
cm_svm <- confusionMatrix(pred_svm, test_data$g_equitable, positive = "equitable")
cm_svm

#Gradient Boost
set.seed(1234)
model_gb <- train(g_equitable~.,
                  train_data,
                  method="xgbTree",
                  metric= "ROC",tuneLength=10,
                  trControl= fitControl,verbosity=0)
pred_gb <- predict(model_gb, test_data)
cm_gb <- confusionMatrix(pred_gb, test_data$g_equitable, positive = "equitable")
cm_gb

#Decision Tree
set.seed(1234)
model_dt <- train(g_equitable~.,
                  train_data,
                  method="rpart",
                  metric= "ROC",tuneLength=10,
                  trControl= fitControl)
pred_dt <- predict(model_dt, test_data)
cm_dt <- confusionMatrix(pred_dt, test_data$g_equitable, positive = "equitable")
cm_dt

plot(model_dt$finalModel)
text(model_dt$finalModel)

### Logistic Reg
set.seed(1234)
model_logis <- train(g_equitable~.,
                     train_data,
                     method="glm",metric="ROC",
                     trControl= fitControl)
pred_logis <- predict(model_logis, test_data)
cm_logis <- confusionMatrix(pred_logis, test_data$g_equitable, positive = "equitable")
cm_logis

## Final model comparison
result <- rbind("LR" = cm_logis$byClass, 
                "DT" = cm_dt$byClass,
                "RF" = cm_rf$byClass, 
                "SVM" = cm_svm$byClass,
                "GB" = cm_gb$byClass) %>%
  ## t() is used to transpose the data
  t() %>% data.frame() %>%  
  rownames_to_column ("Metric") %>% 
  ## Let's drop a few columns that are not so important.
  filter (Metric != "Prevalence" &
            Metric != "Detection Rate" &
            Metric != "Detection Prevalence")
result

###ROC Curves
gbm.probs=predict(model_gb, test_data,type="prob")
rf.probs=predict(model_rf, test_data,type="prob")
svm.probs=predict(model_svm, test_data,type="prob")
dt.probs=predict(model_dt, test_data,type="prob")
lr.probs=predict(model_logis, test_data,type="prob")

gbm.ROC <- roc(predictor=gbm.probs$equitable,
               response=test_data$g_equitable,
               levels=rev(levels(test_data$g_equitable)))
rf.ROC <- roc(predictor=rf.probs$equitable,
              response=test_data$g_equitable,
              levels=rev(levels(test_data$g_equitable)))
svm.ROC <- roc(predictor=svm.probs$equitable,
               response=test_data$g_equitable,
               levels=rev(levels(test_data$g_equitable)))
dt.ROC <- roc(predictor=dt.probs$equitable,
              response=test_data$g_equitable,
              levels=rev(levels(test_data$g_equitable)))
lr.ROC <- roc(predictor=lr.probs$equitable,
              response=test_data$g_equitable,
              levels=rev(levels(test_data$g_equitable)))
gbm.ROC$auc; rf.ROC$auc; svm.ROC$auc; dt.ROC$auc; lr.ROC$auc
#Area under the curve: 
plot(gbm.ROC,col="blue",legacy.axes=T)
plot(rf.ROC,col="red",add=T)
plot(svm.ROC,col="green",add=T)
plot(dt.ROC,col="black",add=T)
plot(lr.ROC,col="yellow",add=T)

legend("bottomright", legend=c("GBM", "RF","SVM","DT","LR"),
       col=c("blue","red","green","black","yellow"), lwd=2)

