setwd("/Users/Downloads/AY24 CBA")

library(data.table)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(car)
library(rpart)
library(rpart.plot)
library(caret)
library(MASS)
library(caTools)
library(broom)
library(glmnet)
library(randomForest)

data = fread("INF002v4.csv")

# copy of original data before modifications
data_ori = data

# total: 28109 records
summary(data)
str(data)

# check for duplicates: 0 (no duplicates exist)
sum(duplicated(data))

# ------------------------------------------------------------------

# q1: notable finding 1
# every variable has a data type of chr
# many variables are correctly recorded as categorical variables
# charges, costs are recorded as chr instead of float
str(data)

# despite there being na values, "" is recorded as chr
# sum of na values return 0
sum(is.na(data))

# however, there are columns where values are ""
# total: 31 (Hospital.Service.Area) + 1 (APR.Severity.of.Illness.Description) + 1 (APR.Risk.of.Mortality) + 11547 (Payment.Typology.2) + 23359 (Payment.Typology.3) = 34,939 
for (i in seq_along(data)) {
  column = names(data)[i]
  empty = sum(data[[i]] == "")
  cat(column, ":", empty, "\n")
}

# to find out which rows are "" (for APR.Severity.of.Illness.Description and APR.Risk.of.Mortality), as there is only 1 case, which is weird
print(data[data$APR.Severity.of.Illness.Description == "", ])
print(data[data$APR.Risk.of.Mortality == "", ])
print(data[data$APR.Severity.of.Illness.Description == "", ] == data[data$APR.Risk.of.Mortality == "", ])

# they are from the same patient: noted that the patient has an APR.Severity.of.Illness.Code of 0, 
# APR.DRG.Description as 'Ungroupable', and 'APR.Medical.Surgical.Description' of 'Not Applicable', which is distinctly different from the rest
print(sum(data$APR.Medical.Surgical.Description == "Not Applicable"))
print(sum(data$APR.DRG.Description == "UNGROUPABLE"))

# decided to remove this patient record
data = subset(data, APR.Risk.of.Mortality != "")
print(data[data$APR.Risk.of.Mortality == "", ])
print(data[data$APR.Severity.of.Illness.Description == "", ])

summary(data)

# to replace "" for Hospital.Service.Area with 'Enhanced De-identification'
# noted that blank are for records with enhanced de-identification (from NYSDOH_SPARCS_De-Identified_Data_Dictionary_2022)
data$Hospital.Service.Area[data$Hospital.Service.Area == ""] = "Enhanced De-identification"
unique(data$Hospital.Service.Area)

# conversion of data types to numeric
# noted that values are formatted with commas, which R does not recognise as a valid numerical format, remove comma first before conversion to numeric
data$Total.Charges = as.numeric(gsub(",", "", data$Total.Charges))
data$Total.Costs = as.numeric(gsub(",", "", data$Total.Costs))
class(data$Total.Charges)
class(data$Total.Costs)

# convert remaining empty strings to NA (Payment Typology)
data[data == ""] = NA
# total NA: 34904 (-2 -2 - 31 due to previous conversion)
sum(is.na(data))

# check after conversions
str(data)

# --------------------------------------------------------------------

# q2: notable finding 2: conversion to factors + one hot encoding and ordinal encoding 
# for nominal and ordinal categorical variables respectively

# check if categorical variables are factors
print(sapply(data, is.factor)) 

# since they are not, convert to factors
# particularly for APR.Severity.of.Illness.Code only, since I plan to encode the columns with string values
unique(data$APR.Severity.of.Illness.Code)

# 1 (least severe) -> 4 (most severe) (since 0 is removed)
# keep a copy of unordered
data$APR.Severity.of.Illness.Code_unordered = as.factor(data$APR.Severity.of.Illness.Code)
data$APR.Severity.of.Illness.Code = ordered(data$APR.Severity.of.Illness.Code, levels = c("1","2","3","4"))
unique(data$APR.Severity.of.Illness.Code)

# age group is likely ordinal (young to old)
# logically, having a higher age may indicate a longer stay
unique(data$Age.Group)
unique(data$APR.Risk.of.Mortality)

ordinal_agegrp = c("0 to 17", "18 to 29", "30 to 49", "50 to 69", "70 or Older")
ordinal_aprrom =c("Minor", "Moderate", "Major", "Extreme")

# convert to integers, then convert integers to factors
# age grp
data$Age.Group = factor(data$Age.Group, levels=ordinal_agegrp, ordered=TRUE)
data$Age.Group = as.integer(data$Age.Group) - 1
data$Age.Group_unordered = data$Age.Group
data$Age.Group = ordered(data$Age.Group, levels = c("0","1","2","3","4"))
unique(data$Age.Group)

# apr risk of mortality
data$APR.Risk.of.Mortality = factor(data$APR.Risk.of.Mortality, levels=ordinal_aprrom, ordered=TRUE)
data$APR.Risk.of.Mortality = as.integer(data$APR.Risk.of.Mortality) 
data$APR.Risk.of.Mortality_unordered = data$APR.Risk.of.Mortality
data$APR.Risk.of.Mortality = ordered(data$APR.Risk.of.Mortality, levels = c("1","2","3","4"))
unique(data$APR.Risk.of.Mortality)

print(sapply(data, is.factor)) 

nominal = c("Hospital.Service.Area", "Gender", "Race", "Ethnicity", "Type.of.Admission", "Patient.Disposition", "CCSR.Diagnosis.Code", "APR.DRG.Code", "APR.Medical.Surgical.Description", "Emergency.Department.Indicator")

for (var in nominal){
  print(unique(data[[var]]))
}

for (var in nominal) {
  if (var %in% names(data)) {
    unique_levels = unique(data[[var]])
    
    for (level in unique_levels) {
      new_col_name = paste(var, level, sep = "_")
      
      # create column with binary variables (level = 1 if present, otherwise 0)
      data[[new_col_name]] = ifelse(data[[var]] == level, 1, 0)
      
      print(paste("One hot encoding of", var, "(", level, ") to new column", new_col_name))
    }
  } else {
    print(paste("Variable", var, "not found in data."))
  }
}

#write.csv(data, "test_encoded_data.csv")

str(data)

# ---------------------------------------------------------------------

# q1: notable finding 3: chi-squared test and multi-collinearity

# remove the most frequent category as the reference group to avoid perfect collinearity
one_hot_encoded = c(
  # one hot encoded columns
  "Hospital.Service.Area_New York City", "Hospital.Service.Area_Hudson Valley", "Hospital.Service.Area_Enhanced De-identification", "Hospital.Service.Area_Long Island",
  "Hospital.Service.Area_Capital/Adirond", "Hospital.Service.Area_Finger Lakes", "Hospital.Service.Area_Central NY", "Hospital.Service.Area_Western NY", "Hospital.Service.Area_Southern Tier",
  
  "Gender_M", "Gender_F", "Gender_U",
  "Race_Other Race", "Race_Multi-racial", "Race_White", "Race_Black/African American", 
  "Ethnicity_Spanish/Hispanic", "Ethnicity_Not Span/Hispanic", "Ethnicity_Unknown", "Ethnicity_Multi-ethnic", 
  "Type.of.Admission_Emergency", "Type.of.Admission_Urgent", "Type.of.Admission_Elective", "Type.of.Admission_Trauma", "Type.of.Admission_Not Available",
  
  "Patient.Disposition_Home or Self Care", "Patient.Disposition_Expired", "Patient.Disposition_Skilled Nursing Home", "Patient.Disposition_Facility w/ Custodial/Supportive Care",
  "Patient.Disposition_Home w/ Home Health Services", "Patient.Disposition_Left Against Medical Advice", "Patient.Disposition_Short-term Hospital", "Patient.Disposition_Inpatient Rehabilitation Facility", "Patient.Disposition_Another Type Not Listed",
  "Patient.Disposition_Hospice - Medical Facility", "Patient.Disposition_Hospice - Home", "Patient.Disposition_Medicare Cert Long Term Care Hospital", 
  "Patient.Disposition_Psychiatric Hospital or Unit of Hosp", "Patient.Disposition_Court/Law Enforcement", "Patient.Disposition_Federal Health Care Facility", "Patient.Disposition_Hosp Basd Medicare Approved Swing Bed",
  "Patient.Disposition_Medicaid Cert Nursing Facility", "Patient.Disposition_Cancer Center or Children's Hospital", "Patient.Disposition_Critical Access Hospital",
  
  "APR.DRG.Code_720", "APR.DRG.Code_5", "APR.DRG.Code_710", "APR.DRG.Code_890", "APR.DRG.Code_4", "APR.DRG.Code_9", "APR.DRG.Code_892", "APR.DRG.Code_1", "APR.DRG.Code_6", 
  "APR.DRG.Code_7", "APR.DRG.Code_8", "APR.DRG.Code_956", "APR.DRG.Code_2", "APR.DRG.Code_161",
  
  "APR.Medical.Surgical.Description_Medical", "APR.Medical.Surgical.Description_Surgical", "APR.Medical.Surgical.Description_Not Applicable",
  "Emergency.Department.Indicator_Y", "Emergency.Department.Indicator_N"
)

  
freq_onehot = list()

for (col in one_hot_encoded) {
  n = sum(data[[col]] == 1, na.rm = TRUE)
  freq_onehot[[col]] = n
}

freq_onehot

# one hot encoded values: removed (set as reference):
# Hospital.Service.Area_New York City: 11311
# Gender_M: 14506
# Race_White: 16708
# Ethnicity_Not Span/Hispanic
# Type.of.Admission_Emergency = 26940
# Patient.Disposition_Home or Self Care: 10240
# APR.DRG.Code_720: 24561
# APR.Medical.Surgical.Description_Medical: 24884
# Emergency.Department.Indicator_Y: 26642

freq_ord = list()

ordinal_var = c("Age.Group", "APR.Risk.of.Mortality", "APR.Severity.of.Illness.Code")


for (col_name in ordinal_var) {
  i = names(sort(table(data[[col_name]]), decreasing = TRUE)[1])
  freq_ord[[col_name]] = i
}

freq_ord

# "4" is the most frequent for all ordinal variables

# re-leveling 
data$Age.Group_ref = as.factor(data$Age.Group_unordered)
data$Age.Group_ref = relevel(data$Age.Group_ref, ref="4")

data$APR.Risk.of.Mortality_ref = as.factor(data$APR.Risk.of.Mortality_unordered)
data$APR.Risk.of.Mortality_ref = relevel(data$APR.Risk.of.Mortality_ref, ref="4")

data$APR.Severity.of.Illness.Code_ref = relevel(data$APR.Severity.of.Illness.Code_unordered, ref="4")

data$APR.Risk.of.Mortality_ref = factor(data$APR.Risk.of.Mortality_ref, levels = c("1", "2", "3", "4"))
data$APR.Risk.of.Mortality_ref = droplevels(data$APR.Risk.of.Mortality_ref)
levels(data$APR.Risk.of.Mortality_ref)

data$APR.Severity.of.Illness.Code_ref = factor(data$APR.Severity.of.Illness.Code_ref, levels = c("1", "2", "3", "4"))
data$APR.Severity.of.Illness.Code_ref = droplevels(data$APR.Severity.of.Illness.Code_ref)
levels(data$APR.Severity.of.Illness.Code_ref)


# statistically significant association between APR.DRG.Code and the APR.Medical.Surgical.Description
# thus, the classification type is likely influenced by the specific APR.DRG.Code
# exclude APR.Medical.Surgical.Description to prevent multicollinearity
unique(data$APR.DRG.Code)
unique(data$APR.Medical.Surgical.Description)

drgcode_description = table(data$APR.DRG.Code, data$APR.Medical.Surgical.Description)
chisq.test(drgcode_description, simulate.p.value = TRUE)

table(data$APR.DRG.Code, data$APR.Medical.Surgical.Description)

possible_predictors = data[, c(
  # one hot encoded columns (references removed)
  # removed Total.Charges and Total.Costs
  "Hospital.Service.Area_Hudson Valley", "Hospital.Service.Area_Enhanced De-identification", "Hospital.Service.Area_Long Island",
  "Hospital.Service.Area_Capital/Adirond", "Hospital.Service.Area_Finger Lakes", "Hospital.Service.Area_Central NY", "Hospital.Service.Area_Western NY", "Hospital.Service.Area_Southern Tier",
  
  "Gender_F", "Gender_U",
  
  "Race_Other Race", "Race_Multi-racial", "Race_Black/African American", 
  
  "Ethnicity_Spanish/Hispanic", "Ethnicity_Unknown", "Ethnicity_Multi-ethnic", 
  
  "Type.of.Admission_Urgent", "Type.of.Admission_Elective", "Type.of.Admission_Trauma", "Type.of.Admission_Not Available",
  
  "Patient.Disposition_Expired", "Patient.Disposition_Skilled Nursing Home", "Patient.Disposition_Facility w/ Custodial/Supportive Care",
  "Patient.Disposition_Home w/ Home Health Services", "Patient.Disposition_Left Against Medical Advice", "Patient.Disposition_Short-term Hospital", "Patient.Disposition_Inpatient Rehabilitation Facility", "Patient.Disposition_Another Type Not Listed",
  "Patient.Disposition_Hospice - Medical Facility", "Patient.Disposition_Hospice - Home", "Patient.Disposition_Medicare Cert Long Term Care Hospital", 
  "Patient.Disposition_Psychiatric Hospital or Unit of Hosp", "Patient.Disposition_Court/Law Enforcement", "Patient.Disposition_Federal Health Care Facility", "Patient.Disposition_Hosp Basd Medicare Approved Swing Bed",
  "Patient.Disposition_Medicaid Cert Nursing Facility", "Patient.Disposition_Cancer Center or Children's Hospital", "Patient.Disposition_Critical Access Hospital",
  
  "APR.DRG.Code_5", "APR.DRG.Code_710", "APR.DRG.Code_890", "APR.DRG.Code_4", "APR.DRG.Code_9", "APR.DRG.Code_892", "APR.DRG.Code_1", "APR.DRG.Code_6", 
  "APR.DRG.Code_7", "APR.DRG.Code_8", "APR.DRG.Code_2", "APR.DRG.Code_161",
  
  "Emergency.Department.Indicator_N",

  # ordinal encoded columns
  "Age.Group_ref", "APR.Risk.of.Mortality_ref",
  
  # other columns to be taken into account
  "APR.Severity.of.Illness.Code_ref",
  "Length.of.Stay"
)]

data_vif = lm(data=possible_predictors, Length.of.Stay ~ .)
alias(data_vif)
vif(data_vif)


# --------------------------------------------------------------------

# q2: determined potential predictors via: 
# correlation (based on domain knowledge and logical deduction),
# chi-squared test (for categorical), 
# relative importance (via VarImp), 
# and through step-wise regression

full_predictors = data[, c(
  # included all categories
  "Hospital.Service.Area_New York City", "Hospital.Service.Area_Hudson Valley", "Hospital.Service.Area_Enhanced De-identification", "Hospital.Service.Area_Long Island",
  "Hospital.Service.Area_Capital/Adirond", "Hospital.Service.Area_Finger Lakes", "Hospital.Service.Area_Central NY", "Hospital.Service.Area_Western NY", "Hospital.Service.Area_Southern Tier",
  
  "Gender_M" ,"Gender_F", "Gender_U",
  
  "Patient.Disposition_Home or Self Care", "Patient.Disposition_Expired", "Patient.Disposition_Skilled Nursing Home", "Patient.Disposition_Facility w/ Custodial/Supportive Care",
  "Patient.Disposition_Home w/ Home Health Services", "Patient.Disposition_Left Against Medical Advice", "Patient.Disposition_Short-term Hospital", "Patient.Disposition_Inpatient Rehabilitation Facility", "Patient.Disposition_Another Type Not Listed",
  "Patient.Disposition_Hospice - Medical Facility", "Patient.Disposition_Hospice - Home", "Patient.Disposition_Medicare Cert Long Term Care Hospital", 
  "Patient.Disposition_Psychiatric Hospital or Unit of Hosp", "Patient.Disposition_Court/Law Enforcement", "Patient.Disposition_Federal Health Care Facility", "Patient.Disposition_Hosp Basd Medicare Approved Swing Bed",
  "Patient.Disposition_Medicaid Cert Nursing Facility", "Patient.Disposition_Cancer Center or Children's Hospital", "Patient.Disposition_Critical Access Hospital",
  
  "Race_White", "Race_Other Race", "Race_Multi-racial", "Race_Black/African American", 
  
  "Ethnicity_Not Span/Hispanic", "Ethnicity_Spanish/Hispanic", "Ethnicity_Unknown", "Ethnicity_Multi-ethnic", 
  
  "Type.of.Admission_Emergency", "Type.of.Admission_Urgent", "Type.of.Admission_Elective", "Type.of.Admission_Trauma", "Type.of.Admission_Not Available",
  
  "APR.DRG.Code_720", "APR.DRG.Code_5", "APR.DRG.Code_710", "APR.DRG.Code_890", "APR.DRG.Code_4", "APR.DRG.Code_9", "APR.DRG.Code_892", "APR.DRG.Code_1", "APR.DRG.Code_6", 
  "APR.DRG.Code_7", "APR.DRG.Code_8", "APR.DRG.Code_2", "APR.DRG.Code_161",
  
  "Emergency.Department.Indicator_Y", "Emergency.Department.Indicator_N",
  
  # ordinal encoded columns
  "Age.Group", "APR.Risk.of.Mortality",
  
  # other columns to be taken into account
  "APR.Severity.of.Illness.Code",
  
  # Y value
  "Length.of.Stay"
)]


# to determine near zero variables (to exclude)
nzv = nearZeroVar(full_predictors, saveMetrics=TRUE)
print(nzv)  

data$APR.Severity.of.Illness.Code_num = as.numeric(as.character(data$APR.Severity.of.Illness.Code))
data$APR.Risk.of.Mortality_num = as.numeric(as.character(data$APR.Risk.of.Mortality))
data$Age.Group_num = as.numeric(as.character(data$Age.Group))
print(unique(data$Age.Group))
print(unique(data$Age.Group_num))
# removed Emergency.Department.Indicator_Y, Gender_M to prevent multicollinearity for lm 
predictors_subset = data[, c(
  
  "Hospital.Service.Area_New York City", "Hospital.Service.Area_Hudson Valley", "Hospital.Service.Area_Long Island",
  "Hospital.Service.Area_Capital/Adirond", "Hospital.Service.Area_Finger Lakes", "Hospital.Service.Area_Central NY", "Hospital.Service.Area_Western NY", 
  
  "Gender_F",
  
  "Patient.Disposition_Home or Self Care", "Patient.Disposition_Expired", 
  "Patient.Disposition_Skilled Nursing Home", "Patient.Disposition_Home w/ Home Health Services",
  
  "Race_White", "Race_Other Race", "Race_Black/African American", 
  
  "Ethnicity_Not Span/Hispanic", "Ethnicity_Spanish/Hispanic", "Ethnicity_Unknown", 
  
  "APR.DRG.Code_720", "APR.DRG.Code_710",  
  
  "Emergency.Department.Indicator_N", 
  
  # ordinal encoded columns
  "Age.Group_num", "APR.Risk.of.Mortality_num",
  
  # other columns to be taken into account
  "APR.Severity.of.Illness.Code_num", 
  
  # Y value
  "Length.of.Stay"
)]

# for LR
lr_importance = train(Length.of.Stay ~ ., data=predictors_subset, method="lm")
varImp(lr_importance)

# for CART
cart_importance = rpart(data$Length.of.Stay ~ ., data=predictors_subset)
cart_varimp = cart_importance$variable.importance
cart_varimp
cart_scaled_varimp = round(100*cart_varimp/sum(cart_varimp))
cart_scaled_varimp

# kendall's tau correlation for ordinal variables
cor.test(data$Length.of.Stay, data$Age.Group_num, method="kendall")
cor.test(data$Length.of.Stay, data$APR.Risk.of.Mortality_num, method="kendall")
cor.test(data$Length.of.Stay, data$APR.Severity.of.Illness.Code_num, method="kendall")

predictors_subset_onehotencoded = c(
  "Hospital.Service.Area_New York City", "Hospital.Service.Area_Hudson Valley", "Hospital.Service.Area_Long Island",
  "Hospital.Service.Area_Capital/Adirond", "Hospital.Service.Area_Finger Lakes", "Hospital.Service.Area_Central NY", "Hospital.Service.Area_Western NY", 
  
  "Gender_F", "Gender_M",
  
  "Patient.Disposition_Home or Self Care", "Patient.Disposition_Expired", 
  "Patient.Disposition_Skilled Nursing Home", "Patient.Disposition_Home w/ Home Health Services",
  
  "Race_White", "Race_Other Race", "Race_Black/African American", 
  
  "Ethnicity_Not Span/Hispanic", "Ethnicity_Spanish/Hispanic", "Ethnicity_Unknown", 
  
  "APR.DRG.Code_720", "APR.DRG.Code_710",  
  
  "Emergency.Department.Indicator_N", "Emergency.Department.Indicator_Y"
)

# kruskal wallis test for nominal variables
kruskal = list()

for (var in predictors_subset_onehotencoded) {
  formula = as.formula(paste("Length.of.Stay ~ `", var, "`", sep = ""))
  kruskal_result = kruskal.test(formula, data=data)
  kruskal[[var]] = kruskal_result
}

for (var in predictors_subset_onehotencoded) {
  cat("\nKruskal-Wallis Test Result for", var, ":\n")
  print(kruskal[[var]])
}

# to be included:
# var imp (LR) > 5, var imp (CART) > 5
# kruskal wallis chi squared > 100
# kendall's tau: moderate correlation

final_predictors_test = data[, c(
  "Hospital.Service.Area_Hudson Valley", # var imp for lm: 8.255
  "Hospital.Service.Area_New York City", # kruskal wallis chi squared: 249.82
  "Hospital.Service.Area_Capital/Adirond", # var imp for lm: 6.993
  "Hospital.Service.Area_Long Island", # var imp for lm: 5.727
  
  "Patient.Disposition_Home or Self Care", # kruskal wallis chi squared: 2779.5, var imp for lm: 23.316, var imp for CART: 8
  "Patient.Disposition_Expired", # var imp for lm: 10.070
  "Patient.Disposition_Skilled Nursing Home", # kruskal wallis chi squared: 2888.3, var imp for lm: 38.730

  "APR.DRG.Code_720", # kruskal wallis chi squared: 1863.4, var imp for lm: 100, var imp for cart: 22%
  "APR.DRG.Code_710", # kruskal wallis chi squared: 1279.8, var imp for lm: 53.667, var imp for cart: 23%
  
  "Race_Black/African American", # kruskal wallis chi squared: 173.35
  "Race_White", # kruskal wallis chi squared: 101.73
  
  "Emergency.Department.Indicator_N", # var imp for lm: 13.369
  
  "APR.Risk.of.Mortality", # var imp for lm: 10.620, var imp for cart: 17% ,kendall's tau: 0.28
  
  "APR.Severity.of.Illness.Code", # var imp for lm: 47.743, var imp for cart: 22%, kendall's tau: 0.32
  
  "Age.Group", #var imp for lm: 13.722
  
  # Y value
  "Length.of.Stay"
)]

#str(final_predictors_test)
# ------------------------------------------------------------------

# q3. linear regression and CART to compare test set errors
# based on selected predictors as per q2

# linear reg
# 70% train set, 30% test set
set.seed(999)
data_train = sample.split(final_predictors_test$Length.of.Stay, SplitRatio=0.7)
data_trainset = subset(final_predictors_test, data_train==TRUE)
data_testset = subset(final_predictors_test, data_train==FALSE)

data_trainset = as.data.frame(data_trainset) 
data_testset = as.data.frame(data_testset) 

# train linear reg model on train set
lr_train = lm(Length.of.Stay ~., data=data_trainset)

# predict on test set
lr_pred = predict(lr_train, newdata=data_testset)

rmse_lr = sqrt(mean((lr_pred - data_testset$Length.of.Stay)^2))
print(rmse_lr)

r2_lr = 1 - (sum((data_testset$Length.of.Stay - lr_pred)^2) / sum((data_testset$Length.of.Stay - mean(data_testset$Length.of.Stay))^2))
print(r2_lr)

# plotting of lm: model diagnosis
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(lr_train)

# optimisation via stepwise linear regression 
set.seed(888)

initial = lm(Length.of.Stay ~., data=data_trainset)
stepwise = step(initial, direction="both", trace=1)

summary(stepwise)
varImp(stepwise)

lr_stepwise_pred = predict(stepwise, newdata=data_testset)  
summary(lr_stepwise_pred)

rmse_lr_stepwise = sqrt(mean((lr_stepwise_pred - data_testset$Length.of.Stay)^2))
print(rmse_lr_stepwise)

r2_lr_stepwise = 1 - sum((data_testset$Length.of.Stay - lr_stepwise_pred)^2) / sum((data_testset$Length.of.Stay - mean(data_testset$Length.of.Stay))^2)
print(r2_lr_stepwise)

# plotting of optimised lm: model diagnosis
par(mfrow = c(2, 2))
plot(stepwise)

# CART
# set seed for randomisation in 10 fold cv 
set.seed(1000)

# used minsplit=1000 (arbitrary value) as this is a large data set
cart_train = rpart(Length.of.Stay ~ ., data=data_trainset, method="anova", control=rpart.control(minsplit=1000, cp=0))
print(cart_train)
printcp(cart_train)
plotcp(cart_train)
rpart.plot(cart_train, nn=TRUE, main="Maximal Regression Tree")

# optimisation of CART model
# find min cv error tree 
# 1 se rule

# min cp from tree w min cv
min_cp = cart_train$cptable[which.min(cart_train$cptable[,"xerror"]), "CP"]
print(min_cp)
# min cv
min_xerror = min(cart_train$cptable[,"xerror"])
print(min_xerror)
# min se
min_xstd = cart_train$cptable[which.min(cart_train$cptable[,"xerror"]), "xstd"]
print(min_xstd)
# threshold for acceptable cv 
threshold_xerror = min_xerror + min_xstd
print(threshold_xerror)
# largest cp based on threshold
best_cp = max(cart_train$cptable[cart_train$cptable[,"xerror"]<=threshold_xerror, "CP"])
print(best_cp)

# pruned tree
pruned_tree = prune(cart_train, cp=best_cp)
rpart.plot(pruned_tree, nn=TRUE, main="Pruned Tree")
pruned_varimp = pruned_tree$variable.importance
pruned_varimp
pruned_scaled_varimp = round(100*pruned_varimp/sum(pruned_varimp))
pruned_scaled_varimp

# test based on pruned tree
cart_test = predict(pruned_tree, newdata=data_testset)
summary(cart_test)

cart_rmse = sqrt(mean((cart_test - data_testset$Length.of.Stay)^2))
print(cart_rmse)

r2_cart = 1 - (sum((data_testset$Length.of.Stay - cart_test)^2)) / sum((data_testset$Length.of.Stay - mean(data_testset$Length.of.Stay))^2)
print(r2_cart)


# random forest
set.seed(111)

train_control = trainControl(method="cv", number=10)

param_grid = expand.grid(mtry=c(1, 2, 3, 4, 5))

rf_tuned = train(Length.of.Stay ~., data=data_trainset, method="rf", trControl=train_control, tuneGrid=param_grid, ntree=50)
print(rf_tuned)

rf_pred_tuned = predict(rf_tuned, newdata=data_testset)
summary(rf_pred_tuned)

rmse_rf_tuned = sqrt(mean((rf_pred_tuned - data_testset$Length.of.Stay)^2))

print(rmse_rf_tuned)

r2_rf = 1 - (sum((data_testset$Length.of.Stay - rf_pred_tuned)^2)) / sum((data_testset$Length.of.Stay - mean(data_testset$Length.of.Stay))^2)
print(r2_rf)

varImp(rf_tuned)
# -------------------------------------------------------------------------

# q4. insights on business application

# histogram of length of stay -> taking mean of LOS to estimate LOS is not the optimal soln
ggplot(data_trainset, aes(x = Length.of.Stay)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Histogram of Length of Stay", x = "Length of Stay", y = "Frequency") +
  theme_minimal()

summary(data$Length.of.Stay)

los_freq = table(data$Length.of.Stay)
los_freq

lr_charge = lm(Total.Charges ~ Length.of.Stay, data=data)
summary(lr_charge)

lr_cost = lm(Total.Costs ~ Length.of.Stay, data=data)
summary(lr_cost)

cor.test(data$Length.of.Stay, data$Total.Charge, method="pearson")
cor.test(data$Length.of.Stay, data$Total.Cost, method="pearson")

# log transformation of LOS
data$Length.of.Stay_log = log(data$Length.of.Stay + 1)
summary(data$Length.of.Stay_log)

# lm model after log transformation of LOS
data_trainset$Length.of.Stay_log = log(data_trainset$Length.of.Stay + 1)
data_testset$Length.of.Stay_log = log(data_testset$Length.of.Stay + 1)

data_trainset = as.data.frame(data_trainset)
data_testset = as.data.frame(data_testset)

lr_train_log = lm(Length.of.Stay_log ~ ., data=data_trainset)

lr_pred_log = predict(lr_train_log, newdata=data_testset)
summary(lr_pred_log)

rmse_lr_log = sqrt(mean((lr_pred_log - data_testset$Length.of.Stay_log)^2))
print(rmse_lr_log)

r2_lr_log = 1 - (sum((data_testset$Length.of.Stay_log - lr_pred_log)^2)/sum((data_testset$Length.of.Stay_log - mean(data_testset$Length.of.Stay_log))^2))
print(r2_lr_log)

lr_pred_ori = exp(lr_pred_log) - 1
rmse_lr_ori = sqrt(mean((data_testset$Length.of.Stay - lr_pred_ori)^2))
print(rmse_lr_ori)

r2_lr_ori = 1 - (sum((data_testset$Length.of.Stay - lr_pred_ori)^2)/sum((data_testset$Length.of.Stay - mean(data_testset$Length.of.Stay))^2))
print(r2_lr_ori)

# cart model after log transformation of LOS
cart_train_log = rpart(Length.of.Stay_log ~ ., data=data_trainset, method="anova", control=rpart.control(minsplit=1000, cp=0))
print(cart_train_log)
printcp(cart_train_log)
plotcp(cart_train_log)

rpart.plot(cart_train_log, nn=TRUE, main="Maximal Regression Tree")

# optimisation of CART model
# find min cv error tree 
# 1 se rule

# min cp from tree w min cv
min_cp_log = cart_train_log$cptable[which.min(cart_train_log$cptable[,"xerror"]), "CP"]
# min cv
min_xerror_log = min(cart_train_log$cptable[,"xerror"])
# min se
min_xstd_log = cart_train_log$cptable[which.min(cart_train_log$cptable[,"xerror"]), "xstd"]
# threshold for acceptable cv 
threshold_xerror_log = min_xerror_log + min_xstd_log
# largest cp based on threshold
best_cp_log = max(cart_train_log$cptable[cart_train_log$cptable[,"xerror"]<=threshold_xerror_log, "CP"])
print(best_cp_log)

# pruned tree
pruned_tree_log = prune(cart_train_log, cp=best_cp_log)

# test based on pruned tree
cart_pred_log = predict(pruned_tree_log, newdata=data_testset)
summary(cart_pred_log)

rmse_cart_log = sqrt(mean((cart_pred_log - data_testset$Length.of.Stay_log)^2))
print(rmse_cart_log)

r2_cart_log = 1 - (sum((data_testset$Length.of.Stay_log - cart_pred_log)^2)/sum((data_testset$Length.of.Stay_log - mean(data_testset$Length.of.Stay_log))^2))
print(r2_cart_log)

cart_pred_ori = exp(cart_pred_log) - 1
rmse_cart_ori = sqrt(mean((data_testset$Length.of.Stay - cart_pred_ori)^2))
print(rmse_cart_ori)

r2_cart_ori = 1 - (sum((data_testset$Length.of.Stay - cart_pred_ori)^2)/sum((data_testset$Length.of.Stay - mean(data_testset$Length.of.Stay))^2))
print(r2_cart_ori)

