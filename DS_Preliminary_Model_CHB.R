library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)  

# Reading in the main dataset
main <- read.csv(file="C:\\Users\\bushc\\Desktop\\DS-450\\2017_18 telemetry lock and dam 19 analysis master.csv",header=TRUE, sep=",")

## CONVERTING DATE COLUMNS
# Converting Date and Time
main$Date<-mdy(main$Date)                 # Convert Date column to Date objects
main$DeployDate<-mdy(main$DeployDate)     # Fixed previous wrong format
main$Up2017.date<-mdy(main$Up2017.date)
main$Up2018.date<-mdy(main$Up2018.date)

main$START_DATETIME<-mdy_hm(main$START_DATETIME)  # Convert Start datetime
main$END_DATETIME<-mdy_hm(main$END_DATETIME)      # Convert End datetime

## CHECKING FOR MISSING DATA
# Columns to check for missing values 
cols_to_check<-c("TRANSMITTERID", "Species", "AGENCY", "Deploy.loc",
                   "Up.passage", "Down.passage", "RESIDENCEEVENT",
                   "DURATION.sec", "DURATION.min", "log.DUR.min", "NUMRECS")

# Removing rows with missing values in key columns
main_clean<-main[complete.cases(main[, cols_to_check]), ]

# Count rows removed due to missing data
n_removed<-nrow(main)-nrow(main_clean)
cat("Rows removed due to missing data:", n_removed, "\n")

## REMOVING DUPLICATES AND UNNEEDED COLUMNS
# Removing Duplicate Measurement Columns
main_clean<-main_clean %>% select(-Stage.Ft, -DURATION.sec)

# Removing Variables With 95% Missing
main_clean<-main_clean %>% select(-Up2017.date, -Up2018.date)

## CONVERTING CATEGORICAL VARIABLES
# Converting categorical variables to factors for easier modeling
main_clean$Season<-as.factor(main_clean$Season)
main_clean$Species<-as.factor(main_clean$Species)
main_clean$AGENCY<-as.factor(main_clean$AGENCY)
main_clean$Deploy.loc<-as.factor(main_clean$Deploy.loc)
main_clean$Up.passage<-as.factor(main_clean$Up.passage)
main_clean$Down.passage<-as.factor(main_clean$Down.passage)
main_clean$UpPass.2017<-as.factor(main_clean$UpPass.2017)
main_clean$UpPass.2018<-as.factor(main_clean$UpPass.2018)

## HANDLING MISSING VALUES
# Handling missing values for Length and Weight using median imputation
main_clean$Length[is.na(main_clean$Length)]<-median(main_clean$Length, na.rm=TRUE)
main_clean$Weight[is.na(main_clean$Weight)]<-median(main_clean$Weight, na.rm=TRUE)

## CREATING LOG-TRANSFORMED VARIABLES BECAUSE OF MAJOR OUTLIERS
# Add log-transformed versions of numeric columns
main_clean$log_DURATION<-log(main_clean$DURATION.min + 1)  # +1 avoids log(0)
main_clean$log_NUMRECS<-log(main_clean$NUMRECS + 1)        
main_clean$log_Weight<-log(main_clean$Weight + 1)         

## CHECKING THE CLEANED DATA
summary(main_clean)
colSums(is.na(main_clean))/nrow(main_clean)

library(randomForest)

## RANDOM FOREST MODEL
# Checking Assumptions
length(unique(main_clean$TRANSMITTERID))
nrow(main_clean)
# Not every entry is unique so will have to do train/test split with all transmitterid that are the same together

numeric_vars<-main_clean[,sapply(main_clean, is.numeric)]
cor(numeric_vars, use = "complete.obs")
#Correlation Matrix which is a little off due to the transmitterid 

table(main_clean$Up.passage)
prop.table(table(main_clean$Up.passage))
# There is a class imbalance, so will have to weight the classes 

# Handling TRANSMITTERID
# Get unique transmitters
unique_transmitters<-unique(main_clean$TRANSMITTERID)

# Randomly assign 70% of transmitters to training
set.seed(123)
train_transmitters<-sample(unique_transmitters, 0.7 * length(unique_transmitters))

# Assigning rows to train/test based on transmitter (WAS NOT SURE ON HOW TO DO THIS)
train_data<-main_clean %>% filter(TRANSMITTERID %in% train_transmitters)
test_data <-main_clean %>% filter(!TRANSMITTERID %in% train_transmitters)


# Handling the Imbalance 
# Making Predictor Formula
rf_formula<-Up.passage~Temp+Stage.m+Week+Season+Species+Length+log_Weight+
  Tot.lock.n + Barge.Tot.n+Rec.Tot.n+log_DURATION+log_NUMRECS
# Balanced Random Forest Model
rf_M<- randomForest(formula=rf_formula,data=train_data,ntree=500,importance=TRUE, sampsize=c("0"=140,"1"=140)) 
# Chose 140 because its smaller than the minority class size and is not too small.

rf_M
# This report shows that my model has performed very well on the training data.
# This is because there was very low class error. Also, only 2.6% of the time the model wrongly classifies a row in testing. 

importance(rf_M)
# From this it can be seen that weight, species, length, and week are the variables with the strongest influence.
# Overall biological traits and temporal information help most when predicting passage. 

#Predictions on Train/Test Data
train_preds<-predict(rf_M, newdata = train_data)
test_preds<-predict(rf_M, newdata = test_data)

table(Predicted=test_preds,Actual=test_data$Up.passage)
# This confusion matrix shows that the majority class (0) was predicted well with 792/828 accuracy. 
# However, model doesn't do as well at all when predicting 1. 
# This could be due to the original data containing very few occurrences of upstream passage.

test_probs<-predict(rf_M, newdata = test_data, type = "prob")
head(test_probs)
# The predictions aren't bad considering the likely-hood of 1 occurring.

# Looking at key metrics
CM<-table(Predicted=test_preds,Actual=test_data$Up.passage)

# Accuracy
accuracy<-sum(diag(CM))/sum(CM)

# Precision & Recall for class 1 using prop.table
precision<-CM["1","1"] / sum(CM["1",])
recall<-CM["1","1"] / sum(CM[,"1"])
f1_score<- 2 * precision * recall/(precision + recall)

# Quick print in one line
c(Accuracy=round(accuracy,4), Precision=round(precision,4),
  Recall=round(recall,4), F1=round(f1_score,4))   # The 4's just say to round to 4 decimal places
# The accuracy says that 93.2% of all predictions are correct
# The precision says that 56% of 1's are predicted correctly
# The recall shows that 65% of actual 1's are correctly identified

## MODEL CONCLUSIONS
# The model struggles to identify the minority events
# The most important predictors are weight, species, length, week.
# The data have very few upstream occurrences makes prediction precision weaker.
# If I was going to improve this model I would have to figure out some way to make the prediction of 1's better. 