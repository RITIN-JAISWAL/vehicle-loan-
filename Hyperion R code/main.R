# ************************************************
# Author: NAMES
# Practical Business Analytics Coursework
#
# Vehicle Loan Dataset Predictor and Classifier
#
# 28th November 2019
#
# UPDATE
# ************************************************
#  clears all objects in "global environment"
rm(list=ls())

#GLOBAL CONSTANTS
DATASET_FILENAME  <- "train.csv"          # Name of input dataset file
DATASET_PROCESSED <- "train_processed.csv"    # Name of input processed dataset file
OUTPUT_FIELD      <- "loan_default"             # Field name of the output class to predict
splitdata           <- 70                   # % split to create TRAIN dataset
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
DISCREET_BINS     <- 5                    # Number of empty bins to determine discreet
MAX_LITERALS      <- 150                   # Maximum numner of hotcoding new fields
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 1000                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage

BASICNN_HIDDEN    <- 10                   # 10 hidden layer neurons
BASICNN_EPOCHS    <- 100                  # Maximum number of training epocs
KFOLDS          <- 6                 # Number of folded experiments
TODAY_DATE <- Sys.Date()
###################################################################################

# ************************************************
# main() :
# Function which controls execution
#
# OUTPUT  : Processed dataset, Classifer Model
# ************************************************
main <- function() {
  # Preprocessing Section
  process(DATASET_FILENAME) 
  
  # Model Creation Section
  classify_model(read.csv(DATASET_PROCESSED))
  
  profit_data <- read.csv('profit.csv')
  #Neural Network Section
  neural(profit_data)
}

# ************************************************
# process() :
# Function which controls preprocessing execution functions
# INPUT   : Raw dataset
# OUTPUT  : Processed dataset
# ************************************************
process <- function(file_name) {
  dataset <- read.csv(file_name,encoding="UTF-8",stringsAsFactors=FALSE)
  irrelevant_fields <- c("manufacturer_id", "supplier_id", "PERFORM_CNS.SCORE.DESCRIPTION", "MobileNo_Avl_Flag", "branch_id")
  
  #Fields to be noramlised during processing
  norm_fields <- c("loaned_amount", "asset_cost", "ltv", "age", "loan_age_months", "pri_no_accounts", "pri_active_accounts", "pri_overdue_accounts",
                   "pri_current_bal", "pri_sanc_amount", "pri_disbursed_amount", "sec_no_accounts", "sec_active_accounts", "sec_overdue_accounts", 
                   "sec_current_bal", "sec_sanc_ammount", "sec_disbursed_amount", "pri_instal_amt", "sec_instal_amt", "new_accts_in_six_months", 
                   "delinq_accts_six_months", "avg_accnt_age_mon", "credit_hist_len_mon", "num_enquiries")
  #CNS score is not being normalised during preprocessing as a large proportion are 0, during ML this should be split into the two clusters first
  dataset <- preprocess_dataset(dataset, irrelevant_fields, norm_fields)
  
  write.csv(dataset, 'train_processed.csv', row.names = FALSE)
}

gc()
cat("\014") 
set.seed(144)
#Define and load libraries required NEED TO CHOOSE THESE
projectLibraries<-c("outliers",
                    "corrplot",
                    "MASS",
                    "pROC",
                    "formattable",
                    "stats",
                    "caret",
                    "PerformanceAnalytics",
                    "party",
                    "dplyr",
                    "tidyverse",
                    "lubridate",
                    "tidyr",
                    "cluster",
                    "factoextra",
                    "caTools",
                    "eeptools",
                    "rpart",
                    "date",
                    "gclus",
                    "ggplot2",
                    "C50",
                    "tidyverse",
                     "randomForest",
                    "e1071",
                    "neuralnet")

#change this on your computers if required
#setwd("C:/Users/ts00987/OneDrive - University of Surrey/Documents/PBAV1/Final Code")
library(pacman)
pacman::p_load(char=projectLibraries,install=TRUE,character.only=TRUE)
source('nick_code.R')
source('functions.R')

main()
