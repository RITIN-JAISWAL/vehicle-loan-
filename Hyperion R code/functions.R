
#************************************************
# preprocess_dataset() :
# cleans the passed dataset
#
# INPUT: None
# OUTPUT :None
# ************************************************
preprocess_dataset <- function(dataset, irrelevant_fields, norm_fields) {
  #returns a clean but unnormalised dataset
  vehicle_loans <- preprocessing(dataset, irrelevant_fields, norm_fields)
  return(vehicle_loans)
}

#************************************************
# preprocessing() :
#
# INPUT: raw dataset in the dataset
# OUTPUT : returns the dataset in a usable state
# ************************************************
preprocessing <- function(dataset, irrelevantFields, norm_fields) {
  #remove rows determined as impactless due to prior knowledge
  new_dataset <- dataset[, !(names(dataset) %in% irrelevantFields)]
  
  #editing column names to make manipulating them later, simpler
  aestheticNames <- c("ID", "loaned_amount", "asset_cost", "ltv", "pincode", "age", "employment", "loan_age_months", "state_ID",
                      "employee_ID","aadhar_flag", "PAN_flag", "voter_flag", "driving_flag", "passport_flag", "CNS_score", "pri_no_accounts", "pri_active_accounts", "pri_overdue_accounts",
                      "pri_current_bal", "pri_sanc_amount", "pri_disbursed_amount", "sec_no_accounts", "sec_active_accounts", "sec_overdue_accounts", 
                      "sec_current_bal", "sec_sanc_ammount", "sec_disbursed_amount", "pri_instal_amt", "sec_instal_amt", "new_accts_in_six_months", 
                      "delinq_accts_six_months", "avg_accnt_age_mon", "credit_hist_len_mon", "num_enquiries", "loan_default")
  names(new_dataset) <- aestheticNames
  
  #Remove tuples with errors and/or missing data here FUNCTION SOON REPLACE THIS
  new_dataset <- new_dataset[!(is.na(new_dataset$employment) | new_dataset$employment==""), ]
  new_dataset$employment <- as.character(new_dataset$employment)
  new_dataset$ID <- as.character(new_dataset$ID)
  new_dataset$pincode <- as.character(new_dataset$pincode)
  new_dataset$state_ID <- as.character(new_dataset$state_ID)
  new_dataset$employee_ID <- as.character(new_dataset$employee_ID)
  #removes rows where age is NA or blank
  new_dataset <- new_dataset[!(is.na(new_dataset$age) | new_dataset$age==""), ]
  
  # Changes Date.of.Birth to age in years & disbursal date to loan age in months
  #solved thanks to Zafer Cesur with a solution for the dates issue -> https://stackoverflow.com/questions/38508886/converting-dates-before-january-1-1970-in-r (accessed 22/11/2019)
  #This could do with patching up a bit, maybe just add the 1900 century if number < current year... Error if the person is old enough
  new_dataset$age <- as.Date(new_dataset$age, format = "%d-%m-%y")
  year(new_dataset$age) <- 1900 + year(new_dataset$age) %% 100
  new_dataset$loan_age_months <- as.Date(new_dataset$loan_age_months, format = "%d-%m-%y")
  
  # function which changes dates to time in years/months, keeps up to date using Sys.Day()
  new_dataset$age <- age_calc((new_dataset$age), enddate = TODAY_DATE, units = "years", precise = FALSE)
  new_dataset$loan_age_months <- age_calc((new_dataset$loan_age_months), enddate = TODAY_DATE, units = "months", precise = FALSE)

  #A dummy column is one which has a value of one when a categorical event occurs and a zero when it doesn't occur
  colnames(new_dataset)[which(names(new_dataset) == "employment")] <- "emp"
  #creates the new dummy variable around the emp column
  dummyVariable <- dummyVars(" ~ emp", data = new_dataset)
  #'predict' creates the 2 column frame within the dataset and assigns the 1 & 0 to the corretc columns using predict()
  new_dataset$emp <- data.frame(predict(dummyVariable, newdata = new_dataset))

  #Change the date fields into number of months
  i <- 1
  length <- nrow(new_dataset)
  while (i < length+1) {
    new_dataset$avg_accnt_age_mon <- as.character(new_dataset$avg_accnt_age_mon)
    num_months <- unlist(strsplit(as.character(new_dataset$avg_accnt_age_mon[i]), split="yrs ", fixed=TRUE))[2]
    num_yrs <- unlist(strsplit(as.character(new_dataset$avg_accnt_age_mon[i]), split="", fixed=TRUE))[1]
    num_months <- unlist(strsplit(as.character(num_months), split="mon"))[1]
    new_dataset$avg_accnt_age_mon[i] <- as.integer(num_months) + as.integer(num_yrs) * 12
    
    new_dataset$credit_hist_len_mon <- as.character(new_dataset$credit_hist_len_mon)
    num_months_1 <- unlist(strsplit(as.character(new_dataset$credit_hist_len_mon[i]), split="yrs ", fixed=TRUE))[2]
    num_yrs_1 <- unlist(strsplit(as.character(new_dataset$credit_hist_len_mon[i]), split="", fixed=TRUE))[1]
    num_months_1 <- unlist(strsplit(as.character(num_months_1), split="mon"))[1]
    new_dataset$credit_hist_len_mon[i] <- as.integer(num_months_1) + as.integer(num_yrs_1) * 12
    
    i <- i + 1
  }
  # Sets these 'ages' to integers
  new_dataset$avg_accnt_age_mon <- as.integer(new_dataset$avg_accnt_age_mon)      
  new_dataset$credit_hist_len_mon <- as.integer(new_dataset$credit_hist_len_mon)

  loan_default1 <- ifelse(new_dataset$loan_default==0,1,0)
  new_dataset <- cbind(new_dataset,loan_default1)
  # Sets 'loan_default1' to a factor with two levels, 1 & 0
  new_dataset$loan_default1 <- as.factor(new_dataset$loan_default1)
  
  #collates all forms of government ID into one column, whether or not an individual has provided any form of gov_id
  new_dataset <- collate_gov(new_dataset)
  #removes columns where there is only one unique value, making this data useless too us
  # by sepcifying that we expect length to be > 1, if it isn't, this will return false
  # Thanks to user 'A5C1D2H2I1M1N2O1R2T1' for this statement: https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
  new_dataset <- new_dataset[vapply(new_dataset, function(x) length(unique(x)) > 1, logical(1L))]
  
  ordinals <- new_dataset[,norm_fields]
  non_ordinals <- new_dataset[ ,!(names(new_dataset) %in% norm_fields)]

  ordinals <- NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)

  new_dataset <- cbind(ordinals, non_ordinals)

  return(new_dataset)
}

#************************************************
# collate_gov() :
#
# INPUT: dataset, specifically using the forms of government ID
# OUTPUT : a single column stating if a row has a governemnt ID included
# ************************************************
collate_gov <- function(dataset) {
  gov_ID <- c('aadhar_flag', 'PAN_flag', 'voter_flag', 'driving_flag', 'passport_flag')
  datasub <- subset(dataset, select=c('aadhar_flag', 'PAN_flag', 'voter_flag', 'driving_flag', 'passport_flag'))
  datasub[,"gov_id"] <- 0
  datasub$gov_id <- as.integer(datasub$aadhar_flag|datasub$PAN_flag|datasub$voter_flag|datasub$driving_flag|datasub$passport_flag)
  datasub <- datasub[, !(names(datasub) %in% gov_ID)]
  new_dataset <- dataset[, !(names(dataset) %in% gov_ID)]
  new_dataset[,"gov_id"] <- 0
  new_dataset$gov_id <- datasub
  return(new_dataset)
}

#************************************************
# machineLearningFunction() :
#
# INPUT: processed dataset
# Calculates the AUC and optimum threshold. o/p an ROC Loan Classifer Model
# ************************************************
classify_model <- function(train_class, clean_data) {
  train_class$loan_default <- as.factor(train_class$loan_default)
 
  pd <- sample(2,nrow(train_class),replace=TRUE,prob=c(0.7,0.3))
  train <- train_class[pd==1,]
  test <- train_class[pd==2,]

  # logistic regression model
  mymodel <- glm(loan_default1~CNS_score+sec_no_accounts+sec_overdue_accounts+sec_current_bal+delinq_accts_six_months+
                   sec_active_accounts+new_accts_in_six_months+loaned_amount+pri_instal_amt+asset_cost+pri_current_bal+
                   new_accts_in_six_months+ltv+emp.empSalaried+emp.empSelf.employed,data=train,family = quasibinomial)
  
  # prediction
  p1 <- predict(mymodel, train, type='response')

  
  threshold_1 <- 0.9
  # misclassification error-train data
  pred1 <- ifelse(p1<threshold_1,0,1)
  tab1 <- table(predicted=pred1,actual=train$loan_default)
  print("Regression model Confusion matrix for train data")
  print(tab1)
 
   # test data prediction
  p2 <- predict(mymodel,test,type="response")
  pred2 <- ifelse(p2<threshold_1,0,1)
  tab2 <- table(predicted=pred2,actual=test$loan_default)
  print("Regression model Confusion matrix for test data")
  print(tab2)
  #prediction_model <- predict(mymodel,train_class,type="response")
  prediction_model <- ifelse( (predict(mymodel,train_class,type="response")) < threshold_1, 0, 1)
  
  profit <- cbind(train_class,prediction_model)
  profit$loan_default<-as.factor(profit$loan_default)
  
  # Plot FPR/TPR through threshold range
  maxYoudan <- myPerformancePlot(probs=p2,testing_data=test)
  # ************************************************
  # ROC graph using a library
  rr<-pROC::roc(response=test[ ,OUTPUT_FIELD], predictor=p2, plot=TRUE,auc=TRUE, auc.polygon=TRUE, percent=TRUE, grid=TRUE,print.auc=TRUE,
                main="ROC Loan Classifier Model", xlab="Specificity (1-FPR) %", ylab="Sensitivity (TPR) %")
  
  # Selects the "best" threshold based on distance
  analysis<-coords(rr, x="best", best.method="closest.topleft", ret=c("threshold", "specificity", "sensitivity"))
  fpr<-round(100.0-analysis["specificity"],digits=2)
  
  # Add crosshairs to the graph
  abline(h=analysis["sensitivity"],col="red",lty=3,lwd=2)
  abline(v=analysis["specificity"],col="red",lty=3,lwd=2)
  
  # Annote with text
  annotate<-paste("Threshold: ",round(analysis["threshold"],digits=4L), " TPR: ",round(analysis["sensitivity"],digits=2L), "% FPR: ",fpr,"%",sep="")
  
  text(x=analysis["specificity"], y=analysis["sensitivity"], adj = c(-0.2,2),cex=1, col="red",annotate)
  # Use the "best" distance threshold to evaluate classifier
  results<-myEvaluateClassifier(probs=p2, testing_data=test, threshold=analysis["threshold"])
  print(results)
  # Use the Youdan threshold to evaluate classifier
  results1<-myEvaluateClassifier(probs=p2, testing_data=test, threshold=maxYoudan)
  
  #IF THERE IS NO CNS SCORE, MUST CHOOSE ANOTHER FIELD
  tree <- ctree(loan_default~CNS_score+sec_no_accounts+sec_overdue_accounts+sec_current_bal+delinq_accts_six_months+
                  sec_active_accounts+new_accts_in_six_months+loaned_amount+pri_instal_amt+asset_cost+pri_current_bal+
                  new_accts_in_six_months+ltv+emp.empSalaried+emp.empSelf.employed,train,controls=ctree_control(mincriterion=0.9, minsplit=50))
  plot(tree)
  # prediction using trees
  pred <- predict(tree,train)
  tab <- table(pred, train$loan_default)
  print("Classification trees Confusion matrix for train data")
  print(tab)
  # predict
  pred1 <- predict(tree,test)
  tab1 <- table(pred1, test$loan_default)
  print("Classification trees Confusion matrix for test data")
  print(tab1)
  #Profit model
  profit<-cbind(train_class, prediction_model)

  total <- sum(profit[which(profit[ ,32] == 1 & profit[ ,34]==1),1])
  negative <- sum(profit[which(profit[,32]>0 & profit[,34]<1),1])
  negative1 <- sum(profit[which(profit[,32]<1 & profit[,34]>0),1])
  positive <- sum(profit[which(profit[,32]==0 & profit[,34]==0),1])
  total_profit <- total-negative-negative1+positive
  print("Total Profit")
  print( total_profit)
 
  write.csv(profit, "profit.csv")
  forest(profit)
}

forest <- function(train_class) {
  train_class$loan_default<-as.factor(train_class$loan_default)
  
  #splitting of dataset
  pd <- sample(2,nrow(train_class),replace=TRUE,prob=c(0.7,0.3))
  train <- train_class[pd==1,]
  test <- train_class[pd==2,]

  rf <- randomForest(loan_default ~ CNS_score+sec_no_accounts+sec_overdue_accounts+sec_current_bal+delinq_accts_six_months+
                       sec_active_accounts+new_accts_in_six_months+loaned_amount+pri_instal_amt+asset_cost+pri_current_bal+
                       new_accts_in_six_months+ltv+emp.empSalaried+emp.empSelf.employed, data = train)
  #View(rf)
  #attributes(rf)
  
  # Prediction & Confusion Matrix - train data
  p1 <- predict(rf, train)
  tab1<-confusionMatrix(p1, train$loan_default)
  print("Random forest Confusion matrix for train data")
  print(tab1)
  # Prediction & Confusion Matrix - test data
  p2 <- predict(rf, test)
 tab2<-confusionMatrix(p2, test$loan_default)
  print("Random forest Confusion matrix for test data")
  print(tab2)
}
#************************************************
# normalise_data() :
#
# INPUT: field in the dataset
# OUTPUT : returns the same field in a normalised state
# ************************************************
normalise_data <- function(data_field, length) {
  # z = (xi - min(x))/(max(x) - min(x))
  min <- min(data_field)
  max <- max(data_field)
  i <- 1
  while (i < length) {
    x <- data_field[i]
    z <- ((x - min)/(max - min))
    data_field[i] <- z
    i <- i + 1
  }
  return(data_field)
}

#************************************************
neural <- function(train_class) {
  train_class$loan_default<-as.factor(train_class$loan_default)
  
  # Min-Max Normalization
  train_class$CNS_score<-normalise_data(train_class$CNS_score)
  train_class$sec_active_accounts<-normalise_data(train_class$sec_active_accounts)
  train_class$new_accts_in_six_months<-normalise_data(train_class$new_accts_in_six_months)
  train_class$loaned_amount<-normalise_data(train_class$loaned_amount)
  train_class$pri_instal_amt<-normalise_data(train_class$pri_instal_amt)
  train_class$asset_cost<-normalise_data(train_class$asset_cost)
    train_class$new_accts_in_six_months<-normalise_data(train_class$new_accts_in_six_months)
  train_class$emp.empSalaried<-normalise_data(train_class$emp.empSalaried)
  train_class$emp.empSelf.employed<-normalise_data(train_class$emp.empSelf.employed)
  train_class$ltv<-normalise_data(train_class$ltv)
  train_class$sec_no_accounts<-normalise_data(train_class$sec_no_accounts)
  train_class$sec_current_bal<-normalise_data(train_class$sec_current_bal)
  train_class$delinq_accts_six_months<-normalise_data(train_class$delinq_accts_six_months)
  train_class$pri_current_bal<-normalise_data(train_class$pri_current_bal)

 
  #splitting of dataset
  pd <- sample(2,nrow(train_class),replace=TRUE,prob=c(0.7,0.3))
  train <- train_class[pd==1,]
  test <- train_class[pd==2,]
  
  Neural_networks<-neuralnet(loan_default ~ CNS_score+sec_no_accounts+sec_overdue_accounts+sec_current_bal+delinq_accts_six_months+
                       sec_active_accounts+new_accts_in_six_months+loaned_amount+pri_instal_amt+asset_cost+pri_current_bal+
                       new_accts_in_six_months+ltv+emp.empSalaried+emp.empSelf.employed,data=train,hidden=5,err.fct="ce",linear.output = FALSE)
  
  plot(Neural_networks)
  # Prediction
  output<-compute(Neural_networks,train[,-1])
  head(output$net.result)
  head(training[1,])
  # Confusion Matrix & Misclassification Error - training data
  output <- compute(Neural_networks,training[,-32])
  p1<-output$net.result
  pred1<-ifelse(p1>0.5, 1, 0)
  tab1<-table(pred1, train$loan_default)
  print("neural networks Confusion matrix for train data")
  print(tab1)
 
  
  # Confusion Matrix & Misclassification Error - testing data
  output <- compute(Neural_networks,test[,-32])
  p2 <- output$net.result
  pred2 <- ifelse(p2>0.5, 1, 0)
  tab2 <- table(pred2, test$loan_default)
  print("neural networks Confusion matrix for test data")
  print(tab2)
  
}


