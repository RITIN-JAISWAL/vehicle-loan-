##############################################
#
# Nick's Functions
#
##############################################


# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}

# ************************************************
# NConvertClass() :
#
# In original dataset, $Status is the classification label
# We need to convert this to give the minority class a value of 0
# this just makes it easiert to define the confusioon matrix!
# for the UCI-G this is a class of {0,1} being {bad, good}
#
# INPUT   :
#             Data Frame        - dataset
#
# OUTPUT  :
#             Data Frame        - dataset
#
# ************************************************
NConvertClass<-function(dataset){
  positionClassOutput<-which(names(dataset)==OUTPUT_FIELD)
  classes<-sort(table(dataset[,positionClassOutput])) #smallest class will be first
  minority<-names(classes[1])
  indexToStatus2<-which(dataset[positionClassOutput]==minority)
  dataset[positionClassOutput][indexToStatus2,]<-0
  dataset[positionClassOutput][-indexToStatus2,]<-1
  return(dataset)
}
# ************************************************
# stratifiedDataset() :
#
# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){
  
  positionClassOutput=which(names(originalDataset)==OUTPUT_FIELD)
  
  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset<-rbind(split1,split2)
  
  #Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]
  
  return(newDataset)
}

# ************************************************
# allocateFoldID() :
#
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
allocateFoldID<-function(dataset){
  recordsPerFold<-ceiling(nrow(dataset)/KFOLDS)
  
  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(dataset)]
  
  dataset$foldId<-foldIds
  
  return(dataset)
}
# ************************************************
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
runExperiment<-function(dataset,FUN, ...){
  
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    splitData<-stratifiedSplit(newDataset=dataset,fold=k)
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=(k==KFOLDS),...)
    
    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()
  
  # Return the means from all the experiments back as a list
  getMeans<-colMeans(allResults)
  getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
  
  return(as.list(colMeans(allResults)))
}

NplotOutliers<-function(sorted,outliers,fieldName){
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}
Nrescaleentireframe<-function(dataset){
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}

myPerformancePlot<-function(probs, testing_data) {
  toPlot <- data.frame()
  
  for(threshold in seq(0,1,by=0.01)){
    results <- myEvaluateClassifier(probs, test=testing_data, threshold=threshold)
    toPlot <- rbind(toPlot,data.frame(x=threshold, fpr=results$FPR, tpr=results$TPR))
  }
  
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  # Plot threshold graph
  plot(toPlot$x,toPlot$tpr,
       type="l",lwd=3, col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main="Threshold Perfomance Loan Classifier Model")
  
  # Plot the specificity (1-FPR)
  lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
  
  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  abline(v=crosspoint,col="red",lty=3,lwd=2)
  
  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
  mindist<-toPlot$x[which(toPlot$distance==min(toPlot$distance))]
  abline(v=mindist,col="green",lty=3,lwd=2)
  
  # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
  lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
  indexToBest<-which(toPlot$youdan==max(toPlot$youdan))
  maxYoudan<-toPlot$x[indexToBest]
  abline(v=maxYoudan,col="purple",lty=3,lwd=2)
  
  legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nDistance=",mindist,"\nYoudan=",maxYoudan))
  
  return(maxYoudan)
} 

myEvaluateClassifier<-function(probs=p2,testing_data=test,threshold) {
  
  predictedClass<-ifelse(probs < threshold,0,1)
  expectedClass<-testing_data[ ,OUTPUT_FIELD]
  results <- NcalcConfusion(expectedClass = expectedClass,predictedClass = predictedClass)
  
  return(results)
} 

NcalcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # This "converts" the above into our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
}
NcalcMeasures<-function(TP,FN,FP,TN){
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}
# ************************************************
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(newDataset,fold){
  
  test<-subset(newDataset, subset= foldId==fold, select=-foldId)
  train<-subset(newDataset, subset= foldId!=fold,select=-foldId)
  
  return(list(
    train=train,
    test=test))
}
# ************************************************
# simpleDT() :
#
# Create C5 Decision Tree on the raw dataset
# A decision tree may not need the dataset to be pre-processed
#
# INPUT   :
#             Data Frame     - train       - original train dataset
#             Data Frame     - test        - original test dataset
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
simpleDT<-function(train,test,plot=TRUE){
  
  # In original dataset, $Status is the classification label
  # We need to convert this to give the minority class a value of 0
  # this just makes it easiert to define the confusioon matrix!
  # for the UCI-G this is a class of {0,1} being {bad, good}
  train<-NConvertClass(train)
  test<-NConvertClass(test)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  tree<-C50::C5.0(x=train[-positionClassOutput],
                  y=factor(train[,positionClassOutput]),
                  rules=TRUE,
                  trials=1)
  
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title="Original Dataset. DT C5.0")
  
  return(measures)
}
# ************************************************
# getTreeClassifications() :
#
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myTree        - tree
#         :   Data Frame     - testDataset - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   int            - classLabel   - lable given to the positive (TRUE) class
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(results=measures,title=title)
  
  return(measures)
} #endof getTreeClassifications()
# ************************************************
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#         :   boolean        - plot         - TRUE=create charts otherwise don't
#         :   string         - title        - string to plot as the chart title
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# Uses   library(pROC)
# 241019NRT - added plot flag and title for charts
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  
  # Helper local scope function
  getFirst<-function(values){
    if (length(values)>1){
      return(values[1])
    } else
      return(values)
  }
  
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-myEvaluateClassifier(test_predicted,
                                 test_expected,
                                 threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  indexToBest<-getFirst(which(toPlot$youdan==max(toPlot$youdan)))
  maxYoudan<-toPlot$x[indexToBest]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  #241019 select just the first min distance, as might be more
  mindist<-getFirst(toPlot$x[which(toPlot$distance==min(toPlot$distance))])
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if (crosspoint<1)
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more
    abline(v=mindist,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),
           col=c("blue","red","green","purple"),
           lty=c(1,1,3,3),
           lwd=2)
    
    text(x=0,y=50, adj = c(-0.2,2), cex=1, col="black",
         paste("THRESHOLDS:\nDistance=", mindist, "\nYoudan=",maxYoudan))
    
    # ************************************************
    # ROC graph using a library
    
    rr<-pROC::roc(response=test_expected,
                  predictor=test_predicted,
                  plot=TRUE,
                  auc=TRUE,
                  auc.polygon=TRUE,
                  percent=TRUE,
                  grid=TRUE,
                  print.auc=TRUE,
                  main=paste("ROC for Classifier Model",title),
                  xlab="Specificity (1-FPR) %",
                  ylab="Sensitivity (TPR) %")
    
    # Selects the "best" threshold based on distance
    analysis<-coords(rr, x="best",transpose = FALSE,
                     best.method="closest.topleft",
                     ret=c("threshold",
                           "specificity",
                           "sensitivity"))
    
    fpr<-round(100.0-analysis["specificity"],digits=2)
    
    #Add crosshairs to the graph
    abline(h=analysis["sensitivity"],col="red",lty=3,lwd=2)
    abline(v=analysis["specificity"],col="red",lty=3,lwd=2)
    
    #Annote with text
    annotate<-paste("Threshold: ",round(analysis["threshold"],digits=4L),
                    " TPR: ",round(analysis["sensitivity"],digits=2L),
                    "% FPR: ",fpr,"%",sep="")
    
    text(x=analysis["specificity"],
         y=analysis["sensitivity"], adj = c(-0.2,2),cex=1,
         col="red",annotate)
    
  } # endof if plotting
  
  # Select the threshold - I have choosen distance
  
  myThreshold<-mindist      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-myEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  results$threshold<-myThreshold
  
  return(results)
} 
