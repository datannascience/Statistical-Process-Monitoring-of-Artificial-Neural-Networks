#**********************************************************************************************************************************************#
#***************** This code summarizes the steps to derive the results presented in Table 1, Experiment 1 of the manuscript.******************#
#***************Control charts based on Projection depth, Algorithm "Coordinate Descent" as well as the False Alarm Rate (FAR)*****************#
#**************in Phase I, Signal Rate (SR) in Phase II, in-control part, and Correct Detection Rate (CDR) in Phase II,************************#
#****************************************************out-of-control part are computed.*********************************************************#
#**********************************************************************************************************************************************#


# Load the input depths and required labels
load("Input_ProjectionDepthCoordDesc.RData")

# Calculate the FAR for reference samples in Experiment 3: CIFAR-10 Data: Phase I (Reference samples)

# Create containers to store the data
alpha <- 0.05
FAR_CDR_refsamples_size2000 <- list()
FAR_CDR_refsamples_size3000 <- list()
FAR_CDR_refsamples_size4000 <- list()

## Size 2000

for (i in 1:10){
  
  depths_current_class <- output_refsample_size2000[[i]][["coorddescGC_results"]] # Take the relevant reference sample
  ranks_current_class <- c()  # Container for the ranks used for the control chart
  
  FAR_currentsample <- -1
  CDR_currentsample <- -1
  
  
  for (k in 1:length(depths_current_class)){
    ranks_current_class[k] <- sum(depths_current_class <= depths_current_class[k]) / length(depths_current_class)  # Normalized ranks
  }
  
  FAR_currentsample <- length(which(ranks_current_class <= alpha))/length(ranks_current_class)  # Compute FAR by checking how many points obtained the rank
  # <= 0.05 (alpha) and normalize by the total number of points
  CDR_currentsample <- 1 - FAR_currentsample # Correct Detection Rate (CDR) is Rate showing how many points are correctly recognized as being in-control
  
  FAR_CDR_refsamples_size2000[[i]] <- list(FAR = FAR_currentsample, CDR = CDR_currentsample)  # Create a list with the results for 10 classes
  
}


FAR_refsamples_size2000_mean <- round(mean(sapply(FAR_CDR_refsamples_size2000,"[[",1)), 2)


## Size 3000


refsample_rcontrolchart_size3000 <- list() # Container for the created control charts

for (i in 1:10){
  
  depths_current_class <- output_refsample_size3000[[i]][["coorddescGC_results"]]  # Take the relevant reference sample
  ranks_current_class <- c() # Container for the ranks used for the control chart
  
  FAR_currentsample <- -1
  CDR_currentsample <- -1
  
  
  for (k in 1:length(depths_current_class)){
    ranks_current_class[k] <- sum(depths_current_class <= depths_current_class[k]) / length(depths_current_class)   # Normalized ranks
  }
  
  FAR_currentsample <- length(which(ranks_current_class <= alpha))/length(ranks_current_class)   # Compute FAR by checking how many points obtained the rank
  # <= 0.05 (alpha) and normalize by the total number of points
  CDR_currentsample <- 1 - FAR_currentsample # Correct Detection Rate (CDR) is Rate showing how many points are correctly recognized as being in-control
  
  
  FAR_CDR_refsamples_size3000[[i]] <- list(FAR = FAR_currentsample, CDR = CDR_currentsample)   # Create a list with the results for 10 classes
  refsample_rcontrolchart_size3000[[i]] <- ranks_current_class # As an example: Collect ranks used for the control charts
}

FAR_refsamples_size3000_mean <- round(mean(sapply(FAR_CDR_refsamples_size3000,"[[",1)), 2)


## Size 4000

for (i in 1:10){
  
  depths_current_class <- output_refsample_size4000[[i]][["coorddescGC_results"]]   # Take the relevant reference sample
  ranks_current_class <- c() # Container for the ranks used for the control chart
  
  FAR_currentsample <- -1
  CDR_currentsample <- -1
  
  
  for (k in 1:length(depths_current_class)){
    ranks_current_class[k] <- sum(depths_current_class <= depths_current_class[k]) / length(depths_current_class)    # Normalized ranks
  }
  
  FAR_currentsample <- length(which(ranks_current_class <= alpha))/length(ranks_current_class)   # Compute FAR by checking how many points obtained the rank
  # <= 0.05 (alpha) and normalize by the total number of points
  CDR_currentsample <- 1 - FAR_currentsample  # Correct Detection Rate (CDR) is Rate showing how many points are correctly recognized as being in-control
  
  
  FAR_CDR_refsamples_size4000[[i]] <- list(FAR = FAR_currentsample, CDR = CDR_currentsample)    # Create a list with the results for 10 classes
  
}


FAR_refsamples_size4000_mean <- round(mean(sapply(FAR_CDR_refsamples_size4000,"[[",1)), 2)


# Test data: Phase II, in-control
## Size 2000
testdata_Size2000_controlchartresults_pred <- list()

ranks_testdata_pred_size2000 <- c()


alpha <- 0.05

#df_test_labels: V11 - predictions, V12 - true labels.

for (i in 1:10000){
  class_pred <- df_test_labels$V11[i] + 1  # Obtain the predicted class
  
  
  ref_depths_pred <- as.numeric(output_refsample_size2000[[class_pred]][[2]]) # the reference sample of the predicted class
  # Remark: Here, the index [[2]] leads to the depths and [[1]] stores the running times 
  ranks_testdata_pred_size2000[i] <- sum(ref_depths_pred <= coorddescGCdepth_results_pred_size2000[i]) / length(ref_depths_pred)
  
}

SR_testdata_pred_size2000 <- length(which(ranks_testdata_pred_size2000 <= alpha))/length(ranks_testdata_pred_size2000)  # Normalized ranks 
CDR_testdata_pred_size2000 <- 1 - SR_testdata_pred_size2000  # Compute SR by checking how many points obtained the rank
# <= 0.05 (alpha) and normalize by the total number of points


# Weighted computation of the signal rate for Phase II, in-control part

labels_pred <- as.numeric(df_test_labels$V11)  # Obtain the predicted classes


labels_pred_allclasses <- list()
for (i in 1:10){
  
  labels_pred_allclasses[[i]] <- c(which(labels_pred == (i-1)))  # Obtain indices of the test data points which were predicted to be a part of a class "i-1"
  
}

fractions_pred_allclasses <- c()
for (i in 1:10){
  fractions_pred_allclasses[i] <- length(labels_pred_allclasses[[i]])/10000  # Obtain the weight of each of the class
}

ranks_pred_allclasses <- list()
for (i in 1:10){
  ranks_pred_allclasses[[i]] <- ranks_testdata_pred_size2000[labels_pred_allclasses[[i]]]  # Extract the rank results for each class
}


SR_pred_allclasses <- c()
for (i in 1:10){
  SR_pred_allclasses[i] <- length(which(ranks_pred_allclasses[[i]]<=0.05))/length(ranks_pred_allclasses[[i]])  # Compute SR of each class
}

SR_pred_testdata_weighted_size2000 <- sum(SR_pred_allclasses*fractions_pred_allclasses) # Compute the weighted version 

## Size 3000
testdata_Size3000_controlchartresults_pred <- list()

ranks_testdata_pred_size3000 <- c()


alpha <- 0.05

#df_test_labels: V11 - predictions, V12 - true labels.

for (i in 1:10000){
  class_pred <- df_test_labels$V11[i] + 1  # Obtain the predicted class
  
  
  ref_depths_pred <- as.numeric(output_refsample_size3000[[class_pred]][[2]]) # the reference sample of the predicted class
  
  ranks_testdata_pred_size3000[i] <- sum(ref_depths_pred <= coorddescGCdepth_results_pred_size3000[i]) / length(ref_depths_pred) # Normalized ranks 
  
}

SR_testdata_pred_size3000 <- length(which(ranks_testdata_pred_size3000 <= alpha))/length(ranks_testdata_pred_size3000)  # Compute SR by checking how many points obtained the rank
# <= 0.05 (alpha) and normalize by the total number of points 
CDR_testdata_pred_size3000 <- 1 - SR_testdata_pred_size3000  


# Weighted computation of the signal rate for Phase II, in-control part

labels_pred <- as.numeric(df_test_labels$V11)  # Obtain the predicted classes


labels_pred_allclasses <- list()
for (i in 1:10){
  
  labels_pred_allclasses[[i]] <- c(which(labels_pred == (i-1)))  # Obtain indices of the test data points which were predicted to be a part of a class "i-1"
  
}

fractions_pred_allclasses <- c()
for (i in 1:10){
  fractions_pred_allclasses[i] <- length(labels_pred_allclasses[[i]])/10000  # Obtain the weight of each of the class
}

ranks_pred_allclasses <- list()
for (i in 1:10){
  ranks_pred_allclasses[[i]] <- ranks_testdata_pred_size3000[labels_pred_allclasses[[i]]]  # Extract the rank results for each class
}


SR_pred_allclasses <- c()
for (i in 1:10){
  SR_pred_allclasses[i] <- length(which(ranks_pred_allclasses[[i]]<=0.05))/length(ranks_pred_allclasses[[i]])  # Compute SR of each class
}

SR_pred_testdata_weighted_size3000 <- sum(SR_pred_allclasses*fractions_pred_allclasses) # Compute the weighted version 


## Size 4000
testdata_Size4000_controlchartresults_pred <- list()

ranks_testdata_pred_size4000 <- c()


alpha <- 0.05

#df_test_labels: V11 - predictions, V12 - true labels.

for (i in 1:10000){
  class_pred <- df_test_labels$V11[i] + 1  # Obtain the predicted class
  
  
  ref_depths_pred <- as.numeric(output_refsample_size4000[[class_pred]][[2]]) # the reference sample of the predicted class
  
  ranks_testdata_pred_size4000[i] <- sum(ref_depths_pred <= coorddescGCdepth_results_pred_size4000[i]) / length(ref_depths_pred)  # Normalized ranks 
  
}

SR_testdata_pred_size4000 <- length(which(ranks_testdata_pred_size4000 <= alpha))/length(ranks_testdata_pred_size4000)  # Compute SR by checking how many points obtained the rank
# <= 0.05 (alpha) and normalize by the total number of points
CDR_testdata_pred_size4000 <- 1 - SR_testdata_pred_size4000 


# Weighted computation of the signal rate for Phase II, in-control part

labels_pred <- as.numeric(df_test_labels$V11)  # Obtain the predicted classes


labels_pred_allclasses <- list()
for (i in 1:10){
  
  labels_pred_allclasses[[i]] <- c(which(labels_pred == (i-1)))  # Obtain indices of the test data points which were predicted to be a part of a class "i-1"
  
}

fractions_pred_allclasses <- c()
for (i in 1:10){
  fractions_pred_allclasses[i] <- length(labels_pred_allclasses[[i]])/10000  # Obtain the weight of each of the class
}

ranks_pred_allclasses <- list()
for (i in 1:10){
  ranks_pred_allclasses[[i]] <- ranks_testdata_pred_size4000[labels_pred_allclasses[[i]]]  # Extract the rank results for each class
}


SR_pred_allclasses <- c()
for (i in 1:10){
  SR_pred_allclasses[i] <- length(which(ranks_pred_allclasses[[i]]<=0.05))/length(ranks_pred_allclasses[[i]])  # Compute SR of each class
}

SR_pred_testdata_weighted_size4000 <- sum(SR_pred_allclasses*fractions_pred_allclasses)  # Compute the weighted version 

# Out-of-control data

## Size 2000
ranks_outofcontrol <- c()
for (i in 1:400){
  class_outofcontrol_pred <- OutofcontrolData_predlabels[i] + 1 # Obtain the predicted class
  ref_depths_outofcontrol <- as.numeric(output_refsample_size2000[[class_outofcontrol_pred]][[2]]) # Take the relevant reference sample
  ranks_outofcontrol[i] <- sum(ref_depths_outofcontrol <= OutofcontrolData_depths_size2000[i]) / length(ref_depths_outofcontrol) # Normalized ranks 
}  
SR_outofcontrol_size2000 <- length(which(ranks_outofcontrol > alpha))/length(ranks_outofcontrol)  # Compute SR by checking how many points obtained the rank
# > 0.05 (alpha) and normalize by the total number of points
CDR_outofcontrol_size2000 <- 1 - SR_outofcontrol_size2000  # Correct Detection Rate (CDR) is Rate showing how many points are correctly recognized as being out-of-control

# Weighted version of CDR

labels_outofcontrol_allclasses <- list()
for (i in 1:10){
  
  labels_outofcontrol_allclasses[[i]] <- c(which(OutofcontrolData_predlabels == (i-1)))  # Obtain indices of the out-of-control data points which were predicted to be a part of a class "i-1"
  
}


fractions_outofcontrol_allclasses <- c()
for (i in 1:10){
  fractions_outofcontrol_allclasses[i] <- length(labels_outofcontrol_allclasses[[i]])/length(OutofcontrolData_predlabels)   # Obtain the weight of each of the class
}

ranks_outofcontrol_allclasses <- list()
for (i in 1:10){
  ranks_outofcontrol_allclasses[[i]] <- ranks_outofcontrol[labels_outofcontrol_allclasses[[i]]]  # Extract the rank results for each class
}


CDR_outofcontrol_allclasses <- c()
for (i in 1:10){
  CDR_outofcontrol_allclasses[i] <- length(which(ranks_outofcontrol_allclasses[[i]]<=0.05))/length(ranks_outofcontrol_allclasses[[i]])   # Compute CDR of each class
}

CDR_outofcontrol_testdata_weighted_size2000 <- sum(CDR_outofcontrol_allclasses*fractions_outofcontrol_allclasses)    # Compute the weighted version 


## Size 3000
ranks_outofcontrol <- c()
for (i in 1:400){
  class_outofcontrol_pred <- OutofcontrolData_predlabels[i] + 1 # Obtain the predicted class
  ref_depths_outofcontrol <- as.numeric(output_refsample_size3000[[class_outofcontrol_pred]][[2]]) # Take the relevant reference sample
  ranks_outofcontrol[i] <- sum(ref_depths_outofcontrol <= OutofcontrolData_depths_size3000[i]) / length(ref_depths_outofcontrol) # Normalized ranks 
}  
SR_outofcontrol_size3000 <- length(which(ranks_outofcontrol > alpha))/length(ranks_outofcontrol)  # Compute SR by checking how many points obtained the rank
# > 0.05 (alpha) and normalize by the total number of points
CDR_outofcontrol_size3000 <- 1 - SR_outofcontrol_size3000  # Correct Detection Rate (CDR) is Rate showing how many points are correctly recognized as being out-of-control


# Weighted version of CDR

labels_outofcontrol_allclasses <- list()
for (i in 1:10){
  
  labels_outofcontrol_allclasses[[i]] <- c(which(OutofcontrolData_predlabels == (i-1)))  # Obtain indices of the out-of-control data points which were predicted to be a part of a class "i-1"
  
}


fractions_outofcontrol_allclasses <- c()
for (i in 1:10){
  fractions_outofcontrol_allclasses[i] <- length(labels_outofcontrol_allclasses[[i]])/length(OutofcontrolData_predlabels)   # Obtain the weight of each of the class
}

ranks_outofcontrol_allclasses <- list()
for (i in 1:10){
  ranks_outofcontrol_allclasses[[i]] <- ranks_outofcontrol[labels_outofcontrol_allclasses[[i]]]  # Extract the rank results for each class
}


CDR_outofcontrol_allclasses <- c()
for (i in 1:10){
  CDR_outofcontrol_allclasses[i] <- length(which(ranks_outofcontrol_allclasses[[i]]<=0.05))/length(ranks_outofcontrol_allclasses[[i]])   # Compute CDR of each class
}

CDR_outofcontrol_testdata_weighted_size3000 <- sum(CDR_outofcontrol_allclasses*fractions_outofcontrol_allclasses)    # Compute the weighted version 

## Size 4000
ranks_outofcontrol <- c()
for (i in 1:400){
  class_outofcontrol_pred <- OutofcontrolData_predlabels[i] + 1 # Obtain the predicted class
  ref_depths_outofcontrol <- as.numeric(output_refsample_size4000[[class_outofcontrol_pred]][[2]]) # Take the relevant reference sample
  ranks_outofcontrol[i] <- sum(ref_depths_outofcontrol <= OutofcontrolData_depths_size4000[i]) / length(ref_depths_outofcontrol) # Normalized ranks 
}  
SR_outofcontrol_size4000 <- length(which(ranks_outofcontrol > alpha))/length(ranks_outofcontrol)  # Compute SR by checking how many points obtained the rank
# > 0.05 (alpha) and normalize by the total number of points
CDR_outofcontrol_size4000 <- 1 - SR_outofcontrol_size4000  # Correct Detection Rate (CDR) is Rate showing how many points are correctly recognized as being out-of-control


# Weighted version of CDR

labels_outofcontrol_allclasses <- list()
for (i in 1:10){
  
  labels_outofcontrol_allclasses[[i]] <- c(which(OutofcontrolData_predlabels == (i-1)))  # Obtain indices of the out-of-control data points which were predicted to be a part of a class "i-1"
  
}


fractions_outofcontrol_allclasses <- c()
for (i in 1:10){
  fractions_outofcontrol_allclasses[i] <- length(labels_outofcontrol_allclasses[[i]])/length(OutofcontrolData_predlabels)   # Obtain the weight of each of the class
}

ranks_outofcontrol_allclasses <- list()
for (i in 1:10){
  ranks_outofcontrol_allclasses[[i]] <- ranks_outofcontrol[labels_outofcontrol_allclasses[[i]]]  # Extract the rank results for each class
}


CDR_outofcontrol_allclasses <- c()
for (i in 1:10){
  CDR_outofcontrol_allclasses[i] <- length(which(ranks_outofcontrol_allclasses[[i]]<=0.05))/length(ranks_outofcontrol_allclasses[[i]])   # Compute CDR of each class
}

CDR_outofcontrol_testdata_weighted_size4000 <- sum(CDR_outofcontrol_allclasses*fractions_outofcontrol_allclasses)    # Compute the weighted version 



# SUMMARY OF THE RESULTS

PD_1_size2000 <- as.array(c(FAR_refsamples_size2000_mean, round(SR_pred_testdata_weighted_size2000, 2), round(CDR_outofcontrol_testdata_weighted_size2000, 2)))
PD_1_size3000 <- as.array(c(FAR_refsamples_size3000_mean, round(SR_pred_testdata_weighted_size3000, 2), round(CDR_outofcontrol_testdata_weighted_size3000, 2)))
PD_1_size4000 <- as.array(c(FAR_refsamples_size4000_mean, round(SR_pred_testdata_weighted_size4000, 2), round(CDR_outofcontrol_testdata_weighted_size4000, 2)))



