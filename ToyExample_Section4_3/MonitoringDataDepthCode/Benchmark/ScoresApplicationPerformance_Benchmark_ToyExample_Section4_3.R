# Benchmark: Perform monitoring using alternative algorithms on the softmax scores, Toy Example, Section 4.3

---------------------------###Apply Mahalanobis Distance (univariate case) and NOF algorithm on softmax output as monitoring procedure###---------------------------------

# Data preparation

# EXPLANATION: train_labels_scores_toyexample - column 1: Labels, column 2: Binary predictions, column 3: Score predictions

# Reference samples
sum(which(train_labels_scores_toyexample[,1] != train_labels_scores_toyexample[,2])) #There are no misclassified samples
refsample_softmax_class0 <- train_labels_scores_toyexample[which(train_labels_scores_toyexample[,2] == 0), 3]

refsample_softmax_class1 <- train_labels_scores_toyexample[which(train_labels_scores_toyexample[,2] == 1), 3]

# Testdata

# Separate into classes
z_test_softmax_class0 <- test_labels_scores_toyexample[which(test_labels_scores_toyexample[,2] == 0), 3]

z_test_softmax_class1 <- test_labels_scores_toyexample[which(test_labels_scores_toyexample[,2] == 1), 3]


# Outofcontrol data
outofcontrol_labels <- outofcontrol_labels_scores_toyexample[,2] 
unique(outofcontrol_labels) # All 1

outofcontrol_softmax <- outofcontrol_labels_scores_toyexample[,3] 


# Outlierness calculation: NOF 

#----------------------------------------Neighborhood Outlier Factor (univariate case)------------------------------------------------#
# Compute outlierness within the reference samples (take all data as a reference sample, basically considering case3)
outlierness_refsample_class0_softmax_NOF <- NOF(refsample_softmax_class0)$NOF
outlierness_refsample_class1_softmax_NOF <- NOF(refsample_softmax_class1)$NOF

# Test data

#The score is normalized between 0 and 1, such that observation with 1 has the lowest density estimation
# and greatest outlierness.
outlierness_testdata_class0_NOF <- c()

for (i in 1:length(z_test_softmax_class0)){
  outlierness_testdata_class0_NOF[i] <- tail(NOF(c(refsample_softmax_class0, z_test_softmax_class0[i]))$NOF, n=1)
  
}

outlierness_testdata_class1_NOF <- c()

for (i in 1:length(z_test_softmax_class1)){
  outlierness_testdata_class1_NOF[i] <- tail(NOF(c(refsample_softmax_class1, z_test_softmax_class1[i]))$NOF, n=1)
  
}


# Outofcontrol data 

unique(outofcontrol_labels) # All 1
outlierness_outofcontroldata_NOF <- c()

for (i in 1:length(outofcontrol_softmax)){
  outlierness_outofcontroldata_NOF[i] <- tail(NOF(c(refsample_softmax_class1, outofcontrol_softmax[i]))$NOF, n=1)
  
}



# Outlierness calculation: MDis

#--------------------------------------------Mahalanobis Distance (univariate case)---------------------------------------------------#

refsample_class0_meansigma <- c(mean(refsample_softmax_class0), sd(refsample_softmax_class0))
refsample_class1_meansigma <- c(mean(refsample_softmax_class1), sd(refsample_softmax_class1))

# Reference samples

outlierness_refsample_class0_softmax_MDis <- abs((refsample_softmax_class0 - refsample_class0_meansigma[1])/refsample_class0_meansigma[2])
outlierness_refsample_class1_softmax_MDis <- abs((refsample_softmax_class1 - refsample_class1_meansigma[1])/refsample_class1_meansigma[2])


# Testdata

outlierness_testdata_class0_MDis <- c()

for (i in 1:length(z_test_softmax_class0)){
  outlierness_testdata_class0_MDis[i] <- abs((z_test_softmax_class0[i] - refsample_class0_meansigma[1])/refsample_class0_meansigma[2])
  
}

outlierness_testdata_class1_MDis <- c()

for (i in 1:length(z_test_softmax_class1)){
  outlierness_testdata_class1_MDis[i] <- abs((z_test_softmax_class1[i] - refsample_class1_meansigma[1])/refsample_class1_meansigma[2])
  
}


# Outofcontrol data

outlierness_outofcontroldata_MDis <- c()

for (i in 1:length(outofcontrol_softmax)){
  outlierness_outofcontroldata_MDis[i] <- abs((outofcontrol_softmax[i] - refsample_class1_meansigma[1])/refsample_class1_meansigma[2])
  
}

#--------------------------------------------------------RUN THE MONITORING----------------------------------------------------------#

OutliernessDetection <- function(scores, ref_scores, proportion, outofcontrol = FALSE){ 
  # Create containers
  ranks <- c()
  FAR <- -1
  CDR <- -1
  m <- length(ref_scores)
  # The smaller the value is, the smaller is outlierness
  for(l in 1:length(scores)){
    ranks[l] <- sum(ref_scores >= scores[l]) / m
  }
  if (outofcontrol == TRUE){
    # Different definition of correctly and wrongly detected samples
    # for the out-of-control state 
    FAR <- length(which(ranks > proportion))/length(ranks)
    CDR <- 1 - FAR
  } else {
    # For the in-control data
    FAR <- length(which(ranks <= proportion))/length(ranks)
    CDR <- 1 - FAR
  }
  
  # Return results 
  
  outlierness_results <- list(ranks = ranks,
                              FAR = FAR,
                              CDR = CDR)
  
  return(outlierness_results)
}



#----------------------------------------------------- Outlierness detection: Mahalanobis Distance (univariate case) -----------------------------------------------------#


# Refsamples

refsample_outliernessdetection_class0_MDis <- list()

refsample_outliernessdetection_class0_MDis <- OutliernessDetection(outlierness_refsample_class0_softmax_MDis, outlierness_refsample_class0_softmax_MDis, proportion = 0.05, outofcontrol = FALSE)



refsample_outliernessdetection_class1_MDis <- list()

refsample_outliernessdetection_class1_MDis <- OutliernessDetection(outlierness_refsample_class1_softmax_MDis, outlierness_refsample_class1_softmax_MDis, proportion = 0.05, outofcontrol = FALSE)

# Testdata

testdata_outliernessdetection_class0_MDis <- list()

testdata_outliernessdetection_class0_MDis <- OutliernessDetection(outlierness_testdata_class0_MDis, outlierness_refsample_class0_softmax_MDis, proportion = 0.05, outofcontrol = FALSE)


testdata_outliernessdetection_class1_MDis <- list()

testdata_outliernessdetection_class1_MDis <- OutliernessDetection(outlierness_testdata_class1_MDis, outlierness_refsample_class1_softmax_MDis, proportion = 0.05, outofcontrol = FALSE)


# Outofcontrol data

outofcontrol_outliernessdetection_MDis <- list()
outofcontrol_outliernessdetection_MDis <- OutliernessDetection(outlierness_outofcontroldata_MDis, outlierness_refsample_class1_softmax_MDis, proportion = 0.05, outofcontrol = TRUE)             



#--------------------------------------------- Outlierness detection: Neighborhood Outlier Factor (univariate case) ----------------------------------------------#


# Refsamples

refsample_outliernessdetection_class0_NOF <- list()

refsample_outliernessdetection_class0_NOF <- OutliernessDetection(outlierness_refsample_class0_softmax_NOF, outlierness_refsample_class0_softmax_NOF, proportion = 0.05, outofcontrol = FALSE)



refsample_outliernessdetection_class1_NOF <- list()

refsample_outliernessdetection_class1_NOF <- OutliernessDetection(outlierness_refsample_class1_softmax_NOF, outlierness_refsample_class1_softmax_NOF, proportion = 0.05, outofcontrol = FALSE)

# Testdata

testdata_outliernessdetection_class0_NOF <- list()

testdata_outliernessdetection_class0_NOF <- OutliernessDetection(outlierness_testdata_class0_NOF, outlierness_refsample_class0_softmax_NOF, proportion = 0.05, outofcontrol = FALSE)


testdata_outliernessdetection_class1_NOF <- list()

testdata_outliernessdetection_class1_NOF <- OutliernessDetection(outlierness_testdata_class1_NOF, outlierness_refsample_class1_softmax_NOF, proportion = 0.05, outofcontrol = FALSE)


# Outofcontrol data

outofcontrol_outliernessdetection_NOF <- list()
outofcontrol_outliernessdetection_NOF <- OutliernessDetection(outlierness_outofcontroldata_NOF, outlierness_refsample_class1_softmax_NOF, proportion = 0.05, outofcontrol = TRUE)             


# SUMMARY: FAR, SR and CDR metrics

fraction_testdata_class0 <- length(z_test_softmax_class0)/dim(test_labels_scores_toyexample)[1]
fraction_testdata_class1 <- length(z_test_softmax_class1)/dim(test_labels_scores_toyexample)[1]

## MDis
MDis_FAR_toyexample <- (refsample_outliernessdetection_class0_MDis$FAR + refsample_outliernessdetection_class1_MDis$FAR)/2
MDis_SR_toyexample <- testdata_outliernessdetection_class0_MDis$FAR*fraction_testdata_class0 + testdata_outliernessdetection_class1_MDis$FAR*fraction_testdata_class1
MDis_CDR_toyexample <- outofcontrol_outliernessdetection_MDis$CDR

## NOF
NOF_FAR_toyexample <- (refsample_outliernessdetection_class0_NOF$FAR + refsample_outliernessdetection_class1_NOF$FAR)/2
NOF_SR_toyexample <- testdata_outliernessdetection_class0_NOF$FAR*fraction_testdata_class0 + testdata_outliernessdetection_class1_NOF$FAR*fraction_testdata_class1
NOF_CDR_toyexample <- outofcontrol_outliernessdetection_NOF$CDR
