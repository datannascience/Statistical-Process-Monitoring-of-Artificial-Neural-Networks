#**********************************************************************************************************************************************#
#***************** This code summarizes the steps to derive the results presented in Table 1, Experiment 1 of the manuscript.******************#
#*************Control charts based on Asymmetric Projection depth, Algorithm "CoordinateDescent" as well as the False Alarm Rate (FAR)*********#
#**************in Phase I, Signal Rate (SR) in Phase II, in-control part, and Correct Detection Rate (CDR) in Phase II,************************#
#****************************************************out-of-control part are computed.*********************************************************#
#**********************************************************************************************************************************************#

# Load the input depths and required labels
load("Input_ProjectionDepthCoordinateDescentAsymmetric.RData")

# Function for r Control Chart
rControlChart <- function(depths, ref_depths, alpha, outofcontrol = FALSE){ 
  # Create containers
  ranks <- c()
  FAR <- -1
  CDR <- -1
  m <- length(ref_depths)
  
  for(l in 1:length(depths)){
    ranks[l] <- sum(ref_depths <= depths[l]) / m
  }
  if (outofcontrol == TRUE){
    # Different definition of correctly and wrongly detected samples
    # for the out-of-control state 
    FAR <- length(which(ranks > alpha))/length(ranks)
    CDR <- 1 - FAR
  } else {
    # For the in-control data
    FAR <- length(which(ranks <= alpha))/length(ranks)
    CDR <- 1 - FAR
  }
  
  # Return results 
  
  r_results <- list(ranks = ranks,
                    FAR = FAR,
                    CDR = CDR)
  
  return(r_results)
}


# Create containers 

refsample_rcontrolchart_asym_CoordDescGC_size2000 <- list()
refsample_rcontrolchart_asym_CoordDescGC_size3000 <- list()
refsample_rcontrolchart_asym_CoordDescGC_size4000 <- list()

# Apply r Control Charts on Phase I data to compute FAR

for (i in 1:10){ 
  
  refsample_rcontrolchart_asym_CoordDescGC_size2000[[i]] <- rControlChart(depths = results_asymprojdepth_size2000_experiment1[[i]][[1]][["asym_CoordDescGC"]], 
                                                                           ref_depths = results_asymprojdepth_size2000_experiment1[[i]][[1]][["asym_CoordDescGC"]],
                                                                           alpha = 0.05,  outofcontrol = FALSE)
  
  
  refsample_rcontrolchart_asym_CoordDescGC_size3000[[i]] <- rControlChart(depths = results_asymprojdepth_size3000_experiment1[[i]][[1]][["asym_CoordDescGC"]], 
                                                                           ref_depths = results_asymprojdepth_size3000_experiment1[[i]][[1]][["asym_CoordDescGC"]],
                                                                           alpha = 0.05,  outofcontrol = FALSE)
  
  refsample_rcontrolchart_asym_CoordDescGC_size4000[[i]] <- rControlChart(depths = results_asymprojdepth_size4000_experiment1[[i]][[1]][["asym_CoordDescGC"]], 
                                                                           ref_depths = results_asymprojdepth_size4000_experiment1[[i]][[1]][["asym_CoordDescGC"]],
                                                                           alpha = 0.05,  outofcontrol = FALSE)
  
}

#Compute FAR from all classes 

FAR_mean_size2000_asym_CoordDescGC <- mean(unlist(lapply(refsample_rcontrolchart_asym_CoordDescGC_size2000, FUN = `[[`, "FAR")))
FAR_mean_size3000_asym_CoordDescGC <- mean(unlist(lapply(refsample_rcontrolchart_asym_CoordDescGC_size3000, FUN = `[[`, "FAR")))
FAR_mean_size4000_asym_CoordDescGC <- mean(unlist(lapply(refsample_rcontrolchart_asym_CoordDescGC_size4000, FUN = `[[`, "FAR")))


# Apply r Control Charts on Phase II, in-control part (test data) to compute SR

# Compute the fraction of each predicted class 

labels_pred <- as.numeric(df_test_labels$V11)


labels_pred_allclasses <- list()
for (i in 1:10){
  
  labels_pred_allclasses[[i]] <- c(which(labels_pred == (i-1)))
  
}

fractions_pred_allclasses <- c()
for (i in 1:10){
  fractions_pred_allclasses[i] <- length(labels_pred_allclasses[[i]])/10000
}


## Size 2000: Apply r Control Chart

testdata_rcontrolchart_asym_CoordDescGC_size2000  <- c()

for (i in 1:10000){
  class_pred <- df_test_labels$V11[i] + 1
  ref_depths_pred <- as.numeric(results_asymprojdepth_size2000_experiment1[[class_pred]][[1]][["asym_CoordDescGC"]])
  testdata_rcontrolchart_asym_CoordDescGC_size2000[i] <- sum(ref_depths_pred <= AsymCoordDescGC_results_pred_size2000[i]) / length(ref_depths_pred)
  
}

SR_testdata_size2000_asym_CoordDescGC <- length(which(testdata_rcontrolchart_asym_CoordDescGC_size2000 <= 0.05))/length(testdata_rcontrolchart_asym_CoordDescGC_size2000)


ranks_pred_allclasses_size2000_asym_CoordDescGC <- list()
for (i in 1:10){
  ranks_pred_allclasses_size2000_asym_CoordDescGC[[i]] <- testdata_rcontrolchart_asym_CoordDescGC_size2000[labels_pred_allclasses[[i]]]
}


SR_allclasses_size2000_asym_CoordDescGC <- c()
for (i in 1:10){
  SR_allclasses_size2000_asym_CoordDescGC[i] <- length(which(ranks_pred_allclasses_size2000_asym_CoordDescGC[[i]]<=0.05))/length(ranks_pred_allclasses_size2000_asym_CoordDescGC[[i]])
}

SR_testdata_weighted_size2000_asym_CoordDescGC <- sum(SR_allclasses_size2000_asym_CoordDescGC*fractions_pred_allclasses)



## Size 3000: Apply r Control Chart

testdata_rcontrolchart_asym_CoordDescGC_size3000  <- c()

for (i in 1:10000){
  class_pred <- df_test_labels$V11[i] + 1
  ref_depths_pred <- as.numeric(results_asymprojdepth_size3000_experiment1[[class_pred]][[1]][["asym_CoordDescGC"]])
  testdata_rcontrolchart_asym_CoordDescGC_size3000[i] <- sum(ref_depths_pred <= AsymCoordDescGC_results_pred_size3000[i]) / length(ref_depths_pred)
  
}

SR_testdata_size3000_asym_CoordDescGC <- length(which(testdata_rcontrolchart_asym_CoordDescGC_size3000 <= 0.05))/length(testdata_rcontrolchart_asym_CoordDescGC_size3000)


ranks_pred_allclasses_size3000_asym_CoordDescGC <- list()
for (i in 1:10){
  ranks_pred_allclasses_size3000_asym_CoordDescGC[[i]] <- testdata_rcontrolchart_asym_CoordDescGC_size3000[labels_pred_allclasses[[i]]]
}


SR_allclasses_size3000_asym_CoordDescGC <- c()
for (i in 1:10){
  SR_allclasses_size3000_asym_CoordDescGC[i] <- length(which(ranks_pred_allclasses_size3000_asym_CoordDescGC[[i]]<=0.05))/length(ranks_pred_allclasses_size3000_asym_CoordDescGC[[i]])
}

SR_testdata_weighted_size3000_asym_CoordDescGC <- sum(SR_allclasses_size3000_asym_CoordDescGC*fractions_pred_allclasses)

## Size 4000: Apply r Control Chart

testdata_rcontrolchart_asym_CoordDescGC_size4000  <- c()

for (i in 1:10000){
  class_pred <- df_test_labels$V11[i] + 1
  ref_depths_pred <- as.numeric(results_asymprojdepth_size4000_experiment1[[class_pred]][[1]][["asym_CoordDescGC"]])
  testdata_rcontrolchart_asym_CoordDescGC_size4000[i] <- sum(ref_depths_pred <= AsymCoordDescGC_results_pred_size4000[i]) / length(ref_depths_pred)
  
}

SR_testdata_size4000_asym_CoordDescGC <- length(which(testdata_rcontrolchart_asym_CoordDescGC_size4000 <= 0.05))/length(testdata_rcontrolchart_asym_CoordDescGC_size4000)


ranks_pred_allclasses_size4000_asym_CoordDescGC <- list()
for (i in 1:10){
  ranks_pred_allclasses_size4000_asym_CoordDescGC[[i]] <- testdata_rcontrolchart_asym_CoordDescGC_size4000[labels_pred_allclasses[[i]]]
}


SR_allclasses_size4000_asym_CoordDescGC <- c()
for (i in 1:10){
  SR_allclasses_size4000_asym_CoordDescGC[i] <- length(which(ranks_pred_allclasses_size4000_asym_CoordDescGC[[i]]<=0.05))/length(ranks_pred_allclasses_size4000_asym_CoordDescGC[[i]])
}

SR_testdata_weighted_size4000_asym_CoordDescGC <- sum(SR_allclasses_size4000_asym_CoordDescGC*fractions_pred_allclasses)


# Apply r Control Charts on Phase II, ou-of-control part (out-of-control data) to compute CDR

# Weighted version
labels_outofcontrol_allclasses <- list()
for (i in 1:10){
  
  labels_outofcontrol_allclasses[[i]] <- c(which(outofcontrol_predlabels == (i-1)))
  
}

fractions_outofcontrol_allclasses <- c()
for (i in 1:10){
  fractions_outofcontrol_allclasses[i] <- length(labels_outofcontrol_allclasses[[i]])/dim(outofcontrol_predlabels)[1]
}


## Size 2000

outofcontrol_rcontrolchart_asym_CoordDescGC_size2000  <- c()

for (i in 1:400){
  class_pred_outofcontrol <- outofcontrol_predlabels$V1[i] + 1
  ref_depths_pred <- as.numeric(results_asymprojdepth_size2000_experiment1[[class_pred_outofcontrol]][[1]][["asym_CoordDescGC"]])
  outofcontrol_rcontrolchart_asym_CoordDescGC_size2000[i] <- sum(ref_depths_pred <= AsymProjDepthCoordDescGC_outofcontrol_size2000[i]) / length(ref_depths_pred)
  
}

CDR_outofcontrol_size2000_asym_CoordDescGC <- length(which(outofcontrol_rcontrolchart_asym_CoordDescGC_size2000 <= 0.05))/length(outofcontrol_rcontrolchart_asym_CoordDescGC_size2000)

ranks_outofcontrol_allclasses_size2000_asym_CoordDescGC <- list()
for (i in 1:10){
  ranks_outofcontrol_allclasses_size2000_asym_CoordDescGC[[i]] <- outofcontrol_rcontrolchart_asym_CoordDescGC_size2000[labels_outofcontrol_allclasses[[i]]]
}

CDR_outofcontrol_allclasses_size2000_asym_CoordDescGC <- c()
for (i in 1:10){
  CDR_outofcontrol_allclasses_size2000_asym_CoordDescGC[i] <- length(which(ranks_outofcontrol_allclasses_size2000_asym_CoordDescGC[[i]]<=0.05))/length(ranks_outofcontrol_allclasses_size2000_asym_CoordDescGC[[i]])
}

CDR_outofcontrol_weighted_size2000_asym_CoordDescGC <- sum(CDR_outofcontrol_allclasses_size2000_asym_CoordDescGC*fractions_outofcontrol_allclasses) 


## Size 3000

outofcontrol_rcontrolchart_asym_CoordDescGC_size3000  <- c()

for (i in 1:400){
  class_pred_outofcontrol <- outofcontrol_predlabels$V1[i] + 1
  ref_depths_pred <- as.numeric(results_asymprojdepth_size3000_experiment1[[class_pred_outofcontrol]][[1]][["asym_CoordDescGC"]])
  outofcontrol_rcontrolchart_asym_CoordDescGC_size3000[i] <- sum(ref_depths_pred <= AsymProjDepthCoordDescGC_outofcontrol_size3000[i]) / length(ref_depths_pred)
  
}

CDR_outofcontrol_size3000_asym_CoordDescGC <- length(which(outofcontrol_rcontrolchart_asym_CoordDescGC_size3000 <= 0.05))/length(outofcontrol_rcontrolchart_asym_CoordDescGC_size3000)

ranks_outofcontrol_allclasses_size3000_asym_CoordDescGC <- list()
for (i in 1:10){
  ranks_outofcontrol_allclasses_size3000_asym_CoordDescGC[[i]] <- outofcontrol_rcontrolchart_asym_CoordDescGC_size3000[labels_outofcontrol_allclasses[[i]]]
}

CDR_outofcontrol_allclasses_size3000_asym_CoordDescGC <- c()
for (i in 1:10){
  CDR_outofcontrol_allclasses_size3000_asym_CoordDescGC[i] <- length(which(ranks_outofcontrol_allclasses_size3000_asym_CoordDescGC[[i]]<=0.05))/length(ranks_outofcontrol_allclasses_size3000_asym_CoordDescGC[[i]])
}

CDR_outofcontrol_weighted_size3000_asym_CoordDescGC <- sum(CDR_outofcontrol_allclasses_size3000_asym_CoordDescGC*fractions_outofcontrol_allclasses) 


## Size 4000

outofcontrol_rcontrolchart_asym_CoordDescGC_size4000  <- c()

for (i in 1:400){
  class_pred_outofcontrol <- outofcontrol_predlabels$V1[i] + 1
  ref_depths_pred <- as.numeric(results_asymprojdepth_size4000_experiment1[[class_pred_outofcontrol]][[1]][["asym_CoordDescGC"]])
  outofcontrol_rcontrolchart_asym_CoordDescGC_size4000[i] <- sum(ref_depths_pred <= AsymProjDepthCoordDescGC_outofcontrol_size4000[i]) / length(ref_depths_pred)
  
}

CDR_outofcontrol_size4000_asym_CoordDescGC <- length(which(outofcontrol_rcontrolchart_asym_CoordDescGC_size4000 <= 0.05))/length(outofcontrol_rcontrolchart_asym_CoordDescGC_size4000)

ranks_outofcontrol_allclasses_size4000_asym_CoordDescGC <- list()
for (i in 1:10){
  ranks_outofcontrol_allclasses_size4000_asym_CoordDescGC[[i]] <- outofcontrol_rcontrolchart_asym_CoordDescGC_size4000[labels_outofcontrol_allclasses[[i]]]
}

CDR_outofcontrol_allclasses_size4000_asym_CoordDescGC <- c()
for (i in 1:10){
  CDR_outofcontrol_allclasses_size4000_asym_CoordDescGC[i] <- length(which(ranks_outofcontrol_allclasses_size4000_asym_CoordDescGC[[i]]<=0.05))/length(ranks_outofcontrol_allclasses_size4000_asym_CoordDescGC[[i]])
}

CDR_outofcontrol_weighted_size4000_asym_CoordDescGC <- sum(CDR_outofcontrol_allclasses_size4000_asym_CoordDescGC*fractions_outofcontrol_allclasses) 



# SUMMARY OF THE RESULTS

PD_1a_size2000 <- as.array(c(FAR_mean_size2000_asym_CoordDescGC, round(SR_testdata_weighted_size2000_asym_CoordDescGC, 2), round(CDR_outofcontrol_weighted_size2000_asym_CoordDescGC, 2)))
PD_1a_size3000 <- as.array(c(FAR_mean_size3000_asym_CoordDescGC, round(SR_testdata_weighted_size3000_asym_CoordDescGC, 2), round(CDR_outofcontrol_weighted_size3000_asym_CoordDescGC, 2)))
PD_1a_size4000 <- as.array(c(FAR_mean_size4000_asym_CoordDescGC, round(SR_testdata_weighted_size4000_asym_CoordDescGC, 2), round(CDR_outofcontrol_weighted_size4000_asym_CoordDescGC, 2)))

