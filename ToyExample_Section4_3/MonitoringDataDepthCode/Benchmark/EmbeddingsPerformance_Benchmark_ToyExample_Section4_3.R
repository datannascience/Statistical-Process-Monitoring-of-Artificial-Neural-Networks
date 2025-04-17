# Write a procedure for computing FAR, SR and CDR based on the results from Outlier Detection Methods

# The KDEOS score is normalized between 0 and 1, such that observation with 1 has the lowest density estimation and greatest outlierness;
# The greater the LOF, the greater outlierness;
# The greater the iForest, the greater outlierness.

# -> That is the reverse interpretation of the depth values (there, the greater the value is, the more central point it is and the less outlying it could be)

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


#----------------------------------------------------- Outlierness detection: KDEOS -----------------------------------------------------#


# Refsamples

refsample_outliernessdetection_class0_KDEOS <- list()

refsample_outliernessdetection_class0_KDEOS <- OutliernessDetection(outlierness_refsample_class0_KDEOS, outlierness_refsample_class0_KDEOS, proportion = 0.05, outofcontrol = FALSE)



refsample_outliernessdetection_class1_KDEOS <- list()

refsample_outliernessdetection_class1_KDEOS <- OutliernessDetection(outlierness_refsample_class1_KDEOS, outlierness_refsample_class1_KDEOS, proportion = 0.05, outofcontrol = FALSE)

# Testdata

testdata_outliernessdetection_class0_KDEOS <- list()

testdata_outliernessdetection_class0_KDEOS <- OutliernessDetection(outlierness_testdata_class0_KDEOS, outlierness_refsample_class0_KDEOS, proportion = 0.05, outofcontrol = FALSE)


testdata_outliernessdetection_class1_KDEOS <- list()

testdata_outliernessdetection_class1_KDEOS <- OutliernessDetection(outlierness_testdata_class1_KDEOS, outlierness_refsample_class1_KDEOS, proportion = 0.05, outofcontrol = FALSE)


# Outofcontrol data

outofcontrol_outliernessdetection_KDEOS <- list()
outofcontrol_outliernessdetection_KDEOS <- OutliernessDetection(outlierness_outofcontroldata_KDEOS, outlierness_refsample_class1_KDEOS, proportion = 0.05, outofcontrol = TRUE)             


#----------------------------------------------------- Outlierness detection: LOF -----------------------------------------------------#


# Refsamples

refsample_outliernessdetection_class0_LOF <- list()

refsample_outliernessdetection_class0_LOF <- OutliernessDetection(outlierness_refsample_class0_LOF, outlierness_refsample_class0_LOF, proportion = 0.05, outofcontrol = FALSE)



refsample_outliernessdetection_class1_LOF <- list()

refsample_outliernessdetection_class1_LOF <- OutliernessDetection(outlierness_refsample_class1_LOF, outlierness_refsample_class1_LOF, proportion = 0.05, outofcontrol = FALSE)

# Testdata

testdata_outliernessdetection_class0_LOF <- list()

testdata_outliernessdetection_class0_LOF <- OutliernessDetection(outlierness_testdata_class0_LOF, outlierness_refsample_class0_LOF, proportion = 0.05, outofcontrol = FALSE)


testdata_outliernessdetection_class1_LOF <- list()

testdata_outliernessdetection_class1_LOF <- OutliernessDetection(outlierness_testdata_class1_LOF, outlierness_refsample_class1_LOF, proportion = 0.05, outofcontrol = FALSE)


# Outofcontrol data

outofcontrol_outliernessdetection_LOF <- list()
outofcontrol_outliernessdetection_LOF <- OutliernessDetection(outlierness_outofcontroldata_LOF, outlierness_refsample_class1_LOF, proportion = 0.05, outofcontrol = TRUE)             


#----------------------------------------------------- Outlierness detection: isolationforest -----------------------------------------------------#


# Refsamples

refsample_outliernessdetection_class0_isolationforest <- list()

refsample_outliernessdetection_class0_isolationforest <- OutliernessDetection(outlierness_refsample_class0_isolationforest, outlierness_refsample_class0_isolationforest, proportion = 0.05, outofcontrol = FALSE)



refsample_outliernessdetection_class1_isolationforest <- list()

refsample_outliernessdetection_class1_isolationforest <- OutliernessDetection(outlierness_refsample_class1_isolationforest, outlierness_refsample_class1_isolationforest, proportion = 0.05, outofcontrol = FALSE)

# Testdata

testdata_outliernessdetection_class0_isolationforest <- list()

testdata_outliernessdetection_class0_isolationforest <- OutliernessDetection(outlierness_testdata_class0_isolationforest, outlierness_refsample_class0_isolationforest, proportion = 0.05, outofcontrol = FALSE)


testdata_outliernessdetection_class1_isolationforest <- list()

testdata_outliernessdetection_class1_isolationforest <- OutliernessDetection(outlierness_testdata_class1_isolationforest, outlierness_refsample_class1_isolationforest, proportion = 0.05, outofcontrol = FALSE)


# Outofcontrol data

outofcontrol_outliernessdetection_isolationforest <- list()
outofcontrol_outliernessdetection_isolationforest <- OutliernessDetection(outlierness_outofcontroldata_isolationforest, outlierness_refsample_class1_isolationforest, proportion = 0.05, outofcontrol = TRUE)             


# Final results of all methods: SUMMARY

fraction_testdata_class0 <- length(test_class0_ids)/dim(testdata_embeddings)[1]
fraction_testdata_class1 <- length(test_class1_ids)/dim(testdata_embeddings)[1]

FAR_refsample_class0_comparisonmethods <- c(refsample_outliernessdetection_class0_KDEOS[["FAR"]], refsample_outliernessdetection_class0_LOF[["FAR"]], refsample_outliernessdetection_class0_isolationforest[["FAR"]])

FAR_refsample_class1_comparisonmethods <- c(refsample_outliernessdetection_class1_KDEOS[["FAR"]], 
                                            refsample_outliernessdetection_class1_LOF[["FAR"]], refsample_outliernessdetection_class1_isolationforest[["FAR"]])

SR_testdata_class0_comparisonmethods <- c(testdata_outliernessdetection_class0_KDEOS[["FAR"]], 
                                           testdata_outliernessdetection_class0_LOF[["FAR"]], testdata_outliernessdetection_class0_isolationforest[["FAR"]])

SR_testdata_class1_comparisonmethods <- c(testdata_outliernessdetection_class1_KDEOS[["FAR"]], 
                                          testdata_outliernessdetection_class1_LOF[["FAR"]], testdata_outliernessdetection_class1_isolationforest[["FAR"]])

fraction_testdata_class0 <- length(test_class0_ids)/dim(testdata_embeddings)[1]
fraction_testdata_class1 <- length(test_class1_ids)/dim(testdata_embeddings)[1]


FAR_refsample_comparisonmethods <- c()
SR_testdata_comparisonmethods <- c()



for(i in 1:length(FAR_refsample_class0_comparisonmethods)){
  FAR_refsample_comparisonmethods[i] <-  (FAR_refsample_class0_comparisonmethods[i] + FAR_refsample_class1_comparisonmethods[i])/2
  SR_testdata_comparisonmethods[i] <- SR_testdata_class0_comparisonmethods[i]*fraction_testdata_class0 + SR_testdata_class1_comparisonmethods[i]*fraction_testdata_class1
}


CDR_outofcontrol_comparisonmethods <- c(outofcontrol_outliernessdetection_KDEOS[["CDR"]], outofcontrol_outliernessdetection_LOF[["CDR"]], outofcontrol_outliernessdetection_isolationforest[["CDR"]])

