# Benchmark: Perform monitoring using alternative algorithms on the embeddings, Toy Example, Section 4.3

## Data preparation: Identical to the data preprocessing for computing data depths and applying r control chart

# Read in data and labels

## Embeddings
traindata_embeddings <- read.table("ToyExample_Section4_3/Data/Embeddings/ToyExample_traindata_embeddings3d.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
testdata_embeddings <- read.table("ToyExample_Section4_3/Data/Embeddings/ToyExample_testdata_embeddings3d.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
outofcontroldata_embeddings <- read.table("ToyExample_Section4_3/Data/Embeddings/ToyExample_outofcontroldata_embeddings3d.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)


## Labels - also the input needed for the score-based methods (MDis and NOF)
train_labels_scores_toyexample <- read.table("ToyExample_Section4_3/Data/Labels/train_labels_scores_toyexample.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
test_labels_scores_toyexample <- read.table("ToyExample_Section4_3/Data/Labels/test_labels_scores_toyexample.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
outofcontrol_labels_scores_toyexample <- read.table("ToyExample_Section4_3/Data/Labels/outofcontrol_labels_scores_toyexample.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)

# Creation of the reference samples
perclass_refsize <- 100

# PHASE I
## EXPLANATION: *_labels_scores_toyexample - Column 1: Labels, Column 2: Binary predictions, Column 3: Score predictions.
# Check if there are misclassified samples in training data
which(train_labels_scores_toyexample[,1] != train_labels_scores_toyexample[,2]) # No misclassification detected

## Take indices of the data points for each class
idx_class0 <- which(train_labels_scores_toyexample[, 1] == 0)
idx_class1 <- which(train_labels_scores_toyexample[, 1] == 1)

## Create two dataframes of training data/reference samples with entire information
class0_phaseI <- cbind(traindata_embeddings[idx_class0, ], train_labels_scores_toyexample[idx_class0, ])
colnames(class0_phaseI) <- c("V1", "V2", "V3", "labels", "predictions", "scores")
class0_phaseI_ordered <-  class0_phaseI[order(class0_phaseI$scores), ]

class1_phaseI <- cbind(traindata_embeddings[idx_class1, ], train_labels_scores_toyexample[idx_class1, ])
colnames(class1_phaseI) <- c("V1", "V2", "V3", "labels", "predictions", "scores")
class1_phaseI_ordered <-  class1_phaseI[order(class1_phaseI$scores, decreasing = TRUE), ]

## Define reference samples 
refsample_class0 <- class0_phaseI_ordered[1:perclass_refsize, 1:3]
refsample_class1 <- class1_phaseI_ordered[1:perclass_refsize, 1:3] 

# PHASE II: In-control

## Separate into classes the test data and create two dataframes
test_class0_ids <- which(test_labels_scores_toyexample[, 2] == 0)
z_test_class0 <- testdata_embeddings[c(test_class0_ids), ]

test_class1_ids <- which(test_labels_scores_toyexample[,2] == 1)
z_test_class1 <- testdata_embeddings[c(test_class1_ids), ]

# PHASE II: Out-of-control

outofcontrol_labels <- outofcontrol_labels_scores_toyexample[, 2]
unique(outofcontrol_labels) # All 1, the outofcontroldata_embeddings can stay as an original dataframe



#----------------------------------------KDEOS------------------------------------------------#
#The score is normalized between 0 and 1, such that observation with 1 has the lowest density estimation and greatest outlierness. 

# Compute outlierness within the reference samples 
outlierness_refsample_class0_KDEOS <- KDEOS(refsample_class0, k_min = 10, k_max = 20, eps = NULL)
outlierness_refsample_class1_KDEOS <- KDEOS(refsample_class1, k_min = 10, k_max = 20, eps = NULL)

# Test data

outlierness_testdata_class0_KDEOS <- c()

for (i in 1:dim(z_test_class0)[1]){
  outlierness_testdata_class0_KDEOS[i] <- tail(KDEOS(rbind(refsample_class0, z_test_class0[i,]), k_min = 10, k_max = 20, eps = NULL), n=1)
  
}

outlierness_testdata_class1_KDEOS <- c()

for (i in 1:dim(z_test_class1)[1]){
  outlierness_testdata_class1_KDEOS[i] <- tail(KDEOS(rbind(refsample_class1, z_test_class1[i,]), k_min = 10, k_max = 20, eps = NULL), n=1)
  
}

# Outofcontrol data

unique(outofcontrol_labels) # All 1
outlierness_outofcontroldata_KDEOS <- c()

for (i in 1:dim(outofcontroldata_embeddings)[1]){
  outlierness_outofcontroldata_KDEOS[i] <- tail(KDEOS(rbind(refsample_class1, outofcontroldata_embeddings[i,]), k_min = 10, k_max = 20, eps = NULL), n=1)
  
}


#----------------------------------------LOF------------------------------------------------# 

# The greater the LOF, the greater outlierness.

# Compute outlierness within the reference samples 
outlierness_refsample_class0_LOF <- LOF(refsample_class0, k = 10) 
outlierness_refsample_class1_LOF <- LOF(refsample_class1, k = 10)

# Test data

outlierness_testdata_class0_LOF <- c()

for (i in 1:dim(z_test_class0)[1]){
  outlierness_testdata_class0_LOF[i] <- tail(LOF(rbind(refsample_class0, z_test_class0[i,]), k = 10), n=1)
  
}

outlierness_testdata_class1_LOF <- c()

for (i in 1:dim(z_test_class1)[1]){
  outlierness_testdata_class1_LOF[i] <- tail(LOF(rbind(refsample_class1, z_test_class1[i,]), k = 10), n=1)
  
}

# Outofcontrol data

unique(outofcontrol_labels) # All 1
outlierness_outofcontroldata_LOF <- c()

for (i in 1:dim(outofcontroldata_embeddings)[1]){
  outlierness_outofcontroldata_LOF[i] <- tail(LOF(rbind(refsample_class1, outofcontroldata_embeddings[i,]), k = 10), n=1)
  
}



#----------------------------------------Isolation Forest (iForest)------------------------------------------------#

set.seed(10)

# Type = "score": Values closer to 1 indicate more outlierness, while values closer to 0.5 indicate average outlierness, and close to 0 more averageness (harder to isolate).
# For all scoring metrics, higher values indicate more outlierness.

# Compute outlierness within the reference samples 

outlierness_refsample_class0_isolationforest <- c()
outlierness_refsample_class1_isolationforest <- c()

model_refsample_class0_isolationforest <- isolation.forest(refsample_class0, ndim=2, ntrees=100, nthreads=2)
model_refsample_class1_isolationforest <- isolation.forest(refsample_class1, ndim=2, ntrees=100, nthreads=2)

outlierness_refsample_class0_isolationforest <- predict(model_refsample_class0_isolationforest, refsample_class0, type="score")
outlierness_refsample_class1_isolationforest <- predict(model_refsample_class1_isolationforest, refsample_class1, type="score")

# Test data

outlierness_testdata_class0_isolationforest <- c()
outlierness_testdata_class1_isolationforest <- c()

for (i in 1:dim(z_test_class0)[1]){
  outlierness_testdata_class0_isolationforest[i] <- predict(model_refsample_class0_isolationforest,  z_test_class0[i,], type="score")
  
}

for (i in 1:dim(z_test_class1)[1]){
  outlierness_testdata_class1_isolationforest[i] <- predict(model_refsample_class1_isolationforest,  z_test_class1[i,], type="score")
  
}

# Outofcontrol data

unique(outofcontrol_labels) # All 1

outlierness_outofcontroldata_isolationforest <- c()

for (i in 1:dim(outofcontroldata_embeddings)[1]){
  outlierness_outofcontroldata_isolationforest[i] <- predict(model_refsample_class1_isolationforest,  outofcontroldata_embeddings[i,], type="score")
  
}


