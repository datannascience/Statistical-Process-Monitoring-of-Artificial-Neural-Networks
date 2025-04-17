
# Data preparation: Toy Example, Section 4.3

# Read in data and labels

## Embeddings
traindata_embeddings <- read.table("ToyExample_Section4_3/Data/Embeddings/ToyExample_traindata_embeddings3d.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
testdata_embeddings <- read.table("ToyExample_Section4_3/Data/Embeddings/ToyExample_testdata_embeddings3d.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
outofcontroldata_embeddings <- read.table("ToyExample_Section4_3/Data/Embeddings/ToyExample_outofcontroldata_embeddings3d.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)


## Labels
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

