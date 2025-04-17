# MISCLASSIFICATION ANALYSIS: SYMMETRIC PROJECTION DEPTH, COORDINATE DESCENT ALGORITHM
library(dplyr)

# Number of correctly classified and misclassified

misclassified_ids <- which(df_test_labels$V11 != df_test_labels$V12)
nr_misclassified <- length(misclassified_ids) 
nr_corrclass <- length(which(df_test_labels$V11 == df_test_labels$V12))


# Calculate the signal rates with condition either misclassified or correctly predicted class

## Size 2000

nr_misclassified_signals_size2000 <- length(which(as.numeric(which(ranks_testdata_pred_size2000 <= 0.05))%in% misclassified_ids == TRUE))
nr_corrclass_signals_size2000 <- length(which(ranks_testdata_pred_size2000 <= 0.05)) - nr_misclassified_signals_size2000
SRconditional_signal_misclassified_size2000 <- nr_misclassified_signals_size2000/nr_misclassified
SRconditional_signal_corrclass_size2000 <- nr_corrclass_signals_size2000/nr_corrclass


## Size 3000

nr_misclassified_signals_size3000 <- length(which(as.numeric(which(ranks_testdata_pred_size3000 <= 0.05))%in% misclassified_ids == TRUE))
nr_corrclass_signals_size3000 <- length(which(ranks_testdata_pred_size3000 <= 0.05)) - nr_misclassified_signals_size3000
SRconditional_signal_misclassified_size3000 <- nr_misclassified_signals_size3000/nr_misclassified
SRconditional_signal_corrclass_size3000 <- nr_corrclass_signals_size3000/nr_corrclass

## Size 4000

nr_misclassified_signals_size4000 <- length(which(as.numeric(which(ranks_testdata_pred_size4000 <= 0.05))%in% misclassified_ids == TRUE))
nr_corrclass_signals_size4000 <- length(which(ranks_testdata_pred_size4000 <= 0.05)) - nr_misclassified_signals_size4000
SRconditional_signal_misclassified_size4000 <- nr_misclassified_signals_size4000/nr_misclassified
SRconditional_signal_corrclass_size4000 <- nr_corrclass_signals_size4000/nr_corrclass



# SUMMARY OF THE RESULTS
SRconditional_signal_misclass_PD_1_allsizes <- as.array(c(round(SRconditional_signal_misclassified_size2000, 2), round(SRconditional_signal_misclassified_size3000, 2), round(SRconditional_signal_misclassified_size4000, 2)))
SRconditional_signal_corrclass_PD_1_allsizes <- as.array(c(round(SRconditional_signal_corrclass_size2000, 2), round(SRconditional_signal_corrclass_size3000, 2), round(SRconditional_signal_corrclass_size4000, 2)))
