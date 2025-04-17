

# MISCLASSIFICATION ANALYSIS: ASYMMETRIC PROJECTION DEPTH

# Number of correctly classified and misclassified
misclassified_idx <- which(df_test_labels$V11 != df_test_labels$V12)
nr_misclassified <- length(which(df_test_labels$V11 != df_test_labels$V12))
nr_corrclassified <- length(which(df_test_labels$V11 == df_test_labels$V12))

# Size 2000

## Get indices of those data samples that produced a signal
testdata_signals_asym_CoordDescGC_size2000 <- which(testdata_rcontrolchart_asym_CoordDescGC_size2000 <= 0.05)
testdata_signals_asym_NelderMeadGC_size2000 <- which(testdata_rcontrolchart_asym_NelderMeadGC_size2000 <= 0.05)
testdata_signals_asym_RefinedRandom_size2000 <- which(testdata_rcontrolchart_asym_RefinedRandom_size2000 <= 0.05)

## Get how many of the produced signals are also misclassified
testdata_signals_misclass_asym_CoordDescGC_size2000 <- length(intersect(testdata_signals_asym_CoordDescGC_size2000, misclassified_idx)) 
testdata_signals_misclass_asym_NelderMeadGC_size2000 <- length(intersect(testdata_signals_asym_NelderMeadGC_size2000, misclassified_idx))
testdata_signals_misclass_asym_RefinedRandom_size2000 <- length(intersect(testdata_signals_asym_RefinedRandom_size2000, misclassified_idx))

## Get how many were having a signal but correctly predicted
testdata_signals_corrclass_asym_CoordDescGC_size2000 <- length(testdata_signals_asym_CoordDescGC_size2000) - testdata_signals_misclass_asym_CoordDescGC_size2000
testdata_signals_corrclass_asym_NelderMeadGC_size2000 <- length(testdata_signals_asym_NelderMeadGC_size2000) - testdata_signals_misclass_asym_NelderMeadGC_size2000
testdata_signals_corrclass_asym_RefinedRandom_size2000 <- length(testdata_signals_asym_RefinedRandom_size2000) - testdata_signals_misclass_asym_RefinedRandom_size2000

## Compute the conditional signal rates

SRconditional_signal_misclass_asym_CoordDescGC_size2000 <- testdata_signals_misclass_asym_CoordDescGC_size2000/nr_misclassified
SRconditional_signal_corrclass_asym_CoordDescGC_size2000 <- testdata_signals_corrclass_asym_CoordDescGC_size2000/nr_corrclassified

SRconditional_signal_misclass_asym_NelderMeadGC_size2000 <- testdata_signals_misclass_asym_NelderMeadGC_size2000/nr_misclassified
SRconditional_signal_corrclass_asym_NelderMeadGC_size2000 <- testdata_signals_corrclass_asym_NelderMeadGC_size2000/nr_corrclassified

SRconditional_signal_misclass_asym_RefinedRandom_size2000 <- testdata_signals_misclass_asym_RefinedRandom_size2000/nr_misclassified
SRconditional_signal_corrclass_asym_RefinedRandom_size2000 <- testdata_signals_corrclass_asym_RefinedRandom_size2000/nr_corrclassified


# Size 3000

## Get indices of those data samples that produced a signal
testdata_signals_asym_CoordDescGC_size3000 <- which(testdata_rcontrolchart_asym_CoordDescGC_size3000 <= 0.05)
testdata_signals_asym_NelderMeadGC_size3000 <- which(testdata_rcontrolchart_asym_NelderMeadGC_size3000 <= 0.05)
testdata_signals_asym_RefinedRandom_size3000 <- which(testdata_rcontrolchart_asym_RefinedRandom_size3000 <= 0.05)

## Get how many of the produced signals are also misclassified
testdata_signals_misclass_asym_CoordDescGC_size3000 <- length(intersect(testdata_signals_asym_CoordDescGC_size3000, misclassified_idx)) 
testdata_signals_misclass_asym_NelderMeadGC_size3000 <- length(intersect(testdata_signals_asym_NelderMeadGC_size3000, misclassified_idx))
testdata_signals_misclass_asym_RefinedRandom_size3000 <- length(intersect(testdata_signals_asym_RefinedRandom_size3000, misclassified_idx))

## Get how many were having a signal but correctly predicted
testdata_signals_corrclass_asym_CoordDescGC_size3000 <- length(testdata_signals_asym_CoordDescGC_size3000) - testdata_signals_misclass_asym_CoordDescGC_size3000
testdata_signals_corrclass_asym_NelderMeadGC_size3000 <- length(testdata_signals_asym_NelderMeadGC_size3000) - testdata_signals_misclass_asym_NelderMeadGC_size3000
testdata_signals_corrclass_asym_RefinedRandom_size3000 <- length(testdata_signals_asym_RefinedRandom_size3000) - testdata_signals_misclass_asym_RefinedRandom_size3000

## Compute the conditional signal rates

SRconditional_signal_misclass_asym_CoordDescGC_size3000 <- testdata_signals_misclass_asym_CoordDescGC_size3000/nr_misclassified
SRconditional_signal_corrclass_asym_CoordDescGC_size3000 <- testdata_signals_corrclass_asym_CoordDescGC_size3000/nr_corrclassified

SRconditional_signal_misclass_asym_NelderMeadGC_size3000 <- testdata_signals_misclass_asym_NelderMeadGC_size3000/nr_misclassified
SRconditional_signal_corrclass_asym_NelderMeadGC_size3000 <- testdata_signals_corrclass_asym_NelderMeadGC_size3000/nr_corrclassified

SRconditional_signal_misclass_asym_RefinedRandom_size3000 <- testdata_signals_misclass_asym_RefinedRandom_size3000/nr_misclassified
SRconditional_signal_corrclass_asym_RefinedRandom_size3000 <- testdata_signals_corrclass_asym_RefinedRandom_size3000/nr_corrclassified


# Size 4000

## Get indices of those data samples that produced a signal
testdata_signals_asym_CoordDescGC_size4000 <- which(testdata_rcontrolchart_asym_CoordDescGC_size4000 <= 0.05)
testdata_signals_asym_NelderMeadGC_size4000 <- which(testdata_rcontrolchart_asym_NelderMeadGC_size4000 <= 0.05)
testdata_signals_asym_RefinedRandom_size4000 <- which(testdata_rcontrolchart_asym_RefinedRandom_size4000 <= 0.05)

## Get how many of the produced signals are also misclassified
testdata_signals_misclass_asym_CoordDescGC_size4000 <- length(intersect(testdata_signals_asym_CoordDescGC_size4000, misclassified_idx)) 
testdata_signals_misclass_asym_NelderMeadGC_size4000 <- length(intersect(testdata_signals_asym_NelderMeadGC_size4000, misclassified_idx))
testdata_signals_misclass_asym_RefinedRandom_size4000 <- length(intersect(testdata_signals_asym_RefinedRandom_size4000, misclassified_idx))

## Get how many were having a signal but correctly predicted
testdata_signals_corrclass_asym_CoordDescGC_size4000 <- length(testdata_signals_asym_CoordDescGC_size4000) - testdata_signals_misclass_asym_CoordDescGC_size4000
testdata_signals_corrclass_asym_NelderMeadGC_size4000 <- length(testdata_signals_asym_NelderMeadGC_size4000) - testdata_signals_misclass_asym_NelderMeadGC_size4000
testdata_signals_corrclass_asym_RefinedRandom_size4000 <- length(testdata_signals_asym_RefinedRandom_size4000) - testdata_signals_misclass_asym_RefinedRandom_size4000

## Compute the conditional signal rates

SRconditional_signal_misclass_asym_CoordDescGC_size4000 <- testdata_signals_misclass_asym_CoordDescGC_size4000/nr_misclassified
SRconditional_signal_corrclass_asym_CoordDescGC_size4000 <- testdata_signals_corrclass_asym_CoordDescGC_size4000/nr_corrclassified

SRconditional_signal_misclass_asym_NelderMeadGC_size4000 <- testdata_signals_misclass_asym_NelderMeadGC_size4000/nr_misclassified
SRconditional_signal_corrclass_asym_NelderMeadGC_size4000 <- testdata_signals_corrclass_asym_NelderMeadGC_size4000/nr_corrclassified

SRconditional_signal_misclass_asym_RefinedRandom_size4000 <- testdata_signals_misclass_asym_RefinedRandom_size4000/nr_misclassified
SRconditional_signal_corrclass_asym_RefinedRandom_size4000 <- testdata_signals_corrclass_asym_RefinedRandom_size4000/nr_corrclassified


# SUMMARY OF THE RESULTS
SRconditional_signal_misclass_asym_all_size2000 <- as.array(c(round(SRconditional_signal_misclass_asym_CoordDescGC_size2000, 2), round(SRconditional_signal_misclass_asym_NelderMeadGC_size2000, 2), round(SRconditional_signal_misclass_asym_RefinedRandom_size2000, 2)))
SRconditional_signal_misclass_asym_all_size3000 <- as.array(c(round(SRconditional_signal_misclass_asym_CoordDescGC_size3000, 2), round(SRconditional_signal_misclass_asym_NelderMeadGC_size3000, 2), round(SRconditional_signal_misclass_asym_RefinedRandom_size3000, 2)))
SRconditional_signal_misclass_asym_all_size4000 <- as.array(c(round(SRconditional_signal_misclass_asym_CoordDescGC_size4000, 2), round(SRconditional_signal_misclass_asym_NelderMeadGC_size4000, 2), round(SRconditional_signal_misclass_asym_RefinedRandom_size4000, 2)))

SRconditional_signal_corrclass_asym_all_size2000 <- as.array(c(round(SRconditional_signal_corrclass_asym_CoordDescGC_size2000, 2), round(SRconditional_signal_corrclass_asym_NelderMeadGC_size2000, 2), round(SRconditional_signal_corrclass_asym_RefinedRandom_size2000, 2)))
SRconditional_signal_corrclass_asym_all_size3000 <- as.array(c(round(SRconditional_signal_corrclass_asym_CoordDescGC_size3000, 2), round(SRconditional_signal_corrclass_asym_NelderMeadGC_size3000, 2), round(SRconditional_signal_corrclass_asym_RefinedRandom_size3000, 2)))
SRconditional_signal_corrclass_asym_all_size4000 <- as.array(c(round(SRconditional_signal_corrclass_asym_CoordDescGC_size4000, 2), round(SRconditional_signal_corrclass_asym_NelderMeadGC_size4000, 2), round(SRconditional_signal_corrclass_asym_RefinedRandom_size4000, 2)))
