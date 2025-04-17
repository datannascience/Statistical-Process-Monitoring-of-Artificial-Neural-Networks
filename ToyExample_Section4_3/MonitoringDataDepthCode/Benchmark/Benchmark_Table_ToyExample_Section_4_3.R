
# Summary of the results obtained by using benchmark methods for monitoring

df_results_toyexample_comparstudy <- data.frame(matrix(ncol = 9, nrow = 0))
names_col_comparstudy <- c("Size|R|", "Phase", "Observed process", "Metric", "MDis", "NOF", "KDEOS", "LOF", "iForest")
colnames(df_results_toyexample_comparstudy) <- names_col_comparstudy


df_results_toyexample_comparstudy[1, ] <- c(perclass_refsize, "I", "In-Control", "FAR", round(MDis_FAR_toyexample, 2), round(NOF_FAR_toyexample, 2), round(FAR_refsample_comparisonmethods, 2))

df_results_toyexample_comparstudy[2, ] <- c(perclass_refsize, "II", "In-Control", "SR", round(MDis_SR_toyexample, 2), round(NOF_SR_toyexample, 2), round(SR_testdata_comparisonmethods, 2))

df_results_toyexample_comparstudy[3, ] <- c(perclass_refsize, "II", "Out-of-control", "CDR", round(MDis_CDR_toyexample, 2), round(NOF_CDR_toyexample, 2), round(CDR_outofcontrol_comparisonmethods, 2))

View(df_results_toyexample_comparstudy)