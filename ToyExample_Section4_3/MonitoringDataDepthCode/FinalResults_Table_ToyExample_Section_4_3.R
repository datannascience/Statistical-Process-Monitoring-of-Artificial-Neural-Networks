# Create the summary as a table of the monitoring using r control charts and various depth notions


df_results_toyexample <- data.frame(matrix(ncol = 13, nrow = 0))
names_col <- c("Size|R|", "Phase", "Observed process", "Metric", "MD", "SD", "HDr", "APD1", "APD2", "APD3", "PD1", "PD2", "PD3")
colnames(df_results_toyexample) <- names_col

df_results_toyexample[1, ] <- c(perclass_refsize, "I", "In-Control", "FAR",  round(unlist(FAR_refsample_size100), 2))

df_results_toyexample[2, ] <- c(perclass_refsize, "II", "In-Control", "SR",  round(unlist(FAR_testdata_size100), 2))

df_results_toyexample[3, ] <- c(perclass_refsize, "II", "Out-of-control", "CDR", round(unlist(CDR_outofcontrol_size100), 2))

View(df_results_toyexample)