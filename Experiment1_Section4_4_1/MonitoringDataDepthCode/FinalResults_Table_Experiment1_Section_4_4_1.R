
# After source(*) the r control chart files and misclassification analysis, create the final table:


df_results_experiment1 <- data.frame(matrix(ncol = 13, nrow = 0))
names_col <- c("Size|R|", "Phase", "Observed process", "Metric", "MD", "SD", "HDr", "APD1", "APD2", "APD3", "PD1", "PD2", "PD3")
colnames(df_results_experiment1) <- names_col


df_results_experiment1[1, ]  <- c("2000", "I", "In-control", "FAR", MD_size2000[1], "/", HD_r_size2000[1], PD_1a_size2000[1], PD_2a_size2000[1], PD_3a_size2000[1],
                                  PD_1_size2000[1], PD_2_size2000[1], PD_3_size2000[1])

df_results_experiment1[2, ]  <- c("2000", "II", "In-control", "SR", MD_size2000[2], "/", HD_r_size2000[2], PD_1a_size2000[2], PD_2a_size2000[2], PD_3a_size2000[2],
                                  PD_1_size2000[2], PD_2_size2000[2], PD_3_size2000[2])


df_results_experiment1[3, ]  <- c(" ", " ", " ", "SR|M", SRconditional_signal_misclass_MD_allsizes[1], "/", SRconditional_signal_misclass_rHD_allsizes[1],
                                  SRconditional_signal_misclass_asym_all_size2000[1], SRconditional_signal_misclass_asym_all_size2000[2], 
                                  SRconditional_signal_misclass_asym_all_size2000[3], SRconditional_signal_misclass_PD_1_allsizes[1], 
                                  SRconditional_signal_misclass_PD_2_allsizes[1], SRconditional_signal_misclass_PD_3_allsizes[1])

df_results_experiment1[4, ]  <- c(" ", " ", " ", "SR|C", SRconditional_signal_corrclass_MD_allsizes[1], "/", SRconditional_signal_corrclass_rHD_allsizes[1],
                                  SRconditional_signal_corrclass_asym_all_size2000[1], SRconditional_signal_corrclass_asym_all_size2000[2], 
                                  SRconditional_signal_corrclass_asym_all_size2000[3], SRconditional_signal_corrclass_PD_1_allsizes[1], 
                                  SRconditional_signal_corrclass_PD_2_allsizes[1], SRconditional_signal_corrclass_PD_3_allsizes[1])

df_results_experiment1[5, ]  <- c("2000", "II", "Out-of-control", "CDR", MD_size2000[3], "/", HD_r_size2000[3], PD_1a_size2000[3], PD_2a_size2000[3], PD_3a_size2000[3],
                                  PD_1_size2000[3], PD_2_size2000[3], PD_3_size2000[3])


df_results_experiment1[6, ]  <- c("3000", "I", "In-control", "FAR", MD_size3000[1], "/", HD_r_size3000[1], PD_1a_size3000[1], PD_2a_size3000[1], PD_3a_size3000[1],
                                  PD_1_size3000[1], PD_2_size3000[1], PD_3_size3000[1])

df_results_experiment1[7, ]  <- c("3000", "II", "In-control", "SR", MD_size3000[2], "/", HD_r_size3000[2], PD_1a_size3000[2], PD_2a_size3000[2], PD_3a_size3000[2],
                                  PD_1_size3000[2], PD_2_size3000[2], PD_3_size3000[2])


df_results_experiment1[8, ]  <- c(" ", " ", " ", "SR|M", SRconditional_signal_misclass_MD_allsizes[2], "/", SRconditional_signal_misclass_rHD_allsizes[2],
                                  SRconditional_signal_misclass_asym_all_size3000[1], SRconditional_signal_misclass_asym_all_size3000[2], 
                                  SRconditional_signal_misclass_asym_all_size3000[3], SRconditional_signal_misclass_PD_1_allsizes[2], 
                                  SRconditional_signal_misclass_PD_2_allsizes[2], SRconditional_signal_misclass_PD_3_allsizes[2])

df_results_experiment1[9, ]  <- c(" ", " ", " ", "SR|C", SRconditional_signal_corrclass_MD_allsizes[2], "/", SRconditional_signal_corrclass_rHD_allsizes[2],
                                  SRconditional_signal_corrclass_asym_all_size3000[1], SRconditional_signal_corrclass_asym_all_size3000[2], 
                                  SRconditional_signal_corrclass_asym_all_size3000[3], SRconditional_signal_corrclass_PD_1_allsizes[2], 
                                  SRconditional_signal_corrclass_PD_2_allsizes[2], SRconditional_signal_corrclass_PD_3_allsizes[2])

df_results_experiment1[10, ]  <- c("3000", "II", "Out-of-control", "CDR", MD_size3000[3], "/", HD_r_size3000[3], PD_1a_size3000[3], PD_2a_size3000[3], PD_3a_size3000[3],
                                   PD_1_size3000[3], PD_2_size3000[3], PD_3_size3000[3])


df_results_experiment1[11, ]  <- c("4000", "I", "In-control", "FAR", MD_size4000[1], "/", HD_r_size4000[1], PD_1a_size4000[1], PD_2a_size4000[1], PD_3a_size4000[1],
                                   PD_1_size4000[1], PD_2_size4000[1], PD_3_size4000[1])

df_results_experiment1[12, ]  <- c("4000", "II", "In-control", "SR", MD_size4000[2], "/", HD_r_size4000[2], PD_1a_size4000[2], PD_2a_size4000[2], PD_3a_size4000[2],
                                   PD_1_size4000[2], PD_2_size4000[2], PD_3_size4000[2])


df_results_experiment1[13, ]  <- c(" ", " ", " ", "SR|M", SRconditional_signal_misclass_MD_allsizes[3], "/", SRconditional_signal_misclass_rHD_allsizes[3],
                                   SRconditional_signal_misclass_asym_all_size4000[1], SRconditional_signal_misclass_asym_all_size4000[2], 
                                   SRconditional_signal_misclass_asym_all_size4000[3], SRconditional_signal_misclass_PD_1_allsizes[3], 
                                   SRconditional_signal_misclass_PD_2_allsizes[3], SRconditional_signal_misclass_PD_3_allsizes[3])

df_results_experiment1[14, ]  <- c(" ", " ", " ", "SR|C", SRconditional_signal_corrclass_MD_allsizes[3], "/", SRconditional_signal_corrclass_rHD_allsizes[3],
                                   SRconditional_signal_corrclass_asym_all_size4000[1], SRconditional_signal_corrclass_asym_all_size4000[2], 
                                   SRconditional_signal_corrclass_asym_all_size4000[3], SRconditional_signal_corrclass_PD_1_allsizes[3], 
                                   SRconditional_signal_corrclass_PD_2_allsizes[3], SRconditional_signal_corrclass_PD_3_allsizes[3])

df_results_experiment1[15, ]  <- c("4000", "II", "Out-of-control", "CDR", MD_size4000[3], "/", HD_r_size4000[3], PD_1a_size4000[3], PD_2a_size4000[3], PD_3a_size4000[3],
                                   PD_1_size4000[3], PD_2_size4000[3], PD_3_size4000[3])

View(df_results_experiment1)