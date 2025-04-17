
# Example of data depth computation in Experiment 1, Section 4.4.1

load("EmbeddingsPredictedLabels_Experiment1_Section4_4_1.RData")


DepthCalculation <- function(z, refSamples, seed_selected, DepthforRef = FALSE) {
  
  # Containers for Depth Calculations
  prj_CoordDescGC <- c()
  prj_NelderMeadGC <- c()
  prj_RefinedRandom <- c()
  
  asym_CoordDescGC <- c()
  asym_NelderMeadGC <- c()
  asym_RefinedRandom <- c()
  
  halfspace_robust <- c()
  mahalanobis_exact <- c()
  
  
  # Containers for Time Measurements
  tmpTimesCoordDescGC <- c()
  tmpTimesNelderMeadGC <- c()
  tmpTimesRefinedRandom <- c()
  
  tmpTimesAsymCoordDescGC <- c()
  tmpTimesAsymNelderMeadGC <- c()
  tmpTimesAsymRefinedRandom <- c()
  
  tmpTimeshalfspace_robust <- c()
  tmpTimesmahalanobis_exact <- c()
  
  
  
  
  dim_nr <- as.numeric(dim(z)[2])
  
  for (i in 1:dim(z)[1]){
    if (DepthforRef == TRUE){
      X <- matrix(unlist(refSamples[-i, ]), ncol = dim_nr, byrow = FALSE)
    } else {X <- matrix(unlist(refSamples), ncol = dim_nr, byrow = FALSE)}
    
    
    
    ## Approximative calculation
    
    # Projection Depth
    tmpTimesCoordDescGC[i] <- microbenchmark(prj_CoordDescGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 100000, seed = seed_selected, depth = "projection", solver = "CoordDesc",
                                                                                  solverParams = list(space = "Sp", LS = "GS")), times = 1)$time * 1e-9
    
    tmpTimesNelderMeadGC[i] <- microbenchmark(prj_NelderMeadGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 100000, seed = seed_selected, depth = "projection", solver = "NelderMead",
                                                                                    solverParams = list(space = "Sp", start = "Mn", beta = 1, bound = "y")), times = 1)$time * 1e-9
    
    
    tmpTimesRefinedRandom[i] <- microbenchmark(prj_RefinedRandom[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 100000, seed = seed_selected, depth = "projection", solver = "RefinedRandom",
                                                                                      solverParams = list(nref = 10, alpha = 0.5)), times = 1)$time * 1e-9
    
    
    
    # Asymmetric Projection Depth
    
    tmpTimesAsymCoordDescGC[i] <- microbenchmark(asym_CoordDescGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 100000, seed = seed_selected, depth = "asymmprojection", solver = "CoordDesc",
                                                                                       solverParams = list(space = "Sp", LS = "GS")), times = 1)$time * 1e-9
    
    tmpTimesAsymNelderMeadGC[i] <- microbenchmark(asym_NelderMeadGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 100000, seed = seed_selected, depth = "asymmprojection", solver = "NelderMead",
                                                                                         solverParams = list(space = "Sp", start = "Mn", beta = 1, bound = "y")), times = 1)$time * 1e-9
    
    
    
    tmpTimesAsymRefinedRandom[i] <- microbenchmark(asym_RefinedRandom[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 100000, seed = seed_selected, depth = "asymmprojection", solver = "RefinedRandom",
                                                                                           solverParams = list(nref = 10, alpha = 0.5)), times = 1)$time * 1e-9
    
    
    # Robust Halfspace Depth 
    tmpTimeshalfspace_robust[i] <- microbenchmark(halfspace_robust[i] <- RobustDepth::depth.RobustTukey(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), matrix(unlist(X), ncol = dim_nr, byrow = FALSE), exact = FALSE, num.dir = 100000), times = 1)$time * 1e-9
    
    
    
    ## Exact Computation
    
    # Mahalanobis Depth
    tmpTimesmahalanobis_exact[i] <- microbenchmark(mahalanobis_exact[i] <- depth.Mahalanobis(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, mah.estimate = "moment"), times = 1)$time * 1e-9
    
    
  } 
  depth_results <- list(mahalanobis_exact = mahalanobis_exact,
                        halfspace_robust = halfspace_robust,
                        
                        asym_CoordDescGC = asym_CoordDescGC,
                        asym_NelderMeadGC = asym_NelderMeadGC,
                        asym_RefinedRandom = asym_RefinedRandom,
                        
                        prj_CoordDescGC = prj_CoordDescGC,
                        prj_NelderMeadGC = prj_NelderMeadGC,
                        prj_RefinedRandom = prj_RefinedRandom)
  
  
  time_results <- list(tmpTimesmahalanobis_exact = tmpTimesmahalanobis_exact,
                       tmpTimeshalfspace_robust = tmpTimeshalfspace_robust,
                       
                       tmpTimesAsymCoordDescGC = tmpTimesAsymCoordDescGC,
                       tmpTimesAsymNelderMeadGC = tmpTimesAsymNelderMeadGC,
                       tmpTimesAsymRefinedRandom = tmpTimesAsymRefinedRandom,
                       
                       tmpTimesCoordDescGC = tmpTimesCoordDescGC,
                       tmpTimesNelderMeadGC = tmpTimesNelderMeadGC,
                       tmpTimesRefinedRandom = tmpTimesRefinedRandom)
  
  
  
  
  
  return(list(depth_results, time_results))    
}

# Phase I: Example 
# Compute depths for one randomly selected point from a reference sample class 1 (|R| = 2000)

seed_i <- 10
dim_embed <- dim(train_samples_2000[[1]])[2]

system.time(example_PhaseI_refsample_size2000 <- DepthCalculation(matrix(unlist(train_samples_2000[[1]][sample(1:2000, 1), ]), ncol = dim_embed, byrow = FALSE), as.matrix(train_samples_2000[[1]]), seed_selected = seed_i, DepthforRef = TRUE))

# Phase II, in-control: Example 
# Compute depths for the first point from in-control data, Phase II (test data) with respect to a reference sample of class_pred (|R| = 2000)
#df_test_labels: V11 - predictions, V12 - true labels
class_pred <- df_test_labels$V11[1] + 1
system.time(example_PhaseII_incontrol_size2000 <- DepthCalculation(df_test_embeds[1, ], as.matrix(train_samples_2000[[class_pred]]), seed_selected = seed_i, DepthforRef = FALSE))


# Phase II, out-of-control: Example 
# Compute depths for the first point from in-control data, Phase II (test data) with respect to a reference sample of class_pred (|R| = 2000)
class_pred_outofcontrol <- outofcontrol_predlabels$V1[1] + 1
system.time(example_PhaseII_outofcontrol_size2000 <- DepthCalculation(outofcontrol_embeddings[1, ], as.matrix(train_samples_2000[[class_pred_outofcontrol]]), seed_selected = seed_i, DepthforRef = FALSE))

# SUMMARY OF THE RESULTS 
print(paste0("Average depth of a point from a reference sample (Phase I) is ", round(mean(unlist(example_PhaseI_refsample_size2000[[1]])), digits = 2)))
print(paste0("Average depth of the first point from Phase II, in-control part is ", round(mean(unlist(example_PhaseII_incontrol_size2000[[1]])), digits = 2)))
print(paste0("Average depth of the first point from Phase II, out-of-control part is ", round(mean(unlist(example_PhaseII_outofcontrol_size2000[[1]])), digits = 2)))
