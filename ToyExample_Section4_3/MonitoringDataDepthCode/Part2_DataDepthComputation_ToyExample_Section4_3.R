# Depth computation: Toy Example, Section 4.3

DepthCalculation <- function(z, refSamples, seed_selected, DepthforRef = FALSE) {
  
  # Containers for Depth Calculations
  prj_CoordDescGC <- c()
  prj_NelderMeadGC <- c()
  prj_RefinedRandom <- c()
  
  asym_CoordDescGC <- c()
  asym_NelderMeadGC <- c()
  asym_RefinedRandom <- c()
  
  halfspace_robust <- c()
  simplicial_exact <- c()
  mahalanobis_exact <- c()
  
  
  # Containers for Time Measurements
  tmpTimesCoordDescGC <- c()
  tmpTimesNelderMeadGC <- c()
  tmpTimesRefinedRandom <- c()
  
  tmpTimesAsymCoordDescGC <- c()
  tmpTimesAsymNelderMeadGC <- c()
  tmpTimesAsymRefinedRandom <- c()
  
  tmpTimeshalfspace_robust <- c()
  tmpTimessimplicial_exact <- c()
  tmpTimesmahalanobis_exact <- c()
  
  
  
  
  dim_nr <- as.numeric(dim(z)[2])
  
  for (i in 1:dim(z)[1]){
    if (DepthforRef == TRUE){
      X <- matrix(unlist(refSamples[-i, ]), ncol = dim_nr, byrow = FALSE)
    } else {X <- matrix(unlist(refSamples), ncol = dim_nr, byrow = FALSE)}
    
    
    
    ## Approximative calculation
    
    # Projection Depth
    tmpTimesCoordDescGC[i] <- microbenchmark(prj_CoordDescGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 10000, seed = seed_selected, depth = "projection", solver = "CoordDesc",
                                                                                  solverParams = list(space = "Sp", LS = "GS")), times = 1)$time * 1e-9
    
    tmpTimesNelderMeadGC[i] <- microbenchmark(prj_NelderMeadGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 10000, seed = seed_selected, depth = "projection", solver = "NelderMead",
                                                                                    solverParams = list(space = "Sp", start = "Mn", beta = 1, bound = "y")), times = 1)$time * 1e-9
    
    
    tmpTimesRefinedRandom[i] <- microbenchmark(prj_RefinedRandom[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 10000, seed = seed_selected, depth = "projection", solver = "RefinedRandom",
                                                                                      solverParams = list(nref = 10, alpha = 0.5)), times = 1)$time * 1e-9
    
    
    
    # Asymmetric Projection Depth
    
    tmpTimesAsymCoordDescGC[i] <- microbenchmark(asym_CoordDescGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 10000, seed = seed_selected, depth = "asymmprojection", solver = "CoordDesc",
                                                                                       solverParams = list(space = "Sp", LS = "GS")), times = 1)$time * 1e-9
    
    tmpTimesAsymNelderMeadGC[i] <- microbenchmark(asym_NelderMeadGC[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 10000, seed = seed_selected, depth = "asymmprojection", solver = "NelderMead",
                                                                                         solverParams = list(space = "Sp", start = "Mn", beta = 1, bound = "y")), times = 1)$time * 1e-9
    
    
    
    tmpTimesAsymRefinedRandom[i] <- microbenchmark(asym_RefinedRandom[i] <- depth.projprop(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, num.directions = 10000, seed = seed_selected, depth = "asymmprojection", solver = "RefinedRandom",
                                                                                           solverParams = list(nref = 10, alpha = 0.5)), times = 1)$time * 1e-9
    
    
    # Robust Halfspace Depth 
    tmpTimeshalfspace_robust[i] <- microbenchmark(halfspace_robust[i] <- RobustDepth::depth.RobustTukey(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), matrix(unlist(X), ncol = dim_nr, byrow = FALSE), exact = FALSE, num.dir = 10000), times = 1)$time * 1e-9
    
    
    
    ## Exact Computation
    
    # Simplicial Depth
    tmpTimessimplicial_exact[i] <- microbenchmark(simplicial_exact[i] <- depth.simplicial(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, exact = T), times = 1)$time * 1e-9
    
    # Mahalanobis Depth
    tmpTimesmahalanobis_exact[i] <- microbenchmark(mahalanobis_exact[i] <- depth.Mahalanobis(matrix(unlist(z[i, ]), ncol = dim_nr, byrow = FALSE), X, mah.estimate = "moment"), times = 1)$time * 1e-9
    
    
  } 
  depth_results <- list(mahalanobis_exact = mahalanobis_exact,
                        simplicial_exact = simplicial_exact,
                        halfspace_robust = halfspace_robust,
                        
                        asym_CoordDescGC = asym_CoordDescGC,
                        asym_NelderMeadGC = asym_NelderMeadGC,
                        asym_RefinedRandom = asym_RefinedRandom,
                        
                        prj_CoordDescGC = prj_CoordDescGC,
                        prj_NelderMeadGC = prj_NelderMeadGC,
                        prj_RefinedRandom = prj_RefinedRandom)
  
  
  time_results <- list(tmpTimesmahalanobis_exact = tmpTimesmahalanobis_exact,
                       tmpTimessimplicial_exact = tmpTimessimplicial_exact,
                       tmpTimeshalfspace_robust = tmpTimeshalfspace_robust,
                       
                       tmpTimesAsymCoordDescGC = tmpTimesAsymCoordDescGC,
                       tmpTimesAsymNelderMeadGC = tmpTimesAsymNelderMeadGC,
                       tmpTimesAsymRefinedRandom = tmpTimesAsymRefinedRandom,
                       
                       tmpTimesCoordDescGC = tmpTimesCoordDescGC,
                       tmpTimesNelderMeadGC = tmpTimesNelderMeadGC,
                       tmpTimesRefinedRandom = tmpTimesRefinedRandom)
  
  
  
  
  
  return(list(depth_results, time_results))    
}

# Compute depths for reference samples

seed_i <- 10

depth_refsample_class0_size100 <- DepthCalculation(refsample_class0, refsample_class0, seed_selected = seed_i, DepthforRef = TRUE)
depth_refsample_class1_size100 <- DepthCalculation(refsample_class1, refsample_class1, seed_selected = seed_i, DepthforRef = TRUE)

# Calculate depth for test samples

depth_time_testdata_class0_size100 <- DepthCalculation(z = z_test_class0, refSamples = refsample_class0, seed_selected = seed_i, DepthforRef = FALSE)
depth_time_testdata_class1_size100 <- DepthCalculation(z = z_test_class1, refSamples = refsample_class1, seed_selected = seed_i, DepthforRef = FALSE)

# Calculate depth for outofcontrol samples
depth_time_outofcontrol_size100  <- DepthCalculation(z = outofcontroldata_embeddings, refSamples = refsample_class1, seed_selected = seed_i, DepthforRef = FALSE) # Because all predicted labels were 1

