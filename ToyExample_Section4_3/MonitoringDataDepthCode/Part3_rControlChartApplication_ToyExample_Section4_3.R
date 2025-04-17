# Application of r control charts: Toy Example, Section 4.3


rControlChart <- function(depths, ref_depths, alpha, outofcontrol = FALSE){ 
  # Create containers
  ranks <- c()
  FAR <- -1
  CDR <- -1
  m <- length(ref_depths)
  
  # Compute the ranks
  for(l in 1:length(depths)){
    ranks[l] <- sum(ref_depths <= depths[l]) / m
  }
  if (outofcontrol == TRUE){
    
    # Different definition of correctly and wrongly detected samples:
    
    ## For the out-of-control state 
    FAR <- length(which(ranks > alpha))/length(ranks)
    CDR <- 1 - FAR
  } else {
    ## For the in-control data
    FAR <- length(which(ranks <= alpha))/length(ranks)
    CDR <- 1 - FAR
  }
  
  # Return results 
  
  r_results <- list(ranks = ranks,
                    FAR = FAR,
                    CDR = CDR)
  
  return(r_results)
}


# PHASE I: Refsamples

refsample_rcontrolchart_class0_size100 <- list()

for (i in 1:length(depth_refsample_class0_size100[[1]])){ # Loop of the number of calculated data depth notions
 
  refsample_rcontrolchart_class0_size100[[i]] <- rControlChart(depths = depth_refsample_class0_size100[[1]][[i]], 
                                                               ref_depths = depth_refsample_class0_size100[[1]][[i]],
                                                               alpha = 0.05, outofcontrol = FALSE)
}

refsample_rcontrolchart_class1_size100 <- list()

for (i in 1:length(depth_refsample_class1_size100[[1]])){ # Loop of the number of calculated data depth notions
 
  refsample_rcontrolchart_class1_size100[[i]] <- rControlChart(depths = depth_refsample_class1_size100[[1]][[i]], 
                                                               ref_depths = depth_refsample_class1_size100[[1]][[i]],
                                                               alpha = 0.05,  outofcontrol = FALSE)
}


# PHASE II: In-control (Testdata)

rcontrolchart_testdata_class0_size100 <- list()
for (i in 1:length(depth_time_testdata_class0_size100[[1]])){ # Loop of the number of calculated data depth notions
  
  rcontrolchart_testdata_class0_size100[[i]] <- rControlChart(depths = depth_time_testdata_class0_size100[[1]][[i]], ref_depths = depth_refsample_class0_size100[[1]][[i]],
                                                              alpha = 0.05, outofcontrol = FALSE)
}

rcontrolchart_testdata_class1_size100 <- list()
for (i in 1:length(depth_time_testdata_class1_size100[[1]])){ # Loop of the number of calculated data depth notions
  
  rcontrolchart_testdata_class1_size100[[i]] <- rControlChart(depths = depth_time_testdata_class1_size100[[1]][[i]], ref_depths = depth_refsample_class1_size100[[1]][[i]],
                                                              alpha = 0.05, outofcontrol = FALSE)
}


# PHASE II: Out-of-control (Newly generated data)

rcontrolchart_outofcontrol_size100 <- list()
for (i in 1:length(depth_time_outofcontrol_size100[[1]])){ # Loop of the number of calculated data depth notions
  
  rcontrolchart_outofcontrol_size100[[i]] <- rControlChart(depths = depth_time_outofcontrol_size100[[1]][[i]], ref_depths = depth_refsample_class1_size100[[1]][[i]],
                                                           alpha = 0.05, outofcontrol = TRUE)
}



## Phase I and Phase II: SUMMARY

FAR_refsample_size100 <- list()
FAR_testdata_size100 <- list()
CDR_outofcontrol_size100 <- list()

fraction_testdata_class0 <- length(test_class0_ids)/dim(testdata_embeddings)[1]
fraction_testdata_class1 <- length(test_class1_ids)/dim(testdata_embeddings)[1]

for(i in 1:length(refsample_rcontrolchart_class0_size100)){
  FAR_refsample_size100[[i]] <-  (refsample_rcontrolchart_class0_size100[[i]][["FAR"]] + refsample_rcontrolchart_class1_size100[[i]][["FAR"]])/2
  FAR_testdata_size100[[i]] <- rcontrolchart_testdata_class0_size100[[i]][["FAR"]]*fraction_testdata_class0 + rcontrolchart_testdata_class1_size100[[i]][["FAR"]]*fraction_testdata_class1
  CDR_outofcontrol_size100[[i]] <- rcontrolchart_outofcontrol_size100[[i]][["CDR"]]
}
