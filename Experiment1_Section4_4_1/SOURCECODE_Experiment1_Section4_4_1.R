
#-------------------------------------------Source code for Table 1 and Table 2, Experiment 1 in Section 4.4.1---------------------------------------------#


## For Information: system.time(source("SOURCECODE_Experiment1_Section4_4_1.R")) returned
# user        system    elapsed 
#147.06        1.41      148.06
#processor: Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz

# Set your main directory where the folder "Manuscript_Code" is unpacked, including it in the end of the variable dir_main, e.g., "D:/Documents/Manuscript_Code"

dir_main <- "D:/SupplementaryMaterial/Manuscript_Code/"

# Install the most recent version of ddalpha package and the package for computing robust Halfspace depth (below is an example)
#install.packages("D:/Manuscript_Code/Packages/ddalpha_1.3.14.1.tar.gz", repos = NULL, type="source")
#install.packages("D:/Manuscript_Code/Packages/RobustDepth_0.1.0.tar.gz", repos = NULL, type="source")

# set a seed to get the same results as in the manuscript 

set.seed(23)

# Used libraries in the seperate files
library(ddalpha)
library(DDoutlier)
library(dplyr)
library(isotree)
library(microbenchmark)
library(RobustDepth)


# DATA DEPTH PART
setwd(paste0(dir_main,"Experiment1_Section4_4_1/MonitoringDataDepthCode", sep =""))

# Examples of Data Depth Computation: 
source("Example_Calculation_Depths.R")

# Application of r Control Charts and Misclassification Analysis that is based on the output of the respective file "ControlChartResults_*.R":

source("ControlChartResults_MahalanobisDepth.R")
source("Misclassification_Analysis_MahalanobisDepth.R")

source("ControlChartResults_RobustHalfspaceDepth.R")
source("Misclassification_Analysis_RobustHalfspaceDepth.R")

source("ControlChartResults_ProjectionDepthCoordDescAsymmetric.R")
source("ControlChartResults_ProjectionDepthNelderMeadAsymmetric.R")
source("ControlChartResults_ProjectionDepthRefRandomAsymmetric.R")
source("Misclassification_Analysis_AsymmetricProjectionDepth_all.R")

source("ControlChartResults_ProjectionDepthCoordDesc.R")
source("Misclassification_Analysis_ProjectionDepthCoordDesc.R")

source("ControlChartResults_ProjectionDepthNelderMead.R")
source("Misclassification_Analysis_ProjectionNelderMead.R")

source("ControlChartResults_ProjectionDepthRefinedRandom.R")
source("Misclassification_Analysis_ProjectionRefinedRandom.R")


# FINAL RESULTS, TABLE 1: Experiment 1 (Section 4.4.1)
source("FinalResults_Table_Experiment1_Section_4_4_1.R")





