
#---------------------------------------------Source code for Table 1 and Table 2, Toy Example simulation study in Section 4.3---------------------------------------------#


## For Information: system.time(source("SOURCECODE_ToyExample_Section4_3.R")) returned
# user        system    elapsed 
#448.25        0.97      449.11 
#processor: Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz

# Set your main directory where the folder "Manuscript_Code" is unpacked, including it in the end of the variable dir_main, e.g., "D:/Documents/Manuscript_Code/"

dir_main <- "D:/SupplementaryMaterial/Manuscript_Code/"

setwd(dir_main)

# set a seed to get the same results as in the manuscript 
# PLEASE NOTE: The simulation was created and run in RStudio Version 1.2.1335, with R version 3.6.1 (Platform: x86_64-w64-mingw32/x64 (64-bit))

set.seed(23)

# Install the most recent version of ddalpha package and the package for computing robust Halfspace depth (below is an example)
#install.packages("D:/Packages/ddalpha_1.3.14.1.tar.gz", repos = NULL, type="source")
#install.packages("D:/Packages/RobustDepth_0.1.0.tar.gz", repos = NULL, type="source")


# Used libraries in the seperate files
library(ddalpha)
library(DDoutlier)
library(isotree)
library(microbenchmark)
library(RobustDepth)



# Part 1 - Data Preparation for the Monitoring/Outlyingness computation: 
## Our approach:
source("ToyExample_Section4_3/MonitoringDataDepthCode/Part1_DataPreparationMonitoring_ToyExample_Section4_3.R")

## Benchmark:
source("ToyExample_Section4_3/MonitoringDataDepthCode/Benchmark/EmbeddingsApplication_Benchmark_ToyExample_Section4_3.R")


# Part 2 - Data Depth computation: 
## Our approach:
source("ToyExample_Section4_3/MonitoringDataDepthCode/Part2_DataDepthComputation_ToyExample_Section4_3.R")
## For Information: system.time(source("Part2_DataDepthComputation_ToyExample_Section4_3.R")) returned
## User 424.02 System 1.03 elapsed 424.35 (processor: Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz)

# Part 3 - r Control Chart application: 
source("ToyExample_Section4_3/MonitoringDataDepthCode/Part3_rControlChartApplication_ToyExample_Section4_3.R")

## Benchmark:
source("ToyExample_Section4_3/MonitoringDataDepthCode/Benchmark/EmbeddingsPerformance_Benchmark_ToyExample_Section4_3.R")
source("ToyExample_Section4_3/MonitoringDataDepthCode/Benchmark/ScoresApplicationPerformance_Benchmark_ToyExample_Section4_3.R")


# FINAL RESULTS, TABLE 1: Simulation study - Toy Example (Section 4.3)
source("ToyExample_Section4_3/MonitoringDataDepthCode/FinalResults_Table_ToyExample_Section_4_3.R")



# FINAL RESULTS: TABLE 2: Comparative study - Toy Example (Section 4.3)
source("ToyExample_Section4_3/MonitoringDataDepthCode/Benchmark/Benchmark_Table_ToyExample_Section_4_3.R")


