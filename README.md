
# Statistical Process Monitoring of Artificial Neural Networks
 This code is the base of the [publication](https://www.tandfonline.com/doi/full/10.1080/00401706.2023.2239886) "Statistical process monitoring of artificial neural networks".


## Description of the available folders
1. Folder "Packages" contains the package "ddalpha" (Version 1.3.14.1) and the package "RobustDepth" (Version 0.1.0) for computing robust Halfspace depth.

2. Folder "ToyExample_Section4_3" contains:
   -  Folder "Data" - two subfolders contain embeddings and predictions/labels used for computing data depth 
							   and other scores from the benchmark.
   - Folder "MonitoringDataDepthCode" - it contains three files for applying r control charts
							   from scratch (`Part1_*`, `Part2_*`, `Part3_*`) and run benchmark computation (Files in the folder "Benchmark").
   -  File "SOURCECODE_ToyExample_Section4_3.R" - core file for getting the results of toy example in Tables 1 and 2 by sourcing 
							   the file that uses files from two described folders (given that the necessary packages are installed).
	 -  Jupyter notebook "ANN_DataSimulation_ToyExample_Section4_3.ipynb" - this shows how to train the artificial
							   neural network taken for toy example and generate the embeddings.

__Please note:__ The simulation after creating the embeddings in Python (Version Python 3.10) was constructed and run in RStudio Version 1.2.1335, with R version 3.6.1 (Platform: x86_64-w64-mingw32/x64 (64-bit)).
The seeds are used for reproducibility, however, the results might differ across different versions of Python or R. Also, in R it would round down in case of the even decimal, e.g. 0.045 -> 0.04. Thus, in Table 1, SD = 0.03 in toy example 
but in df_results_toyexample in R it would be rounded with round(x, 2) from 0.025 to 0.02 for Phase II, in-control.


3. Folder "Experiment1_Section4_4_1" contains:
     -  Folder "NeuralNetworkCode" - two Jupyter notebooks show how to design and train a convolutional neural network ("1.CNN_ModelTraining_Experiment1_Section4_4_1.ipynb")
							   and how to create out-of-control behavior as well as generate and save the embeddings ("2.CNN_EmbeddingsGeneration_Experiment1_Section4_4_1.ipynb").
							   Here, the notebooks were created and run in Python 3.6.9.
     -  Folder "MonitoringDataDepthCode" - it contains eight files for applying r control charts
							   for each type of depth considered in Experiment 1 (`ControlChartResults_*`), a file that runs an example of how to compute depths 
 							   in Experiment 1, given that the necessary packages are installed ("Example_Calculation_Depths.R"), six files related to the misclassification analysis ("Misclassification_Analysis_*.R"),
							   and a file "FinalResults_Table_Experiment1_Section_4_4_1.R" that summarizes the Table 1, Experiment 1 results by sourcing a core file.
   -  File "SOURCECODE_Experiment1_Section4_4_1" - core file for getting the results of Experiment 1 in Tables 1 and 2 by sourcing 
							   the file that uses files from the folder "MonitoringDataDepthCode". It also runs examples of how to compute depths and apply benchmark methods,
						           given that the necessary packages are installed.			

__Please note:__ As the computation of depths occurred on the cluster for Experiment 1, we take the complete depth results saved as `.RData` in Folder "MonitoringDataDepthCode" and directly proceed with the computation of r control charts. 
