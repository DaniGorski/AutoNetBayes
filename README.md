# AutoNetBayes
AutoNetBayes is a tool developed for conducting Bayesian network meta-analyses using the R programming language. It simplifies the process by allowing users to input data, configure a few commands, and let the system automatically perform the analysis. The results are then presented in structured tables and graphs.

# 1. Use Instructions
1. Initially, it is necessary to install R software and R-Studio platform, which can be downloaded from: https://posit.co/download/rstudio-desktop/. Is recommended keep the R and Rstudio updated.
   
2. Download the R script from: https://github.com/DaniGorski/AutoNetBayes/blob/main/AutoNetBayes%20EN.R

3. The following packages are necessary for execute the shiny interface:
 •	shinyjs, shiny, rio, gemtc, dplyr, purrr, tidyr, openxlsx, rsvg, svglite, network, ggplot2, stringr, igraph, DT, tibble e rjags.

# 2. Use and data organization
The data for each intervention in the study should be organized so that each set of information occupies one row. The file should contain the following columns:
•	Study Identification: Column named study. In the case of crossover studies, name the different periods with equal numbers followed by different letters (e.g., 1A and 1B)
•	Treatments: Column named treatment. Do not use special characters or spaces in the names of interventions.
•	Number of Participants: Column named sampleSize.
•	Outcomes:
    Dichotomous: Column responders (number of patients with the outcome).
  	Continuous: Column mean (mean) and std.dev (standard deviation or standard error).
    Survival: Column responders (number of patients with the outcome) and, if applicable, column exposure.

After uploading the data, the user must enter the type of outcome (dichotomous, continuous or survival) and can use the following functions:
•	Check the network connection
•	Check the name of the requires colunms 
•	Built the network diagram
•	CCalculate network metrics
•	Build a bayesian model 
•	Check the model convergence
•	Check the incoherence in the network
•	Visualization and export the league table
•	Visualization and export of the SUCRA



 

