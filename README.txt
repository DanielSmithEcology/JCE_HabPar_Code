==========================================================================================================================
==========================================================================================================================
Code for: On the interaction between Janzen-Connell effects and habitat partitioning in spatially structured environments
==========================================================================================================================
==========================================================================================================================

Author:  Daniel J. B. Smith 
Contact: smithdj4@arizona.edu 

===============================================================================================================================
Contents / summary: 
===============================================================================================================================
- This document contains descriptions for the code and files necessary to reproduce the paper "On the interaction between specialized natural enemies and habitat partitioning in spatially structured environments"
- All code is written in R. You should use R studio to run the code. 
- There are a number of R packages that must be installed before much of the code will work. 
- Most of the code is heavily commented. Therefore, issues not addressed here are likely addressed in the R scripts. 
- If there are any problems, please contact me at smithdj4@arizona.edu. 
- Thanks for taking the time to take a closer look at my study! 

===============================================================================================================================
"Main_Text_Code" (Folder) 
===============================================================================================================================
- "Fig_1.R", "Fig_2.R", "Fig_3.R", "Fig_4.R", "Fig_5.R" 
	- R scripts that will reproduce Figs. 1-5. The R scripts should be opened in R Studio.
	- Each script will save a SVG file of the figure in your working directory
	- For "Fig_1.R", "Fig_2.R", "Fig_3.R", and "Fig_5.R", "CSV_Files" must be set as the working directory 
	- Most of the code is relatively self-explanitory, and is simply a matter of visualization 

- "Habitat_Generation.R"
	- R script that will reproduce the different habitats (different levels of autocorrelation)
	- It will do so using the "NLMR" package in R, which uses a midpoint displacement algorithm
	- The script will save a CSV file of the each habitat to your working directory 
	- NOTE: the script produces "High", "MedHigh", "Med", "MedLow", and "Low" spatial autocorrelation plots. 
		- The main text uses "MedHigh", "Med", "MedLow" for the plots labeled "High", "Medium", and "Low".
		- "Random" landscapes are implemented by randomizing/scrambling the "Med" landscape (noting the choice makes no difference). 
		- This was done to simplify the number of plots. The percise landscapes chosen makes no qualitative difference. 

===============================================================================================================================
"Appendix_Code" (Folder) 
===============================================================================================================================
- "Appendix_Box_Plot.R" 
	- Will reproduce Fig. S1 from the SI Appendix

- "Appendix_InvasionApprox_S2.R" and "Appendix_InvasionApprox_S3.R"
	- Will reproduce Figs. S2 and S3 from the SI Appendix, respectively 
	- "CSV_Files" must be set as the working directory
	- Code will load all of the SEM simulations and then calculate the invader metric
	- The code will run both the exact value (as described in the SI Appendix) and Approximation
	- Note that you need to choose which R(Sigma, N) function do run the approximation with (both functions are provided in the code) 
	- Running this code takes quite a while

- "Appendix_TS_Plots.R" 
	- Will reproduce Figs. S4-S11 from the SI Appendix
	- "CSV_Files" must be set as the working directory

- "Appendix_Shannon_Diversity.R"
	- Will reproduce Fig. S12
	- In essence, the same as "Fig_3.R", but plots Shannon diversity instead of species diversity 
	- "CSV_Files" must be set as the working directory

===============================================================================================================================
"CSV_Files" (Folder) 
===============================================================================================================================
- This folder contains .CSV files of the SEM simulations outputs and habitats 

- The "Habitat_Generation.R" will reproduce the different habitat files

- The R scripts in the "Simulation_Runs" folder will produce all the SEM simulation outputs

- In general, I recommend that you set "CSV_Files" to your working directory when you're running the above R scripts, as nearly every script uses data from it to produce the figures / analyses. 

===============================================================================================================================
"Simulation_Runs" (Folder) 
===============================================================================================================================
- Contains all of the R scripts used for the Spatially Explicit Model (SEM) simulations 
- These will take a while to run -- the "CSV_Files" folder contain all outputs from this code
- Different files are named to reflect which simulations they run
- Each R script contains a number of functions that implement habitat partioning, Janzen-Conell effects, etc.  


=============================================================================================================================
=============================================================================================================================



