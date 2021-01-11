# MMVPK
README: 

These scripts relate to specific parts of the presentation. The presentation is annotated to show which scripts produced which files. 
In general I did explore the data in a variety of ways and i've chosen to leave in exploratory analysis that I didn't later present. 

Dataclean.R is the script I used to explore the data initially. It's not a script that is involved directly in any of the final data I will present, but i've left it in as it may be a useful insight into my workflow and general thought process. 

Functions.R is the script that carries key functions used by other scripts. This is sourced where needed by other scripts. 

Note that for data privacy I have not uploaded the provided data-set to github 

Plotting.R is many of the simple plots that are shown early in the presentation 

ConcAnalysis.R is more detailed analysis of the concentration data

PKAnalysis.R is compartmental models with mrgsolve. I would advise against sourcing this entire script as the mrgsolve models will take a long time to run

PDAnalysis.R is a more detailed analysis of drug response data 

