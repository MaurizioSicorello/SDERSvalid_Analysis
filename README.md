# Psychometric validation and daily life assessment of State Difficulties in Emotion Regulation (S-DERS)

## Project Description
This repository contains data and analysis code for an empirical research project on State Difficulties in Emotion Regulation (S-DERS). This project comprises: 
-the validation of a German version of the S-DERS questionnaire by Lavender et al. (2017) following a mood induction
-an application of the questionnaire to daily life using ecological momentary assessment
Details on the project can be found in the respective preprints and publications (see below). 
The core project contributers are Maurizio Sicorello, David Kolar, Moritz Elsässer, Luise Prüßner, and Wiebke Heyse

## Reproducibility Notes
To reproduce the analyses (or get a general better understanding of the repository structure), read the following instructions: 
1. R-project: All analyses were conducted using R. To reproduce analyses, open the "SDERSvalid_Analysis" file on the top level to initiate an R environment with the correct configurations.
2. Package tracking: All packages with their versions using the renv package. Install the renv package and use the renv::restore() command to install the correct package version
3. Relative paths: The project uses paths relative to the location of the R-project file using the "here" package, which should ensure cross-platform compatibility
4. use scripts: Analyses scripts can be found in the "scripts" subfolder. These should optimally be opened within the previously opened R-project 
5. local functions: The project uses some custom functions, which are defined in and loaded from the folder "functions"
6. data: The scripts depend on data, which can be found in the "data" folder. This data is minimally preprocessed (e.g., invalid cases due to technical difficulties are removed and scale scores are calculated). For details on this minimal preprocessing see the respective scripts in the "scripts" folder
7. the manuscripts folder contains separate folders for the resulting publications. they are relevant for reproducibity, as many tables and figures are automatically moved to these subdirectories from the main scripts

## Notes on specific papers

### A German version of the State Difficulties in Emotion Regulation Scale (S-DERS): Translation, Validation and Extended Factor Models
This paper focuses on the cross-sectional properties of the S-DERS following a mood induction. Relevant analysis scripts are scripts 1-3. Relevant data is "SDERSvalid_crossSec_data_preprocessed.csv". The original factor loadings from Lavender et al. (2017) can be found in "LavenderEFAloadings.csv". The respective manuscripts folder is called "SDERSvalid_crossSec"

