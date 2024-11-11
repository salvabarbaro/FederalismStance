# FederalismStance
Replication files to the paper entitled "What explains German Länder governments' stances towards federalism?" by Salvatore Barbaro and Julia M. Rode. 


1. Data
1.1. microdf.RData consists of all micro data associated with the assessment of states' stances. microdf.RData is a R-related file. 

1.2 LFAseit1950.csv consists of the fiscal equalisation scheme data from 1950 on. Source: Federal Ministry of Finanace, Federal Statistical Office.

2. RScripts
FactorAnalysis.R: All statistical evaluations related to states' stances. Several comments shall help to explain the steps.
BWpics.R: Generates all figures (in BW). 
Script02.R: Survey analysis for the state of BW. The script uses da data set available at gesis. The analysis relates to a section in the supplementary material.

3. Remarks
The scripts run perfectly on Linux machines. We have tested the scripts on Windows with no errors occuring.

A Data Acessibility Statement
The self conducted data as well as the Data from the Federal Statistical Office are provided. For access to gesis data refer gesis.org.

B DATASET LIST
See above (1.)

C SOFTWARE REQUIREMENTS
R 4.1.2 (Bird Hippie) or above. Tested on a Debian Linus OS.

D CONTROLLED RANDOMNESS
Mostly not applicable. Exemption: FactorAnalysis.R uses a bootstrap procedure with a given seed number as one robustness check.

E APPROXIMATE TIME NEEDED TO REPRODUCE THE ANALYSIS ON A STANDARD (2024) DESKTOP MACHINE
About 2 minutes (without the bootstrap part). Bootstrap: 3 min. You can speed-up the processing time when importing the gisco-data (bund_shp <- gisco_get_nuts()) with resolution = 20 instead of resolution = 03.

F DESCRIPTION OF CODE
The descriptions can be found as comment lines in the script.

INSTRUCTION FOR REPLICATORS
None. 
