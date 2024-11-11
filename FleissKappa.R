library(dplyr)
library(irr)
## Calculating Fleiss-kappa
## I. Fed_ind_Fleiss is a dataset with statements
# of the Prime Ministers during the Covid-19 pandemic.
# This data set is not considered in the present work.
load("Fed_ind_Fleiss.Rdata")
#
df <- Fed_ind_Fleiss %>% select(., c("index1", "index2", "index3"))
#
kappam.fleiss(df)
################# Kappa = 0.91 ('excellent') p-value = 0
#######################################################################
## II. Att_t_fed_Fleiss is a dataset with the statements found in 
#  coalition treatments of 15 German states. 
#  The Fleiss kappa is considered in Barbaro and Rode (Publius)
load("Att_t_fed_Fleiss.Rdata")
df2 <- Att_t_fed_Fleiss %>% select(., c("index1", "index2", "index3"))
kappam.fleiss(df2) ## kappa = 0.787, p-value = 0
