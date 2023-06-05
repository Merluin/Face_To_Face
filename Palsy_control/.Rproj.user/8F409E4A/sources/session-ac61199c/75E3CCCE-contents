###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        04/06/2023
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the CPO_palsy_AMIM1 experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Accuracy.
#
#  Experiment CPO_palsy_AMIM1
#
#  Update:      04/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))

table(v$Pt.code, dataset_gw1$Video.emotion)

# incomplet pt.code data:

# e2wtlj1f max 34 trials subject 19 match 3
# 8ij7agah max 56 trials subject 8 match 34

dataset_gw1%>%
  filter(Pt.code == "8ij7agah" | Pt.code == "e2wtlj1f")%>%
  group_by(Pt.code)%>%
  mutate( max.trial = max(Exp.trial))%>%
  filter(Exp.trial == 2)%>%
  dplyr::select(Pt.code, Pt.id, Pt.match, max.trial)
# check the numbers of responses





#################################################
# 
# END
#
############################### data_exploration