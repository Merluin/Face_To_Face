###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs data analysis for the CARIPARO experiment.
#
#  Experiment   MBS
#
#  Update:      11/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
#replace_csv("group", "control")

devtools::load_all()

# Data --------------------------------------------------------------------

data <- read.xlsx("objects/Studio2_Complete_Dataset.xlsx")


