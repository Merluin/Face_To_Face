###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the uncertainty and bais measures of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the uncertaunty and bais.
#
#  Experiment   MBScontrol
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))

# GEW is composed by 16 emotions' category. Each emotion/category fill 22,5° of the GEW (360°/16 = 22,5°)
# With respect to its angle's reference,  each emotion category extends from 11,25° to - 11,25°. Correct responses are categorised if the Resp.angle fall in the correct label interval: 
# Example: Video.emotion == fear (video seen), Wheel.angle == 236.25° (angle reference), Resp.angle = 240°, the Pt response fall in the 22,5° interval.


# "uncertainty and bais" will analyse what alternative/error is made (that is to say if some emotion increase the uncertainty) and if so, if such error is systematic (bais)
# We will:
# 1) analyse the circular variance of errors (centred over mode and over chance), 
# 2) second we wil simulate the circular variance for an optimised 22,5° response
# 3) compare error variance to optimised variance
# 4) 









#################################################
# 
# END
#
############################# uncertainty bais