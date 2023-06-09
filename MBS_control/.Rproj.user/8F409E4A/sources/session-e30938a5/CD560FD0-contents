###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/06/2023
#  Description: General script
#  Experiment   MBScontrol
#
#  Update:      03/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions ---------------------------------------------------------------
devtools::load_all()

# set folders structure for analysis --------------------------------------
create_dir_structure()
# usethis::use_description()


# Pre-processing-----------------------------------------------------------

run_script("scripts/01_dataset_builder.R") # description in /docs/Dataset_description.html

# statistics

run_script("scripts/02_accuracy.R") # for accuracy report see /docs
run_script("scripts/02_perceived_intensity.R") # for perceived_intensitys ee /docs



#################################################
# 
# END
#
#################################### main_script