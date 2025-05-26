###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/06/2023
#  Description: General script
#  Experiment   CPO_palsy_AMIM1
#
#  Update:      03/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions ---------------------------------------------------------------
devtools::load_all()

# set folders structure for analisis --------------------------------------
create_dir_structure()
# usethis::use_description()


# Pre-processing-----------------------------------------------------------
# you will find dataset description in /docs/Dataset_description.html
run_script("scripts/01_dataset_builder.R")

e2wtlj1f
8ij7agah
#################################################
# 
# END
#
#################################### main_script