###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        04/06/2023
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the Online student experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Accuracy.
#
#  Experiment Online student
#
#  Update:      04/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","online_circular.RData"))

# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion,  Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion",  "correct"))

accuracy<-correct_data%>%
  group_by(subject,  emotion, video_set) %>%
  summarise(correct = sum(correct),
            n = n(),
            acc = correct/n) 

# %>% filter(subject != "e2wtlj1f" , subject != "8ij7agah") # subjects with incomplet data


# Plot accuracy GWE 1 vs GWE 2 ----------------------------------------------
dat_summ <- dataset_full %>%
  filter( Video.emotion != "neutrality" , Wheel.task == "task") %>% # no match
  drop_na(Video.emotion)%>%
  mutate(count = 1)%>%
  group_by(Pt.code,Video.emotion,Video.set,Wheel.name, Resp.category) %>%
  summarise(n = sum(count))%>%
  group_by(Video.emotion,Video.set,Wheel.name, Resp.category) %>%
  summarise(n = mean(n))


plot_freq_gw1 <- dat_summ %>% 
  filter(Wheel.name == "GW1" )%>%
  mutate(Video.set = stringr::str_to_title(Video.set))%>%
  plotaccuracy()

plot_freq_gw2 <- dat_summ %>% 
  filter(Wheel.name == "GW2" )%>%
  mutate(Video.set = stringr::str_to_title(Video.set))%>%
  plotaccuracy()

             
                 
# Accuracy GEW ------------------------------------------------------------

# Accuracy GEW ------------------------------------------------------------
table_accuracy <- accuracy %>% 
  group_by(emotion, video_set) %>% 
  summarise(acc = mean(acc, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = acc) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")

# Fit  correct for each emotion--------------------------------------------

dataset_anova <- accuracy %>%
  group_by(subject,video_set,emotion) %>%
  summarise( acc = mean(acc, na.rm = TRUE))

# Save dataset for Jamovi in CSV format
write.csv(dataset_anova, "objects/dataset_anova_accuracy.csv", row.names = FALSE)

# Perform ANOVA
full <- aov_ez("subject", "acc", dataset_anova,within = c("emotion", "video_set"))





#################################################
# 
# END
#
######################################## accuracy