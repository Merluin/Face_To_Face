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
#  Update:      18/02/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))

# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject, group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            n = n(),
            acc = correct/n) 

# %>% filter(subject != "e2wtlj1f" , subject != "8ij7agah") # subjects with incomplet data

# Plot accuracy GWE 1 vs GWE 2 ----------------------------------------------
dat_summ <- dataset_full %>%
  filter( Video.emotion != "neutrality" , Wheel.task == "task") %>% # no match
  drop_na(Video.emotion)%>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group,Wheel.name, Resp.category, Resp.correct) %>%
  mutate(Resp.correct = 1)%>%
  group_by(Pt.code,Video.emotion,Video.set,Pt.group,Wheel.name) %>%
  mutate(n = n())%>%
  group_by(Pt.code,Video.emotion,Video.set,Resp.category,Pt.group,Wheel.name) %>%
  mutate( Resp.correct = sum(Resp.correct),
          Resp.distribution = Resp.correct/n)%>%
  summarize(Resp.distribution = mean(Resp.distribution))%>%
  group_by(Video.emotion,Video.set,Resp.category,Pt.group,Wheel.name) %>%
  summarize(Resp.distribution = mean(Resp.distribution)*8)

plot_freq_gw1 <- dat_summ %>% 
  filter(Wheel.name == "GW1" )%>%
  mutate(Video.set = stringr::str_to_title(Video.set))%>%
  plotaccuracy()

plot_freq_gw2 <- dat_summ %>% 
  filter(Wheel.name == "GW2" )%>%
  mutate(Video.set = stringr::str_to_title(Video.set))%>%
  plotaccuracy()

                 
# Accuracy GEW ------------------------------------------------------------

table_accuracy <- accuracy %>% 
  group_by(group,emotion, video_set) %>% 
  summarise(accu = paste0(
    round(mean(acc, na.rm = TRUE),2),
    " ± ",
    round(sd(acc, na.rm = TRUE),2)))%>% 
  pivot_wider(names_from = emotion, values_from = accu) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")

# Fit  correct for each emotion--------------------------------------------

dataset_anova<-correct_data %>%
  group_by(subject,video_set,group,emotion) %>%
  summarise( acc = mean(correct, na.rm = TRUE))

# Save dataset for Jamovi in CSV format
write.csv(dataset_anova, "objects/dataset_anova_accuracy.csv", row.names = FALSE)

# Perform ANOVA
full <- aov_ez("subject", "acc", dataset_anova,within = c("emotion", "video_set"),  between = "group")

ADFES <- aov_ez("subject", "acc", dataset_anova%>%
                  filter( video_set == "ADFES"),within = "emotion",  between = "group")

JeFEE <- aov_ez("subject", "acc", dataset_anova%>%
                  filter( video_set == "JeFEE"),within = "emotion",  between = "group")








#################################################
# 
# END
#
######################################## accuracy