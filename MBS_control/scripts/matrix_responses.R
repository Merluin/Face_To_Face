###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response.
#
#  Experiment   MBScontrol
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------

# dataset of resp count
dataset <-  dataset_gw1 %>%
  dplyr::select(Pt.code, Pt.match, Pt.group, Video.set, Video.emotion,  Resp.category) %>%
  'colnames<-'(c("subject" ,"match", "group", "video_set", "emotion",  "response")) 


# dataset of resp count per subject
dataset_subject <- dataset %>%
  group_by(subject, match, group, video_set, emotion, response) %>%
  summarise( count = n()/8)

# dataset of resp count per group
dataset_group <- dataset_subject %>%
  group_by(group, video_set, emotion, response) %>%
  summarise( count = mean(count))
  

# table to matrix

#################################################
# 
# END
#
############################### matrix responses






dataset_group %>% 
  spread(response, count, fill = 0) %>%
  gather(response, count,-c(1:4)) %>%
  mutate(count = count*100,
         response = factor(response, levels = c("pride", "elation", "happiness", "satisfaction", "relief", "hope", "interest", "surprise", 
                                                "sadness", "fear", "shame", "guilt", "envy", "disgust", "contempt", "anger", "no resp")),
         emotion = factor(emotion, levels = c("happiness", "surprise", "sadness", "fear", "disgust", "anger"))) %>%
  as.data.frame() %>% 
  ggplot(aes(response, emotion)) + 
  geom_tile(aes(fill = count)) + 
  geom_text(aes(label = round(count, 2)), 
            size = 3) + 
  coord_fixed() + 
  scale_fill_viridis() + 
  guides(fill = FALSE) +
  facet_grid(group ~ video_set)
