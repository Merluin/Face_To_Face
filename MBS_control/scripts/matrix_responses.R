###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for plotting responses tables.
#
#  Experiment   MBScontrol
#
#  Update:      23/05/2023
###########################################################################

# Clear the workspace
rm(list = ls())

# Load necessary libraries
library(devtools)
load_all()

# Load the dataset of response counts
load(file.path("objects", "mbs_circular.RData"))

# Prepare the dataset
dataset <- dataset_gw1 %>%
  dplyr::select(Pt.code, Video.name, Pt.group, Video.set, Video.emotion, Resp.category) %>%
  'colnames<-'(c("subject", "video.id", "group", "video_set", "emotion", "response"))

# Compute dataset of response count per subject
dataset_subject <- dataset %>%
  group_by(subject, video.id, group, video_set, emotion, response) %>%
  summarise(count = n() / 2) %>%
  spread(response, count, fill = 0) %>%
  gather(response, count, -c(1:5))

# Compute dataset of response count per group
dataset_group <- dataset_subject %>%
  group_by(group, video.id, video_set, emotion, response) %>%
  summarise(count = mean(count) * 100) %>%
  mutate(response = factor(response, levels = c("pride", "elation", "happiness", "satisfaction",
                                                "relief", "hope", "interest", "surprise",
                                                "sadness", "fear", "shame", "guilt",
                                                "envy", "disgust", "contempt", "anger")))

# Plot the response counts for each emotion and video set
emo <- c("anger", "happiness", "surprise", "sadness", "fear", "disgust")
gp <- "control"

for (i in 1:6) {
  ADFES <- dataset_group %>%
    filter(group == gp, emotion == emo[i], video_set == "ADFES") %>%
    ggplot(aes(response, video.id)) +
    geom_tile(aes(fill = count)) +
    geom_text(aes(label = round(count, 3)), size = 3) +
    guides(fill = FALSE) +
    coord_fixed() +
    scale_fill_viridis() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    facet_grid(emotion ~ video_set) +
    ggtitle(paste0(emo[i], " ", gp))
  
  JeFEE <- dataset_group %>%
    filter(group == gp, emotion == emo[i], video_set == "JeFEE") %>%
    ggplot(aes(response, video.id)) +
    geom_tile(aes(fill = count)) +
    geom_text(aes(label = round(count, 3)), size = 3) +
    guides(fill = FALSE) +
    coord_fixed() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    facet_grid(emotion ~ video_set) +
    scale_fill_viridis() +
    ggtitle(paste0(emo[i], " ", gp))
  
  plot <- cowplot::plot_grid(ADFES, JeFEE, labels = "AUTO", ncol = 1)
  
  ggsave(plot, file = file.path("files", paste0(emo[i], "_", gp, ".pdf")))
}



# Perform multinomial analysis
model <- multinom(response ~ emotion * video_set * group, data = dataset)
m1 <- brm(response ~ emotion * video_set * group + (1|), data = dataset)

# Calculate p-values of the coefficients
p_values <- car::Anova(model, type = "III")

# Calculate marginal means (estimated probabilities) for each factor combination
means <- emmeans::emmeans(model, specs = ~ emotion * video_set * group)

# Perform post hoc comparisons using Tukey's method
posthoc <- emmeans::pairs(means, adjust = "tukey")

# Print the post hoc comparisons
print(posthoc)

#################################################
# 
# END
#
############################### matrix responses
