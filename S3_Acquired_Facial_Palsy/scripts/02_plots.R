###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs plots from data analysis for the CARIPARO experiment.
#
#  Experiment   Onlinecontrol
#
#  Update:      11/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
#replace_csv("group", "control")

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))


# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code,Pt.id, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject", "id","match","video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject,id,match,group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks

df <- accuracy%>%
  group_by( video_set,group,subject,match,id) %>%
  summarise(acc = mean(acc,na.rm=TRUE))

#Plot
# Ricrea la variabile di match
df <- df %>%
  ungroup() %>%
  mutate(match = paste0(parse_number(as.character(match)),"_" ,video_set))


assessments <- dataset_gw1 %>%
  select(Pt.code, Asmt.sunnybrook) %>%
  distinct() %>%
  rename(subject = Pt.code)

df <- df %>%
  left_join(assessments, by = "subject")


# Calcola medie, sd, se
descritive <- df %>%
  filter(group %in% c("palsy", "control")) %>%
  group_by(video_set, group) %>%
  summarise(mean_acc = mean(acc),
            sd_acc = sd(acc),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "palsy" & video_set == "ADFES" ~ 1 + 0.1,
    group == "palsy" & video_set == "JeFEE" ~ 1 + 0.1,
    group == "control" & video_set == "ADFES" ~ 2 - 0.1,
    group == "control" & video_set == "JeFEE" ~ 2 - 0.1
  ))

# Colori per le condizioni
colors <- c("ADFES" = "blue", "JeFEE" = "red")

# Plot finale
p1 <- ggplot(df %>% filter(group %in% c("palsy", "control")),
             aes(x = group, y = acc, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc),
             color = "black",
             size = 2.5, shape = 18, inherit.aes = FALSE) +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Mean Accuracy", fill = "Expression Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")




# Calculate mean intensity
intensity_mean <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set,Pt.match,  Video.emotion, Pt.group, Resp.intensity, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set","match",  "emotion", "group", "intensity","correct"))


df <- intensity_mean%>%
  group_by(subject,video_set ,group , emotion, match) %>%
  summarise(intensity = mean(intensity,na.rm=TRUE))%>%
  group_by(subject,video_set ,group ,match) %>%
  summarise(intensity = mean(intensity,na.rm=TRUE))


#Plot
# Ricrea la variabile di match
df <- df %>%
  ungroup() %>%
  mutate(match = paste0(parse_number(as.character(match)), video_set))

# Calcola medie, sd, se
descritive <- df %>%
  filter(group %in% c("palsy", "control")) %>%
  group_by(video_set, group) %>%
  summarise(mean_acc = mean(intensity),
            sd_acc = sd(intensity),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "palsy" & video_set == "ADFES" ~ 1 + 0.1,
    group == "palsy" & video_set == "JeFEE" ~ 1 + 0.1,
    group == "control" & video_set == "ADFES" ~ 2 - 0.1,
    group == "control" & video_set == "JeFEE" ~ 2 - 0.1
  ))


# Plot finale
p2 <- ggplot(df %>% filter(group %in% c("palsy", "control")),
             aes(x = group, y = intensity, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc),
             color = "black",
             size = 2.5, shape = 18, inherit.aes = FALSE) +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Mean intensity (px)", fill = "Expression Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")



p3 <- cowplot::plot_grid(p1, p2, ncol = 1,labels = "AUTO")


ggsave("files/Figure_8.jpg", 
       plot = p3,     
       device = "jpeg", 
       dpi = 600,
       width = 18, height = 28, units = "cm")
