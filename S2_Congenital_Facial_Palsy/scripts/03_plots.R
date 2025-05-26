###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs plots from data analysis for the CARIPARO experiment.
#
#  Experiment   MBScontrol
#
#  Update:      11/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
#replace_csv("group", "control")

devtools::load_all()

# Data --------------------------------------------------------------------
df_acc <- read_csv("data/study2_accuracy_behavioral_data.csv") %>%
  group_by(subject,video_set,group) %>%
  summarise( accuracy = mean(accuracy, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set,group))

df_int <- read_csv("data/study2_intensity_behavioral_data.csv")%>%
  group_by(subject,video_set, group) %>%
  summarise(intensity = mean(intensity, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set,group)) %>%
  ungroup() %>%
  select(-c(subject,video_set,group))

df <- left_join(df_acc, df_int, by = "label") %>%
  select(-label)



df_acc <- accuracy%>%
  group_by( video_set,group,subject) %>%
  summarise(acc = mean(acc,na.rm=TRUE)*100)




#Plot
# Ricrea la variabile di match
df <- df %>%
  ungroup() %>%
  mutate(match = paste0(parse_number(as.character(subject)), video_set))


assessments <- dataset_gw1 %>%
  select(Pt.code, Asmt.sunnybrook, Asmt.omft) %>%
  distinct() %>%
  rename(subject = Pt.code)

df <- df %>%
  left_join(assessments, by = "subject")


# Calcola medie, sd, se
descritive <- df %>%
  filter(group %in% c("Moebius", "Control")) %>%
  group_by(video_set, group) %>%
  summarise(mean_acc = mean(acc),
            sd_acc = sd(acc),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "Moebius" & video_set == "ADFES" ~ 1 + 0.1,
    group == "Moebius" & video_set == "JeFEE" ~ 1 + 0.1,
    group == "Control" & video_set == "ADFES" ~ 2 - 0.1,
    group == "Control" & video_set == "JeFEE" ~ 2 - 0.1
  ))

# Colori per le condizioni
colors <- c("ADFES" = "#657d9a", "JeFEE" = "#b37269")
colorgroup <- c(Moebius = "#F0E442", Control = "#C07DA5")
# Plot finale
p1 <- ggplot(df %>% filter(group %in% c("Moebius", "Control")),
             aes(x = group, y = acc, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match", cov = "group") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc, fill = video_set),
              size = 3, shape = 23, color = "black") +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colorgroup) +
  labs(x = NULL, y = "Mean Accuracy (%)", fill = "Expression Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") + theme(text=element_text(size=10,  family="Helvetica"),
                                          panel.background = element_blank(),
                                          axis.line = element_line(colour = "black"),
                                          strip.text.x = element_text(size = 6.6),
                                          strip.text.y = element_text(size = 6.6),
                                          panel.grid.major = element_blank(),  # Rimuove le griglie principali
                                          panel.grid.minor = element_blank() )




# Calculate mean intensity
intensity_mean <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity","correct")) %>%
  mutate(group = ifelse(group == "moebius", "Moebius","Control"))
# Calculate intensity mean for neutral dataset
dat_neutral <- dataset_neutral %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity")) %>%
  mutate(correct = 1) %>%
  mutate(group = ifelse(group == "moebius", "Moebius","Control"))
data <- rbind(intensity_mean, dat_neutral) %>%
  group_by(subject,video_set ,group , emotion) %>%
  summarise(intensity = mean(intensity,na.rm=TRUE))%>%
  ungroup() 

write_csv(data, "behavioral_data_study2_intensity.csv")

df_int <- rbind(intensity_mean, dat_neutral) %>%
  group_by(subject,video_set ,group ) %>%
  summarise(intensity = mean(intensity,na.rm=TRUE))


#Plot
# Ricrea la variabile di match
df <- df %>%
  ungroup() %>%
  mutate(match = paste0(parse_number(as.character(subject)), video_set))

# Calcola medie, sd, se
descritive <- df %>%
  filter(group %in% c("Moebius", "Control")) %>%
  group_by(video_set, group) %>%
  summarise(mean_acc = mean(intensity),
            sd_acc = sd(intensity),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "Moebius" & video_set == "ADFES" ~ 1 + 0.1,
    group == "Moebius" & video_set == "JeFEE" ~ 1 + 0.1,
    group == "Control" & video_set == "ADFES" ~ 2 - 0.1,
    group == "Control" & video_set == "JeFEE" ~ 2 - 0.1
  ))


# Plot finale
p2 <- ggplot(df %>% filter(group %in% c("Moebius", "Control")),
             aes(x = group, y = intensity, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match", cov = "group") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc, fill = video_set),
             size = 3, shape = 23, color = "black") +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colorgroup) +
  labs(x = NULL, y = "Mean intensity (px)", fill = "Expression Type") +
  theme_minimal() +
  theme(legend.position = "none")+ theme(text=element_text(size=10,  family="Helvetica"),
                                           panel.background = element_blank(),
                                           axis.line = element_line(colour = "black"),
                                           strip.text.x = element_text(size = 6.6),
                                           strip.text.y = element_text(size = 6.6),
                                         panel.grid.major = element_blank(),  # Rimuove le griglie principali
                                         panel.grid.minor = element_blank() )



p3 <- cowplot::plot_grid(p1, p2, nrow = 1,labels = "AUTO")


ggsave("files/Fig_2_finale.png",           
       plot = p3,           
       device = "png",         
       dpi = 300,              
       width = 18,              
       height = 9,             
       units = "cm")



