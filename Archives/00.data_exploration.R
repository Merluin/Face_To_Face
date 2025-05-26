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
#  Update:      04/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","online_circular.RData"))

table(dataset_gw1$Pt.code, dataset_gw1$Video.emotion)


# Participants Table summary ---------------------------------------------------------------
demogaphic<- dataset_gw1%>%
  group_by(Pt.code)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  dplyr::select(Pt.code, Pt.gender, Pt.age, Pt.study)%>%
  'colnames<-'(c("Subject","Gender","Age" ,"Education"))


demogaphic_summary <- demogaphic%>%
  group_by(Gender) %>% 
  summarise(n = n(),
            Age_mean = mean(Age),
            Age_Sd = sd(Age),
            Education_mean = mean(Education),
            Education_Sd = sd(Education))

# add total
demogaphic_tbl_summary <- rbind(demogaphic_summary, 
                                data.frame(Gender ='Total',t(colSums(demogaphic_summary[,2])),t(colSums(demogaphic_summary[,3])), t(colMeans(demogaphic_summary[, -c(1,2,3)]))))%>% 
  'colnames<-'(c("Gender","n","Mean", "Sd","Mean ","Sd " ))%>%
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 9)%>% 
  align( part = "header", align = "left") %>% 
  align( part = "body", align = "center")%>%
  colformat_double(j = 3:6, digits = 2) %>% 
  align(part = "header", align = "center") %>% 
  align( part = "body", align = "center")%>%
  add_header_row(colwidths = c(2, 2, 2),
                 values = c("", "Age","Education") )%>%
  bold(i = c(3), bold = TRUE)



# neutral resp & GEW Plots ---------------------------------------------------------------

bg <- magick::image_read("files/gew_low_res.png")
bg <- magick::image_modulate(bg, brightness = 80)

gew_legend <- coords %>%   
  mutate(mask = "Legend",
         flip = ifelse(x_emo < 0, degree_emo + 180, degree_emo),
         emotion = stringr::str_to_title(emotion)) %>% 
  ggplot() +
  ggpubr::background_image(bg) +
  geom_text(aes(x = x_emo*0.75, y = y_emo*0.75, 
                label = emotion, 
                angle = flip),
            size = 5.5, fontface = "bold",
            check_overlap = TRUE) +
  facet_grid(. ~ mask) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300))


neutral_plot <- dataset_full %>% 
  filter(Video.emotion == "neutrality") %>% 
  ggplot(aes(x = Wheel.x, y = Wheel.y, shape = Video.set, color = Pt.gender)) +
  ggpubr::background_image(bg) +
  ggh4x::facet_nested(Video.set ~ Video.emotion, switch="y") +
  geom_point(alpha = 0.5, show.legend = TRUE, size = 3) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300))+
  guides(shape = guide_legend(title = "Video.set")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  facet_grid(Wheel.name ~ Video.set)+
  guides(color = guide_legend(title = "Group"),
         shape = guide_legend(title = "Group")) 

plot_gew_legend_neutral <- cowplot::plot_grid(neutral_plot, gew_legend, labels = "AUTO")


# Responses Plots Gw1 & Gw2 ---------------------------------------------------------------

plot_gew_emotions_gw1 <- dataset_full %>% 
  filter(Video.emotion != "neutrality", Wheel.task == "task", Wheel.name == "GW1") %>% 
  ggplot(aes(x = Wheel.x, y = Wheel.y, shape = Video.set, color = Video.set)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, size = 1) +
  ggh4x::facet_nested( Video.set ~ Video.emotion) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  scale_fill_manual(values = c("NA", "white"))

plot_gew_emotions_gw2 <- dataset_full %>% 
  filter(Video.emotion != "neutrality", Wheel.task == "task", Wheel.name == "GW2") %>% 
  ggplot(aes(x = Wheel.x, y = Wheel.y, shape = Video.set, color = Video.set)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, size = 1) +
  ggh4x::facet_nested(  Video.set ~ Video.emotion) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  scale_fill_manual(values = c("NA", "white"))

# matrix confusion
# dataset of resp count
dataset <-  dataset_gw1 %>%
  dplyr::select(Pt.code, Video.set, Video.emotion,  Resp.category) %>%
  'colnames<-'(c("subject", "video_set", "emotion",  "response")) 


# dataset of resp count per subject
dataset_subject <- dataset %>%
  group_by(subject,  video_set, emotion, response) %>%
  summarise( count = n()/8)

# dataset of resp count per group
dataset_group <- dataset_subject %>%
  group_by( video_set, emotion, response) %>%
  summarise( count = mean(count))

# matrix plot
matrix_confusion <- dataset_group %>% 
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
  facet_grid(video_set ~ .)

# Saving
save(demogaphic_tbl_summary,plot_gew_legend_neutral,plot_gew_emotions_gw1,plot_gew_emotions_gw2,matrix_confusion, file = file.path("objects", "online_info.RData"))


#################################################
# 
# END
#
############################### data_exploration


#################################################
# 
# END
#
############################### data_exploration