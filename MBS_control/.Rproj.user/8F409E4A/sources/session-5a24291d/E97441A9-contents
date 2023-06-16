###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        04/06/2023
#     This script explore data
#
#  Experiment   MBScontrol
#
#  Update:      04/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))

table(dataset_gw1$Pt.code, dataset_gw1$Video.emotion)

# no incomplete pt.code data.

# Demographic Table ---------------------------------------------------------------
# dataset for flextable object
demogaphic<- dataset_gw1%>%
  filter(Pt.code != "10_moebius")%>%
  group_by(Pt.code)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  dplyr::select(Pt.group,Pt.id, Pt.gender, Pt.age, Pt.study,Plsy.locus,Surg.type,Surg.date,Asmt.sunnybrook,
                Asmt.fdi.fisica,Asmt.fdi.sociale,Asmt.tas.20,Asmt.aq,Asmt.omft,Asmt.lpost.tot)%>%
  'colnames<-'(c("Group","Subject","Gender","Age" ,"Education","Paralisi","Type","Date","Sunnybrook","FDI fisica",
                 "FDI sociale", "TAS 20", "AQ", "OFMT", "L POST"))

# flextable object
demogaphic_tbl_full <- demogaphic%>% 
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 9)%>% 
  merge_v(j = 1)%>%
  align( part = "header", align = "left") %>% 
  align( part = "body", align = "center") %>%
  add_header_row(colwidths = c(6, 2, 7),
                 values = c("Participant", "Surgery","Assesment") )

# Participants Table summary ---------------------------------------------------------------
# dataset for flextable object
demogaphic_summary <- demogaphic%>%
  dplyr::select(Group,Subject,Gender,Age,Education)%>%
  group_by(Group,Gender) %>% 
  summarise(n = n(),
            Age_mean = mean(Age),
            Age_Sd = sd(Age),
            Education_mean = mean(Education),
            Education_Sd = sd(Education))

# add total
demogaphic_tbl_summary <- rbind(demogaphic_summary[1:2,], 
                data.frame(Group = "control", Gender ='Sub-total',t(colSums(demogaphic_summary[1:2,3])), t(colMeans(demogaphic_summary[1:2, -c(1,2,3)]))),
                demogaphic_summary[3:4,], 
                data.frame(Group = "moebius", Gender ='Sub-total',t(colSums(demogaphic_summary[3:4,3])), t(colMeans(demogaphic_summary[3:4, -c(1,2,3)]))),
                data.frame(Group = NA, Gender ='Total',t(colSums(demogaphic_summary[,3])), t(colMeans(demogaphic_summary[, -c(1,2,3)]))))%>% 
  'colnames<-'(c("Group","Gender","n","Mean", "Sd","Mean ","Sd " ))%>%
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 9)%>% 
  merge_v(j = 1)%>%
  align( part = "header", align = "left") %>% 
  align( part = "body", align = "center")%>%
  colformat_double(j = 4:7, digits = 2) %>% 
  align(part = "header", align = "center") %>% 
  align( part = "body", align = "center")%>%
  add_header_row(colwidths = c(3, 2, 2),
                 values = c("", "Age","Education") )%>%
  bold(i = c(3,6,7), bold = TRUE)


# Participants Responses Table---------------------------------------------------------------
# dataset neutral for flextable object
dat_neutral <- dataset_full%>%
  filter(Wheel.task == "task", Pt.code != "10_moebius" ,Video.emotion == "neutrality")%>%
  dplyr::select(Wheel.name,Pt.code,Pt.group,Video.emotion,Video.set,Wheel.angle,Resp.angle,Resp.diff,Resp.intensity)%>%
  group_by(Wheel.name,Video.emotion,Wheel.angle,Pt.group,Video.set)%>%
  summarise(Resp.angle.mean = NA,
            Wheel.angle.mean = NA,
            Resp.angle.sd = NA,
            Resp.intensity.mean = mean(Resp.intensity, na.rm=TRUE),
            Resp.intensity.sd = sd(Resp.intensity, na.rm=TRUE),
            Resp.diff.mean = NA,
            Resp.diff.sd = NA)%>%
  ungroup()%>%
  dplyr::select(Wheel.name,Video.emotion,Wheel.angle.mean,
                Pt.group,Video.set, 
                Resp.angle.mean,Resp.angle.sd,
                Resp.diff.mean,Resp.diff.sd,
                Resp.intensity.mean,Resp.intensity.sd)%>%
  mutate(Resp.intensity.mean = round(Resp.intensity.mean,2),
         Resp.intensity.sd = round(Resp.intensity.sd,2))%>%
  'colnames<-'(c("GEW","Emotion","Angle reference",
                 "Group" ,"Set",
                 "Response angle", "Response variance", 
                 "Bias","Uncertainty",
                 "Mean (px)", "Sd"))

# dataset nemotions for flextable object
dat_gw1 <- dataset_full%>%
  filter(Wheel.task == "task",Wheel.name == "GW1", Pt.code != "10_moebius" ,Video.emotion != "neutrality")%>%
  drop_na(Resp.angle)%>%
  dplyr::select(Wheel.name,Pt.code,Pt.group,Video.emotion,Video.set,Wheel.angle,Resp.angle,Resp.diff,Resp.intensity)%>%
  group_by(Wheel.name,Video.emotion,Wheel.angle,Pt.group,Video.set)%>%
  summarise(Resp.angle.mean = rad_to_deg(CircStats::circ.mean(Resp.angle)) %% 360,
            Wheel.angle = rad_to_deg(CircStats::circ.mean(Wheel.angle)) %% 360,
            Resp.angle.sd = 1 - CircStats::circ.disp(Resp.angle)$var,
            Resp.intensity.mean = mean(Resp.intensity, na.rm=TRUE),
            Resp.intensity.sd = sd(Resp.intensity, na.rm=TRUE),
            Resp.diff.mean = rad_to_deg(CircStats::circ.mean(Resp.diff)) %% 360,
            Resp.diff.sd = 1 - CircStats::circ.disp(Resp.diff)$var)%>%
  ungroup()%>%
  dplyr::select(Wheel.name,Video.emotion,Wheel.angle,
                Pt.group,Video.set, 
                Resp.angle.mean,Resp.angle.sd,
                Resp.diff.mean,Resp.diff.sd,
                Resp.intensity.mean,Resp.intensity.sd)%>%
  mutate(Wheel.angle = paste0(as.character(round(Wheel.angle,2)),"°"),
         Resp.angle.mean = paste0(as.character(round(Resp.angle.mean,2)),"°"),
         Resp.angle.sd = paste0(as.character(round(Resp.angle.sd,2)),"°"),
         Resp.diff.mean = paste0(as.character(round(Resp.diff.mean,2)),"°"),
         Resp.diff.sd = paste0(as.character(round(Resp.diff.sd,2)),"°"),
         Resp.intensity.mean = round(Resp.intensity.mean,2),
         Resp.intensity.sd = round(Resp.intensity.sd,2))%>%
  'colnames<-'(c("GEW","Emotion","Angle reference",
                 "Group" ,"Set",
                 "Response angle", "Response variance", 
                 "Bias","Uncertainty",
                 "Mean (px)", "Sd"))

# dataset nemotions for flextable object
dat_gw2 <- dataset_full%>%
  filter(Wheel.task == "task",Wheel.name == "GW2", Pt.code != "10_moebius" ,Video.emotion != "neutrality")%>%
  drop_na(Resp.angle)%>%
  dplyr::select(Wheel.name,Pt.code,Pt.group,Video.emotion,Video.set,Wheel.angle,Resp.angle,Resp.diff,Resp.intensity)%>%
  group_by(Wheel.name,Video.emotion,Wheel.angle,Pt.group,Video.set)%>%
  summarise(Resp.angle.mean = rad_to_deg(CircStats::circ.mean(Resp.angle)) %% 360,
            Wheel.angle.mean = "0°",
            Resp.angle.sd = 1 - CircStats::circ.disp(Resp.angle)$var,
            Resp.intensity.mean = mean(Resp.intensity, na.rm=TRUE),
            Resp.intensity.sd = sd(Resp.intensity, na.rm=TRUE),
            Resp.diff.mean = NA,
            Resp.diff.sd = NA)%>%
  ungroup()%>%
  dplyr::select(Wheel.name,Video.emotion,Wheel.angle.mean,
                Pt.group,Video.set, 
                Resp.angle.mean,Resp.angle.sd,
                Resp.diff.mean,Resp.diff.sd,
                Resp.intensity.mean,Resp.intensity.sd)%>%
  mutate(Resp.angle.mean = paste0(as.character(round(Resp.angle.mean,2)),"°"),
         Resp.angle.sd = paste0(as.character(round(Resp.angle.sd,2)),"°"),
         Resp.intensity.mean = round(Resp.intensity.mean,2),
         Resp.intensity.sd = round(Resp.intensity.sd,2))%>%
  'colnames<-'(c("GEW","Emotion","Angle reference",
                 "Group" ,"Set",
                 "Response angle", "Response variance", 
                 "Bias","Uncertainty",
                 "Mean (px)", "Sd"))

Responses_table <- rbind(dat_neutral,dat_gw1,dat_gw2)%>%
  arrange(GEW,Emotion,Group,Set)%>%
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 9)%>% 
  merge_v(j = c(1:5))%>%
  add_header_row(colwidths = c(5, 4,2),
                 values = c("Genova Emotion Wheel","Responses", "Perceived Intensity") )%>%
  align(align = "center", part = "all")


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
  ggplot(aes(x = Wheel.x, y = Wheel.y, shape = Pt.group, color = Pt.group)) +
  ggpubr::background_image(bg) +
  ggh4x::facet_nested(Video.set ~ Video.emotion, switch="y") +
  geom_point(alpha = 0.5, show.legend = TRUE, size = 3) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300))+
  guides(shape = guide_legend(title = "Pt.group")) +
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

plot_gew_emotions <- dataset_full %>% 
  filter(Video.emotion != "neutrality", Wheel.task == "task", Pt.code != "10_moebius") %>% 
  ggplot(aes(x = Wheel.x, y = Wheel.y, shape = Pt.group, color = Pt.group)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, size = 1) +
  ggh4x::facet_nested( Video.set + Wheel.name  ~ Video.emotion) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  scale_fill_manual(values = c("NA", "white"))



#################################################
# 
# END
#
############################### data_exploration