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
  filter( Video.emotion != "neutrality") %>% # no match
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
  summarize(Resp.distribution = mean(Resp.distribution))


GEW <- c("GW1", "GW2")
for(i in 1:length(GEW)){
plot_gew_discrete <- dat_summ %>% 
  filter(Wheel.name == GEW[i] )%>%
  mutate(Video.set = stringr::str_to_title(Video.set)) %>% 
  ggplot(aes(x = Resp.category, y = Resp.distribution, fill = Pt.group)) +
  geom_col(position = position_dodge()) +
  facet_grid(Video.emotion~Video.set) +
  cowplot::theme_minimal_hgrid() +
  theme_paper(font_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
                                   face = ifelse(levels(dataset_full$Resp.category) %in% unique(dat_summ$Video.emotion),
                                                 "bold", "plain"),
                                   size = ifelse(levels(dataset_full$Resp.category) %in% unique(dat_summ$Video.emotion),
                                                 10, 8)),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank()) +
  labs(fill = "group")

#save
ggsave_plot(plot_gew_discrete,
            name = file.path("figures", "png", paste0("plot_gew",i,"_discrete")),
            device = "png", width = 15, height = 10)

saveRDS(plot_gew_discrete, file = file.path("objects",  paste0("plot_frequencies_gew",i,".rds")))

}

             
                 
# Accuracy GEW ------------------------------------------------------------

tab_acc_gew <- accuracy %>% 
  group_by(group,emotion, video_set) %>% 
  summarise(acc = mean(acc, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = acc) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")

saveRDS(tab_acc_gew, file = file.path("objects","table_accuracy.rds"))

# Fit  correct for each emotion--------------------------------------------


  # Adatta il modello di regressione logistica
  dataset_glm<-correct_data%>%
    mutate(correct = as.factor(correct))%>%
    na.omit()
  
  fit <- glm(correct ~ emotion + group * video_set , data = dataset_glm, family = binomial)
  
  # Generate table summary
  table <- tab_model(fit, show.df = FALSE) #, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3)
  # Create ANOVA table
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  #Contrasts
  emotion<- testInteractions(fit, pairwise = "emotion", adjustment = "fdr")
  group<- testInteractions(fit, pairwise = "group", adjustment = "fdr")
  video<- testInteractions(fit, pairwise = "video_set", adjustment = "fdr")
  groupvideoset<- testInteractions(fit, pairwise = "group", fixed = "video_set", adjustment = "fdr")
  
  
  temp<-rbind(emotion[1:15,],
              group[1,],
              video[1,],
              groupvideoset[1:2,])
  
  contrast<-temp%>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Contrasts (FDR corrected)") %>%
    column_spec(5, color = ifelse(temp$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  

  # Generate model plot
  emotion <- flexplot(correct~emotion, data= dataset_glm)
  group <- flexplot(correct~group, data= dataset_glm)
  video <- flexplot(correct~video_set, data= dataset_glm)
  
  cowplot::plot_grid( group,video,emotion)

#################################################
# 
# END
#
######################################## accuracy