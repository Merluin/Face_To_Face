###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Accuracy.
#
#  Experiment   MBScontrol
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))

# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject, group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks


# Plot accuracy GWE 1 vs GWE 2 ----------------------------------------------
dat_summ <- dataset_full %>%
  filter(Pt.code != "10_moebius", Video.emotion != "neutrality" ) %>% # no match
  drop_na(Video.emotion)%>%
  mutate(count = 1)%>%
  group_by(Pt.code,Video.emotion,Video.set,Wheel.name, Resp.category,Pt.group) %>%
  summarise(n = sum(count))%>%
  group_by(Video.emotion,Video.set,Wheel.name, Resp.category,Pt.group) %>%
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
table_accuracy <- accuracy %>% 
  group_by(group,emotion, video_set) %>% 
  summarise(acc = mean(acc, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = acc) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")



# Fit  correct for each emotion--------------------------------------------

  # Adatta il modello di regressione logistica
  x<-correct_data%>%
    mutate(correct = as.factor(correct),
           video_set = as.factor(video_set))%>%
    na.omit()
  
  fit <- glmer(correct ~ emotion * group * video_set + (1|subject) , data = x, family = binomial)

  
  # Generate table summary
  table <- tab_model(fit) #, show.df = FALSE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Create ANOVA table
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3) 
  chiquadro <- anova(fit) # car::Anova
  summary(chiquadro)$coefficients
  
  chi_table <- chiquadro %>%
    kbl(caption = "Anova(correct ~ emotion * group * video_set, type = 3) Accuracy n=20") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  interaction<- testInteractions(fit, pairwise = "group", fixed = c("video_set", "emotion"), adjustment = "fdr")
  interaction2<- testInteractions(fit, pairwise = "group", fixed = c("video_set", "emotion"))
  
  
  #Contrasts
  m1<-emmeans(fit, pairwise ~ group|video_set|emotion)
  m2<-emmeans(fit, pairwise ~ video_set|group)
  
  contrasts <- rbind(m1$contrasts,m2$contrasts)%>%
    data.frame()%>%
    mutate(p.corrected = p.adjust(p.value,"fdr"))
  
  # kable table object
  contrast<-contrasts%>%
    mutate_if(is.numeric,~round(., digits = 4)) %>%
    kbl(caption = "Contrasts (FDR corrected)") %>%
    column_spec(9, color = ifelse(contrasts$p.corrected <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")

  
  # Generate model plot
  
  #explorative plots
  emotion <- flexplot(correct~emotion | video_set*group, data= x)
  video <- flexplot(correct~video_set, data= x)
  plot <- cowplot::plot_grid(video,emotion,  nrow = 2)
  
  # plot
  themeperso <- theme_paper(font_size = 10) +
    theme(legend.position = "bottom")
  
  plotemotion <- ggpredict(fit, terms = c( "emotion"))%>%
    plot()+ 
    labs(title = "Main effect emotion",
         x = "Emotions",
         y = "Accuracy") +
    themeperso
  
  plotvideo <- ggpredict(fit, terms = c( "video_set"))%>%
    plot()+ 
    labs(title = "Main effect Video set",
         x = "Emotions",
         y = "Accuracy") +
    themeperso
  
  plotemogroup <- ggpredict(fit, terms = c("emotion", "group")) %>%
    plot() + 
    labs(
      title = "Interaction  group * emotion",
      x = "Emotions",
      y = "Accuracy") +
    themeperso
  
  plotemovideo <- ggpredict(fit, terms = c( "group", "video_set"))%>%
    plot()+ 
    labs(title = "Interaction video set * group",
         x = "Emotions",
         y = "Accuracy") +
    themeperso
  
  plotint3 <- ggpredict(fit, terms = c( "emotion", "group","video_set"))%>%
    plot()+ 
    labs(title = "Effetti interaction group * emotion * video_set",
         x = "Emotions",
         y = "Accuracy") +
    themeperso
  
ploteffect<-cowplot::plot_grid(plotemotion,
          plotvideo,
          plotemogroup,
          plotemovideo,
          plotint3,
          ncol = 1,
          scale = c(.9, .9, .9, .9,.9))
  
  
  

# Save the results
  save(fit, table, chi_table, contrast, ploteffect,table_accuracy, plot_freq_gw1, plot_freq_gw2,  file = file.path("models","accuracy.RData"))
  
#################################################
# 
# END
#
######################################## accuracy