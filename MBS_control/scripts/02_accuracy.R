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
  filter( Video.emotion != "neutrality" , Wheel.task == "task") %>% # no match
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
  fit_mixed<- mixed(correct ~ emotion * group * video_set + (1|subject) ,method = "LRT", data = x, family = binomial, expand_re = TRUE)
  #fit_pb<- mixed(correct ~ emotion * group * video_set + (1|subject) ,method = "PB", data = x, family = binomial, expand_re = TRUE)
  
  # Generate table summary
  table <- tab_model(fit ) #, show.df = FALSE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Create ANOVA table
  # Perform ANOVA
  chiquadro <- car::Anova(fit, type = 3) 
  # chiquadro <- anova(fit) # car::Anova
  # summary(chiquadro)$coefficients
  
  chi_table <- chiquadro %>%
    kbl(caption = "correct ~ emotion * group * video_set, p-values via partial sums of squares, Accuracy n=20") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  LRT_table <- fit_mixed$anova_table %>%
    kbl(caption = "correct ~ emotion * group * video_set, p-values via the likelihood ratio tests, Accuracy n=20") %>%
    column_spec(5, color = ifelse(fit_mixed$anova_table$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  # interaction<- testInteractions(fit, pairwise = "group", fixed = c("video_set", "emotion"), adjustment = "fdr")
  # interaction2<- testInteractions(fit, pairwise = "group", fixed = c("video_set", "emotion"))
  
  
  #Contrasts
  emotion<-emmeans(fit_mixed, pairwise ~ emotion)
  tabella_emotion <- data.frame(as.data.frame(summary(emotion)$contrasts))
  
  video_set<-emmeans(fit_mixed, pairwise ~ video_set)
  tabella_video_set <- data.frame(as.data.frame(summary(video_set)$contrasts))
  
  emotion_group<-emmeans(fit_mixed, pairwise ~ group|emotion)
  tabella_emotion_group <- data.frame(as.data.frame(summary(emotion_group)$contrasts))
  
  emotion_video_set<-emmeans(fit_mixed, pairwise ~ video_set|emotion)
  tabella_emotion_video_set <- data.frame(as.data.frame(summary(emotion_video_set)$contrasts))
  
  emotion_group_video_set<-emmeans(fit_mixed, pairwise ~ group|emotion|video_set)
  tabella_emotion_group_video_set <- data.frame(as.data.frame(summary(emotion_group_video_set)$contrasts))
  
  
  
  contrasts <- rbind(tabella_emotion%>%mutate(video_set = "all",emotion = NA, effect = "emotion"),
                     tabella_video_set%>%mutate(video_set = NA,emotion = "all", effect = "video_set"),
                     tabella_emotion_group%>%mutate(video_set = "all", effect = "emotion|group"),
                     tabella_emotion_video_set%>%mutate(video_set = NA, effect = "emotion|video_set"),
                     tabella_emotion_group_video_set%>%mutate(effect = "group|emotion|video_set"))%>%
    select(effect,video_set, emotion, contrast, estimate, SE, df, z.ratio, p.value)
  
  # kable table object
  options(knitr.kable.NA = '')
  contrast<-contrasts%>%
    mutate_if(is.numeric,~round(., digits = 4)) %>%
    kbl(caption = "Contrasts (no corrected)") %>%
    column_spec(9, color = ifelse(contrasts$p.value <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")

  
  # Generate model plot
  
  #explorative plots
  flex_emotion <- flexplot(correct~emotion | video_set*group, data= x)
  flex_video <- flexplot(correct~video_set, data= x)
  flex_plot <- cowplot::plot_grid(video_set,emotion,  nrow = 2)
  
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
  
  plotemovideo <- ggpredict(fit, terms = c( "emotion", "video_set"))%>%
    plot()+ 
    labs(title = "Interaction video set * emotion",
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
  save(fit, table, chi_table,LRT_table, contrast, ploteffect,table_accuracy, plot_freq_gw1, plot_freq_gw2,  file = file.path("models","accuracy.RData"))
  
#################################################
# 
# END
#
######################################## accuracy