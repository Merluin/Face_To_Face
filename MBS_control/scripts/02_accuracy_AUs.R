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
palsymean <- read.csv(file.path("original_data/demography/compromissionmean.csv"), sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "aa") %>%
  group_by(Wheel.emotion,Pt.code) %>%
  summarise(Palsy.score = mean(Palsy.score))%>%
  'colnames<-'(c("emotion" ,"subject","palsy"))
  
# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"match","video_set", "emotion", "group", "correct")) 


accuracy<-correct_data%>%
  group_by(subject,match,group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks

correct_data <- left_join(correct_data,palsymean, by = c("subject","emotion"))

# plot
accuracy %>%
  filter(subject != "1_moebius",
         subject != "2_moebius",
         subject != "3_moebius",
         subject != "4_moebius",
         subject != "5_moebius",
         subject != "6_moebius",
         subject != "7_moebius",
         subject != "8_moebius",
         subject != "9_moebius",
         subject != "10_moebius"
  ) %>%
  group_by(group, emotion, video_set) %>%
  summarise(mean_se(acc)) %>%
  ggplot(aes(x = emotion, y = y, fill = group, ymax = ymax, ymin = ymin), group = 1) +
  geom_errorbar(position = "dodge") +
  geom_col(position = "dodge") +
  facet_grid( video_set ~ .)


# Fit  correct for each emotion--------------------------------------------

# ADFES fear inclusi MBS 1 2 5 9 model correct ~ group
#anger 1,2,3,4,6,9
#fear 1,2,5,9
#disgust 1,2,3,4,6,9

# Fit  
au_deficit <- c("happiness","surprise","sadness","fear","disgust","anger")
deficit <- au_deficit[1]

  # Adatta il modello di regressione logistica
  x<-correct_data%>%
    mutate(correct = as.factor(correct),
           video_set = as.factor(video_set))%>%
    na.omit()%>% filter( emotion == deficit)

  fit <- glmer(correct ~ palsy  + (1|subject) , data = x, family = binomial)
  fit1 <- glmer(correct ~  palsy * video_set + (1|subject) , data = x, family = binomial)
  anova(fit,fit1)
  summary(fit)
  plot(allEffects(fit1))


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
  video_set<-emmeans(fit_mixed, pairwise ~ video_set)
  tabella_video_set <- data.frame(as.data.frame(summary(video_set)$contrasts))
  
  group<-emmeans(fit_mixed, pairwise ~ group)
  tabella_group <- data.frame(as.data.frame(summary(group)$contrasts))

  
 group_video_set<-emmeans(fit_mixed, pairwise ~ group|video_set)
  tabella_group_video_set <- data.frame(as.data.frame(summary(group_video_set)$contrasts))
  
  
  
  contrasts <- rbind(tabella_group%>%mutate(video_set = NA,emotion = deficit, effect = "group"),
                     tabella_video_set%>%mutate(video_set = NA,emotion = deficit, effect = "video_set"),
                     tabella_group_video_set%>%mutate( emotion = deficit, effect = "video_set|group"))%>%
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
  flex_video <- flexplot(correct~video_set, data= x)
  flex_plot <- cowplot::plot_grid(video_set,  nrow = 2)
  
  # plot
  themeperso <- theme_paper(font_size = 10) +
    theme(legend.position = "bottom")
  
  plotgroup <- ggpredict(fit, terms = c( "group"))%>%
    plot()+ 
    labs(title = "Main effect group",
         x = "Emotions",
         y = "Accuracy") +
    themeperso
  
  plotvideo <- ggpredict(fit, terms = c( "video_set"))%>%
    plot()+ 
    labs(title = "Main effect Video set",
         x = "Emotions",
         y = "Accuracy") +
    themeperso
  
  plotgroupvideo <- ggpredict(fit, terms = c("group", "video_set")) %>%
    plot() + 
    labs(
      title = "Interaction  group * video_set",
      x = "Emotions",
      y = "Accuracy") +
    themeperso
  
  
  
ploteffect<-cowplot::plot_grid(plotgroup,
          plotvideo,
          plotgroupvideo,
          ncol = 1,
          scale = c(.9, .9, .9))
  
  
  

# Save the results
  save(fit, table, chi_table,LRT_table, contrast, ploteffect,table_accuracy, plot_freq_gw1, plot_freq_gw2,  file = file.path("models",paste0("accuracy_",deficit,".RData")))
  
#################################################
# 
# END
#
######################################## accuracy