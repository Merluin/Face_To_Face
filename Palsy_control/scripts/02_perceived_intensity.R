###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Perceived intensity measure of the Palsycontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Perceived intensity.
#
#  Experiment   palsycontrol
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))

# Calculate mean intensity
intensity_mean <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity","correct"))

# Calculate intensity mean for neutral dataset
dat_neutral <- dataset_neutral %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity"))

# Generate a table summarizing the intensities
table_intensity <- intensity_mean%>%
  group_by(group,emotion, video_set) %>% 
  summarise(intensity = mean(intensity, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = intensity) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")


# Generate aplot for emotions intensities
plot_emotion<- flexplot(intensity~emotion+group |  video_set,
                             spread = "stdev", # sterr, stdev, 
                             data = intensity_mean,
                             alpha = 0.07) +
  theme(legend.position = "bottom") +
  ylab("Perceived intensity (mean & Sd in px)") +
  xlab("")

# Fit  correct for each emotion--------------------------------------------
  
# Fit linear mixed-effects model
fit <- lmer(intensity ~ emotion * group * video_set + (1|subject),
            data = intensity_mean%>%filter(correct == 1))
  
  # Generate table summary
  table <- tab_model(fit) #, show.df = FALSE, string.p = "p adjusted", p.adjust = "bonferroni")
  visualize(fit, plot = "residuals")
  # Create ANOVA table
  # Perform ANOVA
  chiquadro <- car::Anova(fit, test.statistic= "Chisq", type = "3") # test.statistic=c("Chisq", "F")
  
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  #Contrasts
  emotion <- testInteractions(fit, pairwise = "emotion")
  video<- testInteractions(fit, pairwise = "video_set")
  emotiongroup<- testInteractions(fit, pairwise = "group", fixed = "emotion")
  videogroup<- testInteractions(fit, pairwise = "group", fixed = "video_set")
  
    # data table of contrast
  temp<-rbind(emotion,
              video,
              emotiongroup,
              videogroup)
  
  # kable table object
  contrast<-temp%>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Contrasts (FDR corrected)") %>%
    column_spec(5, color = ifelse(temp$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  # Generate plot
  themeperso <- theme_paper(font_size = 10) +
    theme(legend.position = "bottom")
  
  plotemotion <- ggpredict(fit, terms = c( "emotion"))%>%
    plot()+ 
    labs(title = "Main effect emotion",
         x = "Emotions",
         y = "Intensity (px)") +
    themeperso
  
  plotvideo <- ggpredict(fit, terms = c( "video_set"))%>%
    plot()+ 
    labs(title = "Main effect Video set",
         x = "Emotions",
         y = "Intensity (px)") +
    themeperso
  
  plotgroupemotion <- ggpredict(fit, terms = c( "emotion", "group"))%>%
    plot()+ 
    labs(title = "Interaction Video set * emotion",
         x = "Emotions",
         y = "Intensity (px)") +
    themeperso
  
  plotgroupvideo <- ggpredict(fit, terms = c(  "video_set", "group"))%>%
    plot()+ 
    labs(title = "Interaction Video set * emotion",
         x = "Emotions",
         y = "Intensity (px)") +
    themeperso
  
  ploteffect<-ploteffect<-cowplot::plot_grid(plotemotion,
                                 plotvideo,plotgroupemotion,
                                 plotgroupvideo,
                                 ncol = 1,
                                 scale = c(.9, .9, .9, .9))
  

# Save the results
save(fit, table, chi_table, contrast, ploteffect,table_intensity, plot_emotion,  file = file.path("models/perceived_intensity.RData"))

#################################################
# 
# END
#
############################# perceived intensity