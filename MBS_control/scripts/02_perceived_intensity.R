###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Perceived intensity measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Perceived intensity.
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
intensity_mean <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity","correct"))

unique(intensity_mean$subject)

# Calculate intensity mean for neutral dataset
dat_neutral <- dataset_neutral %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity")) %>%
  mutate(correct = 1)

data <- rbind(intensity_mean, dat_neutral) %>%
  group_by(subject,video_set, emotion ,group ) %>%
  summarise(mean = mean(intensity,na.rm=TRUE))
write.xlsx(data, "objects/summary_intensity_subjects.xlsx")


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

fit0 <- lmer(intensity ~ group   + (1|subject) , data = intensity_mean%>%filter(correct == 1))
fit1 <- lmer(intensity ~ emotion + group + (1|subject) , data = intensity_mean%>%filter(correct == 1))
fit2 <- lmer(intensity ~ emotion + group + video_set + (1|subject) , data = intensity_mean%>%filter(correct == 1))
fit3 <- lmer(intensity ~ emotion * group + (1|subject) , data = intensity_mean%>%filter(correct == 1))
fit4 <- lmer(intensity ~ emotion * group * video_set + (1|subject) , data = intensity_mean%>%filter(correct == 1))
anova(fit0,fit1,fit2,fit3,fit4)
  
  # Generate table summary
  table <- tab_model(fit) #, show.df = FALSE, string.p = "p adjusted", p.adjust = "bonferroni")
  visualize(fit, plot = "residuals")
  # Create ANOVA table
  # Perform ANOVA
  fit <- fit4
  chiquadro <- car::Anova(fit, test.statistic= "Chisq", type = "3") # test.statistic=c("Chisq", "F")
  plot(allEffects(fit))
  
  chi_table <- chiquadro %>%
    drop_na(`Pr(>Chisq)`) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    kbl(caption = "Anova(model, type = 3)") %>%
    column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
    kable_classic(full_width = F, html_font = "Cambria")
  # Convert the HTML output of kable to flextable
  chi_table <- chiquadro %>%
    tibble::rownames_to_column("Effect") %>%
    dplyr::select(Effect, everything()) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()%>% 
    theme_vanilla() %>% 
    align(align = "center")
  

  #Contrasts
  emotion<-emmeans(fit, pairwise ~ emotion)
  tabella_emotion <- data.frame(as.data.frame(summary(emotion)$contrasts))
  
  video_set<-emmeans(fit, pairwise ~ video_set)
  tabella_video_set <- data.frame(as.data.frame(summary(video_set)$contrasts))
  
  emotion_video_set<-emmeans(fit, pairwise ~ video_set|emotion)
  tabella_emotion_video_set <- data.frame(as.data.frame(summary(emotion_video_set)$contrasts))
  
  
  
  contrasts <- rbind(tabella_emotion%>%mutate(video_set = "all",emotion = "", effect = "emotion")%>%
                       select(contrast, estimate, SE, df, t.ratio, p.value, video_set, emotion, effect),
                     tabella_video_set%>%mutate(video_set = "",emotion = "all", effect = "video_set")%>%
                       select(contrast, estimate, SE, df, t.ratio, p.value, video_set, emotion, effect),
                     tabella_emotion_video_set%>%mutate(video_set = "", effect = "emotion|video_set")%>%
                       select(contrast, estimate, SE, df, t.ratio, p.value, video_set, emotion, effect)) %>%
    select(effect,video_set, emotion, contrast, estimate, SE, df, t.ratio, p.value)
  
  
  contrast<-contrasts%>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit() %>%
    fontsize( size = 8)%>% 
    theme_vanilla() %>% 
    align(align = "center")%>%
    width(j = 1:9, width = .7) 
  
  
  
  
  JeFEEdata <- intensity_mean%>%filter(correct == 1) %>% filter(video_set == "JeFEE")
  ADFESdata <- intensity_mean%>%filter(correct == 1) %>% filter(video_set == "ADFES")
  fit_JeFEE <- lmer(intensity ~ emotion * group  + (1|subject) , data = JeFEEdata)
  fit_ADFES <- lmer(intensity ~ emotion * group  + (1|subject) , data = ADFESdata)
  
  JeFEE_chiquadro <- car::Anova(fit_JeFEE, type = 3) 
  
  JeFEE_table<- JeFEE_chiquadro %>%
    tibble::rownames_to_column("Effect") %>%
    dplyr::select(Effect, everything()) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()%>%
    fontsize( size = 10)%>% 
    theme_vanilla() %>% 
    align(align = "center")
  
  ADFES_chiquadro <- car::Anova(fit_ADFES, type = 3) 
  
  ADFES_table<- ADFES_chiquadro %>%
    tibble::rownames_to_column("Effect") %>%
    dplyr::select(Effect, everything()) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()
  
  emotion<-emmeans(fit_ADFES, pairwise ~ emotion)
  tabella_emotion <- data.frame(as.data.frame(summary(emotion)$contrasts))
  
  contrastsADFES <- rbind(tabella_emotion%>%mutate(video_set = "ADFES",emotion = NA, effect = "emotion"))%>%
    select(effect,video_set, emotion, contrast, estimate, SE, df, t.ratio, p.value)%>%
    mutate_if(is.numeric,~round(., digits = 4)) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()%>%
    fontsize( size = 8)%>% 
    theme_vanilla() %>% 
    align(align = "center") %>%
    width(j = 1:9, width = .7) 
  
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
  
  plotvideoemotion <- ggpredict(fit, terms = c( "emotion", "video_set"))%>%
    plot()+ 
    labs(title = "Interaction Video set * emotion",
         x = "Emotions",
         y = "Intensity (px)") +
    themeperso
  
  ploteffect<-ploteffect<-cowplot::plot_grid(plotemotion,
                                 plotvideo,plotvideoemotion,
                                 ncol = 1,
                                 scale = c(.9, .9, .9))
  

# Save the results
save(table_intensity, table, chi_table, contrast,JeFEE_table,ADFES_table,contrastsADFES,  ploteffect,table_intensity, plot_emotion,plotvideoemotion,  file = file.path("models/perceived_intensity.RData"))
load(file.path("models","perceived_intensity.RData"))


# Creating a Word document with the table
library(officer)
doc <- officer::read_docx()
doc <- body_add_flextable(doc, value = table_intensity)
doc <- body_add_par(doc, value ="", style ="Normal")

doc <- body_add_gg(doc, value = plot_emotion, style = "centered")
doc <- body_add_par(doc, value ="", style ="Normal")

doc <- body_add_break(doc)
doc <- body_add_par(doc, value ="Full vars", style ="Normal")
doc <- body_add_flextable(doc, value = chi_table)
doc <- body_add_par(doc, value ="", style ="Normal")

doc <- body_add_flextable(doc, value = contrast)
doc <- body_add_par(doc, value ="", style ="Normal")

doc <- body_add_break(doc)
doc <- body_add_par(doc, value ="JeFEE sub_dataset", style ="Normal")
doc <- body_add_flextable(doc, value = JeFEE_table)
doc <- body_add_par(doc, value ="", style ="Normal")

doc <- body_add_break(doc)
doc <- body_add_par(doc, value ="ADFES sub_dataset", style ="Normal")
doc <- body_add_flextable(doc, value = ADFES_table)
doc <- body_add_par(doc, value ="", style ="Normal")

doc <- body_add_par(doc, value ="", style ="Normal")
doc <- body_add_flextable(doc, value = contrastsADFES)

file_path <- "objects/summary_intensity.docx"
print(doc, target = file_path)

#################################################
# 
# END
#
############################# perceived intensity