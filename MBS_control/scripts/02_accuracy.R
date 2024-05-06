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
  dplyr::select(Pt.code, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"match","video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject,match,group, emotion, video_set) %>%
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
#anger
#disgust
  x <- correct_data %>%
  mutate(correct = as.factor(correct),
         video_set = as.factor(video_set)) %>%
  na.omit() 

# subject != "5_moebius",
# subject != "7_moebius",
# subject != "8_moebius",
# subject != "10_moebius",

  # filter(match != 5,
  #        match != 7,
  #        match != 8,
  #        match != 10,
  #        emotion == "anger")

  fit0 <- glmer(correct ~ group   + (1|subject) , data = x, family = binomial)
  fit1 <- glmer(correct ~ emotion + group + (1|subject) , data = x, family = binomial)
  fit2 <- glmer(correct ~ emotion + group + video_set + (1|subject) , data = x, family = binomial)
  fit3 <- glmer(correct ~ emotion * group + (1|subject) , data = x, family = binomial)
  fit4 <- glmer(correct ~ emotion * group * video_set + (1|subject) , data = x, family = binomial)
anova(fit0,fit1,fit2,fit3,fit4)
fit <- fit4
  # Generate table summary
  table <- tab_model(fit ) #, show.df = FALSE, string.p = "p adjusted", p.adjust = "bonferroni")
  
  # Create ANOVA table
  # Perform ANOVA
    
    chiquadro <- car::Anova(fit, type = 3) 
    plot(allEffects(fit))
    
  chi_table <- chiquadro %>%
    kbl(caption = "correct ~ emotion * group * video_set, p-values via partial sums of squares, Accuracy n=20") %>%
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
  
  # Conditional formatting in flextable
  # ft_chi_table <- set_table_properties(ft_chi_table, layout = "autofit")
  # ft_chi_table <- color(ft_chi_table, i = ~ `Pr(>Chisq)` <= 0.05, j = 4, color = "red")
  # 
  #Contrasts
  emotion<-emmeans(fit, pairwise ~ emotion)
  tabella_emotion <- data.frame(as.data.frame(summary(emotion)$contrasts))
  
  video_set<-emmeans(fit, pairwise ~ video_set)
  tabella_video_set <- data.frame(as.data.frame(summary(video_set)$contrasts))
  
  emotion_video_set<-emmeans(fit, pairwise ~ video_set|emotion)
  tabella_emotion_video_set <- data.frame(as.data.frame(summary(emotion_video_set)$contrasts))
  
  emotion_group_video_set<-emmeans(fit, pairwise ~ group|emotion|video_set)
  tabella_emotion_group_video_set <- data.frame(as.data.frame(summary(emotion_group_video_set)$contrasts))
  
  
  
  contrasts <- rbind(tabella_emotion%>%mutate(video_set = "all",emotion = NA, effect = "emotion"),
                     tabella_video_set%>%mutate(video_set = NA,emotion = "all", effect = "video_set"),
                     tabella_emotion_video_set%>%mutate(video_set = NA, effect = "emotion|video_set"),
                     tabella_emotion_group_video_set%>%mutate(effect = "group|emotion|video_set"))%>%
    select(effect,video_set, emotion, contrast, estimate, SE, df, z.ratio, p.value)
  
  
  contrast<-contrasts%>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit() %>%
    fontsize( size = 8)%>% 
    theme_vanilla() %>% 
    align(align = "center")%>%
    width(j = 1:9, width = .7) 
  

  # glmer by video set
  JeFEEdata <- x %>% filter(video_set == "JeFEE")
  ADFESdata <- x %>% filter(video_set == "ADFES")
  fit_JeFEE <- glmer(correct ~ emotion * group  + (1|subject) , data = JeFEEdata, family = binomial)
  fit_ADFES <- glmer(correct ~ emotion * group  + (1|subject) , data = ADFESdata, family = binomial)
  
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
  
  emotion<-emmeans(fit_JeFEE, pairwise ~ emotion)
  tabella_emotion <- data.frame(as.data.frame(summary(emotion)$contrasts))
  
  emotiongroup<-emmeans(fit_JeFEE, pairwise ~ group|emotion)
  tabella_emotiongroup_Jefee <- data.frame(as.data.frame(summary(emotiongroup)$contrasts))
  
  contrastsJEFEE <- rbind(tabella_emotion%>%
                            mutate(video_set = "JeFEE",emotion = NA, effect = "emotion"),
                          tabella_emotiongroup_Jefee%>%mutate(video_set = "JeFEE", effect = "emotion|group"))%>%
    select(effect,video_set, emotion, contrast, estimate, SE, df, z.ratio, p.value)%>%
    mutate_if(is.numeric,~round(., digits = 4)) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()%>%
    fontsize( size = 8)%>% 
    theme_vanilla() %>% 
    align(align = "center")%>%
    width(j = 1:8, width = .7) 
  
  
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
    select(effect,video_set, emotion, contrast, estimate, SE, df, z.ratio, p.value)%>%
    mutate_if(is.numeric,~round(., digits = 4)) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()%>%
    fontsize( size = 8)%>% 
    theme_vanilla() %>% 
    align(align = "center") %>%
    width(j = 1:9, width = .7) 
  
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
          plotemovideo,
          plotint3,
          ncol = 2,
          scale = c(.9, .9, .9,.9))
  
  
  

# Save the results
  save(fit, table, chi_table, contrast,JeFEE_table,contrastsJEFEE,ADFES_table,contrastsADFES, ploteffect,table_accuracy, plot_freq_gw1, plot_freq_gw2,plotint3,  file = file.path("models","accuracy.RData"))
  load(file.path("models","accuracy.RData"))
  

  
  # Creating a Word document with the table
  library(officer)
  doc <- officer::read_docx()
  doc <- body_add_flextable(doc, value = table_accuracy)
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_gg(doc, value = plotint3, style = "centered")
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_break(doc)
  doc <- body_add_par(doc, value ="Full vars", style ="Titolo 1")
  doc <- body_add_flextable(doc, value = chi_table)
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_flextable(doc, value = contrast)
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_break(doc)
  doc <- body_add_par(doc, value ="JeFEE sub_dataset", style ="Normal")
  doc <- body_add_flextable(doc, value = JeFEE_table)
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_flextable(doc, value = contrastsJEFEE)
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_break(doc)
  doc <- body_add_par(doc, value ="ADFES sub_dataset", style ="Normal")
  doc <- body_add_flextable(doc, value = ADFES_table)
  doc <- body_add_par(doc, value ="", style ="Normal")
  
  doc <- body_add_par(doc, value ="", style ="Normal")
  doc <- body_add_flextable(doc, value = contrastsADFES)
  
  # file_path <- "objects/summary_accuracy.docx"
  print(doc, target = file_path)

#################################################
# 
# END
#
######################################## accuracy