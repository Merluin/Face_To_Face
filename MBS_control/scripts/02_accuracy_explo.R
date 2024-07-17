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
#replace_csv("group", "control")

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

correct_data_neu <-  dataset_neutral %>%
  dplyr::select(Pt.code, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"match","video_set", "emotion", "group", "correct"))

accuracy_neu<-correct_data_neu%>%
  group_by(subject,match,group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks

write.xlsx(accuracy, "objects/summary_subjects.xlsx")

# Fit  correct for each emotion--------------------------------------------

  # Adatta il modello di regressione logistica
#anger
#disgust
  x <- correct_data %>%
  mutate(correct = as.factor(correct),
         video_set = as.factor(video_set)) %>%
  na.omit() 

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
  group<-emmeans(fit_JeFEE, pairwise ~ group)
  tabella_group <- data.frame(as.data.frame(summary(group)$contrasts))
  inter<-emmeans(fit_JeFEE, pairwise ~ emotion * group)
  tabella_inter <- data.frame(as.data.frame(summary(inter)$contrasts))
  
  contrastsJEFEE <- rbind(tabella_emotion%>%
                            mutate(video_set = "JeFEE", effect = "emotion"),
                          tabella_group%>%
                            mutate(video_set = "JeFEE", effect = "group"),
                          tabella_inter%>%
                            mutate(video_set = "JeFEE",effect = "interaction"))%>%
    select(effect,video_set, contrast, estimate, SE, df, z.ratio, p.value)%>%
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
  group<-emmeans(fit_ADFES, pairwise ~ group)
  tabella_group <- data.frame(as.data.frame(summary(group)$contrasts))
  inter<-emmeans(fit_ADFES, pairwise ~ emotion * group)
  tabella_inter <- data.frame(as.data.frame(summary(inter)$contrasts))
  
  contrastsADFES <- rbind(tabella_emotion%>%
                            mutate(video_set = "ADFES", effect = "emotion"),
                          tabella_group%>%
                            mutate(video_set = "ADFES", effect = "group"),
                          tabella_inter%>%
                            mutate(video_set = "ADFES",effect = "interaction"))%>%
    select(effect,video_set, contrast, estimate, SE, df, z.ratio, p.value)%>%
    mutate_if(is.numeric,~round(., digits = 4)) %>%
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit()%>%
    fontsize( size = 8)%>% 
    theme_vanilla() %>% 
    align(align = "center") %>%
    width(j = 1:8, width = .7) 
  

  
  # Creating a Word document with the table
  library(officer)
  doc <- officer::read_docx()

  
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
  
   file_path <- "objects/summary_accuracy_explorative.docx"
  print(doc, target = file_path)

#################################################
# 
# END
#
######################################## accuracy