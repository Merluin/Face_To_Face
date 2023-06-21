###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the uncertainty and bais measures of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the uncertaunty and bais.
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

# GEW is composed by 16 emotions' category. Each emotion/category fill 22,5° of the GEW (360°/16 = 22,5°)
# With respect to its angle's reference,  each emotion category extends from 11,25° to - 11,25°. Correct responses are categorised if the Resp.angle fall in the correct label interval: 
# Example: Video.emotion == fear (video seen), Wheel.angle == 236.25° (angle reference), Resp.angle = 240°, the Pt response fall in the 22,5° interval.


# "uncertainty and bais" will analyse what alternative/error is made (that is to say if some emotion increase the uncertainty) and if so, if such error is systematic (bais)
# We will:
# 1) 
# 2) 
# 3) 
# 4) 

# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.category, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "category","correct")) %>%
  filter(subject != "10_moebius") # no match

error <- correct_data %>%
  filter(correct == 0) %>%
  group_by(subject, group, emotion, video_set, category) %>%
  summarise(bais = n()) %>%
  group_by(group, emotion, video_set, category) %>%
  summarise(bais = sum(bais)/18)
  

test_list <- list()  # Creazione di una lista vuota per i dataset separati
video_setlevel <- c("ADFES", "JeFEE")
grouplevel <- c("control", "moebius")
emotionlevel <- c("surprise" ,"anger" ,"disgust" ,"sadness" ,"fear" ,"happiness")

for (i in 1:length(video_setlevel)) {
  for (ii in 1:length(grouplevel)) {
    for (iii in 1:length(emotionlevel)) {

      
      temp <- error %>%
        filter(group == grouplevel[ii] & emotion == emotionlevel[iii] & video_set == video_setlevel[i]) 
      
      if (nrow(temp) > 0) {
        
        temp<-temp%>%
          complete(category, fill = list(bais = 0, expected = 1/15)) %>%
          filter(category != emotionlevel[iii] & category != "no resp") %>%
          mutate(percent = (bais / sum(bais)) * 100)
        
        # Aggiungi il dataset separato alla lista
        name <- paste(grouplevel[ii], emotionlevel[iii], video_setlevel[i], sep = "_")
        test_list[[name]] <- temp
        print(name)
        
      } 
    }
  }
}

# Funzione per applicare il test del chi-quadro e restituire i risultati
apply_chi_square <- function(percent, expected) {
  chisq.test(percent, p = expected)
}

# Applicazione del test del chi-quadro a ogni coppia di dati nella lista
result_list <- lapply(1:length(test_list), function(i) {
  apply_chi_square(test_list[[i]]$percent, test_list[[i]]$expected)
})
names(result_list) <- names(test_list)


# Creazione della tabella con il nome della lista e il valore p
result_table <- data.frame(Lista = names(result_list),
                           P_value = sapply(result_list, function(x) x$p.value))

# Mostra la tabella dei risultati
print(result_table %>%
        mutate(Significativo = ifelse(P_value < 0.05, "*", "")))

m<-test_list$control_happiness_ADFES %>%
  data.frame() %>%
  select(percent,expected)

library(fifer)
chisq.post.hoc(result_list$control_happiness_ADFES, test='chisq.test')

#################################################
# 
# END
#
############################# uncertainty bais