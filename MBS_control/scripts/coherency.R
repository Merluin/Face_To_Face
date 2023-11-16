###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response.
#
#  Experiment   MBScontrol
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------

# dataset of resp count
load(file.path("objects","mbs_circular.RData"))

# Calculate absolute frequencies by categories
dataset <-  dataset_gw1 %>%
  dplyr::select( Exp.trigger, Exp.bloch, Pt.match, Video.set, Video.emotion, Pt.group, Resp.category) %>%
  'colnames<-'(c( "trigger","bloch", "match","video_set", "emotion", "group", "cat")) %>%
  rowwise() %>% 
  mutate(distance = ifelse(cat != "no resp" ,distance_clockwise(emotion, cat), NA)) 

data<- dataset %>%
  select(trigger, bloch, match, video_set, emotion, group, distance) %>%
  spread(group,distance, fill = NA) %>%
mutate(contigency = case_when( control == 0 & moebius == 0 ~ "a",
                                 control > moebius ~ "b",
                                 control < moebius ~ "c",
                                 control == moebius & control != 0 ~ "d")) %>%
  group_by(video_set, emotion,contigency) %>%
  summarise(total_count = n()) %>%
  spread(contigency,total_count, fill = 0)

# New row to add
new_row <- data.frame(video_set = "total", emotion = "all", a = sum(data$a), b = sum(data$b), c = sum(data$c), d = sum(data$d),X.NA. = NA)

# Add the new row
data <- rbind(data.frame(data), new_row)

# Compute exploration kappa and Cohen's kappa for each row in the dataset
for(i in 1:nrow(data)){
  x <- matrix(unlist(data[i,c("a","b","c","d")]), nrow = 2, byrow = T)
  
  e <- agreement(x)
  data$agreement[i] <- round(e,2)
  
  k <- cohen.kappa(x)
  
  data$kappa[i]   <- round(k$kappa,3)
  data$lower[i]   <- round(k$confid[1,1],2)
  data$upper[i]   <- round(k$confid[1,3],2)
  
}

kappa_distance <- data

plot<- dataset %>%
  group_by(group,emotion, distance,video_set) %>%
  summarise(count = as.numeric(n())) %>%
  data.frame() 

plot%>%
  ggplot(aes(x= distance, y = count, fill = group, color = group)) +
  geom_col(position=position_dodge()) +
  facet_grid(emotion ~ video_set)

# Calculate absolute frequencies
data <-  dataset_gw1 %>%
  dplyr::select( Exp.trigger, Exp.bloch, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c( "trigger","bloch", "match","video_set", "emotion", "group", "correct")) %>%
  spread(group,correct, fill = NA) %>%
  mutate(contigency = case_when( control == 1 & moebius == 1 ~ "a",
                                 control == 1 & moebius == 0 ~ "b",
                                 control == 0 & moebius == 1 ~ "c",
                                 control == 0 & moebius == 0 ~ "d")) %>%
  group_by(video_set, emotion,contigency) %>%
  summarise(total_count = n()) %>%
  spread(contigency,total_count, fill = 0)

# New row to add
new_row <- data.frame(video_set = "total", emotion = "all", a = sum(data$a), b = sum(data$b), c = sum(data$c), d = sum(data$d))

# Add the new row
data <- rbind(data.frame(data), new_row)

# Compute exploration kappa and Cohen's kappa for each row in the dataset
for(i in 1:nrow(data)){
  x <- matrix(unlist(data[i,c("a","b","c","d")]), nrow = 2, byrow = T)
  
  e <- agreement(x)
  data$agreement[i] <- round(e,2)
  
  k <- cohen.kappa(x)
  
  data$kappa[i]   <- round(k$kappa,3)
  data$lower[i]   <- round(k$confid[1,1],2)
  data$upper[i]   <- round(k$confid[1,3],2)
  
}

kappa_acc <- data

# single case confrontation
data <-  dataset_gw1 %>%
  dplyr::select( Pt.code, Exp.trigger, Exp.bloch, Video.set, Video.emotion,  Resp.correct) %>%
  'colnames<-'(c( "subject","trigger","bloch", "video_set", "emotion",  "correct")) %>%
  spread(subject,correct, fill = NA) 


mbs1 <- data %>%
  select(trigger, bloch, video_set, emotion, `1_moebius` , contains("control")) %>%
  gather(control, resp, contains("control") ) %>%
  mutate(contigency = case_when( resp == 1 & `1_moebius` == 1 ~ "a",
                                 resp == 1 & `1_moebius` == 0 ~ "b",
                                 resp == 0 & `1_moebius` == 1 ~ "c",
                                 resp == 0 & `1_moebius` == 0 ~ "d")) %>%
  group_by(control,video_set, emotion,contigency) %>%
  summarise(total_count = n()) %>%
  spread(contigency,total_count, fill = 0)

cols_name <- c( "trigger", "bloch", "video_set", "emotion","resp_mbs", "control", "resp_control", "moebius")
contigencytable <- rbind(data %>%
  select(trigger, bloch, video_set, emotion, `1_moebius` , contains("control")) %>%
  gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_1") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `2_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_2") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `3_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_3") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `4_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_4") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `5_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_5") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `6_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_6") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `7_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_7") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `8_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_8") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `9_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_9") %>%
    'colnames<-'(cols_name),
  data %>%
    select(trigger, bloch, video_set, emotion, `10_moebius` , contains("control")) %>%
    gather(control, resp, contains("control")) %>%
    mutate(moebius = "mbs_10") %>%
    'colnames<-'(cols_name))%>%
  mutate(contigency = case_when( resp_control == 1 & resp_mbs == 1 ~ "a",
                                 resp_control == 1 & resp_mbs == 0 ~ "b",
                                 resp_control == 0 & resp_mbs == 1 ~ "c",
                                 resp_control == 0 & resp_mbs == 0 ~ "d")) %>%
  group_by(moebius,control,video_set, emotion,contigency) %>%
  summarise(total_count = n()) %>%
  spread(contigency,total_count, fill = 0)


for(i in 1:nrow(contigencytable)){
  x <- matrix(unlist(contigencytable[i,c("a","b","c","d")]), nrow = 2, byrow = T)
  
  e <- agreement(x)
  contigencytable$agreement[i] <- e
  
  k <- cohen.kappa(x)
  
  contigencytable$kappa[i]   <- k$kappa
  contigencytable$lower[i]   <- round(k$confid[1,1],2)
  contigencytable$upper[i]   <- round(k$confid[1,3],2)
  
  contigencytable <- contigencytable %>%
    mutate(agree = ifelse(kappa >= 0.7, "*","") ) %>%
    data.frame
}

agree <- contigencytable %>%
  filter( agree == "*") %>%
  select(  moebius, control, video_set,  emotion,agreement, kappa, lower, upper, a,b,c,d) %>%
  'colnames<-'(c( "obs_1","obs_2", "video_set", "emotion","agreement",  "Kappa", "lower", "upper","a","b","c","d"))

table(agree$obs_1,agree$emotion,agree$video_set)
#################################################
# 
# END
#
############################### matrix responses





range(data$distance)

# Lista delle etichette nel senso orario
labels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")

# Funzione per calcolare la distanza tra due etichette nel senso orario
distance_clockwise <- function(label1, label2) {
  labels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")
  index1 <- match(label1, labels)
  index2 <- match(label2, labels)
  distance <- abs(index2 - index1)
  if (distance > 8) {
    distance <- 16 - distance
  }
  return(distance)
}

# Esempio di utilizzo
announced_label <- "E"
response <- "A"
distance <- distance_clockwise(announced_label, response)
print(distance) 



  
