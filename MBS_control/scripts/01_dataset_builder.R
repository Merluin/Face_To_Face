###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        23/08/2022
#  Description: Generate the dataset from Psychopy
#  Experiment:  MBScontrol
#
#  Update:      03/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Load custom functions using devtools package
devtools::load_all()

# Concatenate all files from Psychopy
# Files are .csv placed in the "original_data" folder
dataset <- list.files(path = "original_data", full.names = TRUE, pattern = 'csv') %>%
  lapply(., function(x) read.csv(x, sep = ",", header = TRUE, stringsAsFactors = FALSE)) %>%
  lapply(clean_practice) %>%
  bind_rows()

# Modify the dataset by adding subject and trial information
dataset <- dataset %>%
  mutate(subject = ifelse(tolower(group) == "moebius", participant, participant + length(unique(participant)))) %>%
  # group_by(subject) %>% 
  # mutate(trial = 1:n()) %>% 
  # ungroup()%>%
  mutate(exp.thisN = ifelse(exp_blocks.thisRepN == 0, exp.thisN+1, exp.thisN+57)) # 56 trial per blocks

# note : exp.ThisRepN blocco 1/2 rename trial exp.thisN

# Define the desired column names for the dataset
namecols<-c("ID.subject", "Exp.trigger" ,"Exp.date", "Exp.trial", "Exp.bloch", "Pt.code", "Pt.gender", "Pt.study", "Pt.age", "Pt.group",
            "Wheel.name", "Resp.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
            "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id")

# Create the "Pct" sub_dataset
Pct <- dataset %>%
  filter(loop_practice.thisRepN >= 0) %>%
  dplyr::select("ID.subject", "trigger","date", "exp.thisN", "exp_blocks.thisRepN","subject", "sex", "education", "age",  "group",
                "practice", "primary.time", "primary.x", "primary.y",
                "file_duration", "file", "file_emotion_level", "file_gender",
                "file_emotion", "file_id") %>%
  'colnames<-'(namecols) %>%
  mutate(Wheel.name = "GW1", 
         Wheel.task = "practice",
         Video.name = str_replace(Video.name, "stimoli/03.practice/", ""))

# Create the "Gw1" sub_dataset
Gw1 <- dataset %>%
  filter(exp_blocks.thisRepN >= 0) %>%
  dplyr::select("ID.subject", "trigger","date", "exp.thisN", "exp_blocks.thisRepN", "subject", "sex", "education", "age",  "group",
                "practice", "primary.time", "primary.x", "primary.y",
                "file_duration", "file", "file_emotion_level", "file_gender",
                "file_emotion", "file_id") %>%
  'colnames<-'(namecols) %>%
  mutate(Wheel.name = "GW1", 
         Wheel.task = "task",
         Video.name = str_replace(Video.name, "stimoli/01.full/", ""),
         Video.name = str_replace(Video.name, "stimoli/02.subtle/", "")) %>%
  drop_na(Video.id)

# Create the "Gw2" sub_dataset
Gw2 <- dataset %>%
  filter(exp_blocks.thisRepN >= 0) %>%
  dplyr::select("ID.subject", "trigger","date", "exp.thisN", "exp_blocks.thisRepN", "subject", "sex", "education", "age", "group",
                "practice", "secondary.time", "secondary.x", "secondary.y",
                "file_duration", "file", "file_emotion_level", "file_gender",
                "file_emotion", "file_id") %>%
  'colnames<-'(namecols) %>%
  mutate(Wheel.name = "GW2", 
         Wheel.task = "task",
         Video.name = str_replace(Video.name, "stimoli/01.full/", ""),
         Video.name = str_replace(Video.name, "stimoli/02.subtle/", "")) %>%
  drop_na(Video.id)

# Create the "data_full" dataset by combining "Pct", "Gw1" and "Gw2" and set the vars types
dataset <- rbind(Pct, Gw1, Gw2) %>%
  mutate(
    Exp.date = strptime(Exp.date, format = "%Y_%b_%d_%H%M"),
    Exp.date = as.Date(Exp.date, format = "%Y_%b_%d"),
    Exp.trial = as.numeric(Exp.trial),
    Exp.trigger = as.numeric(Exp.trigger),
    Pt.id = as.factor(Pt.code),
    Pt.code = as.factor(ID.subject),
    Pt.gender = ifelse(Pt.gender == "f", "female", "male"),
    Pt.gender = factor(Pt.gender, levels = c("female","male")),
    Pt.group = factor(Pt.group, levels = c("control","moebius")),
    Pt.study = as.numeric(Pt.study),
    Pt.age = as.numeric(Pt.age),
    Pt.match = parse_number(ID.subject),
    Wheel.name = factor(Wheel.name, levels = c("GW1","GW2")),
    Wheel.task = as.factor(Wheel.task),
    Wheel.x = str_remove_all(Wheel.x, "[\\[|\\] ']"),
    Wheel.y = str_remove_all(Wheel.y, "[\\[|\\] ']"),
    Resp.rt = str_remove_all(Resp.rt, "[\\[|\\] ']"),
    Wheel.x = as.numeric(Wheel.x),
    Wheel.y = as.numeric(Wheel.y),
    Resp.rt = as.numeric(Resp.rt),
    Resp.rt = round(Resp.rt * 1000, 1),
    Resp.rt = replace_na(Resp.rt, 19999),
    Video.set = ifelse(Video.intensity == "full", "ADFES", "JeFEE"),
    Video.set = factor(Video.set, levels = c("ADFES", "JeFEE")),
    Video.gender = ifelse(Video.gender == "femmina", "female", "male"),
    Video.gender = as.factor(Video.gender),
    Video.name = as.factor(Video.name),
    Video.emotion = case_when(
      Video.emotion == "angry" ~ "anger",
      Video.emotion == "disgusted" ~ "disgust",
      Video.emotion == "fear" ~ "fear",
      Video.emotion == "happy" ~ "happiness",
      Video.emotion == "neutral" ~ "neutrality",
      Video.emotion == "sad" ~ "sadness",
      Video.emotion == "surprised" ~ "surprise",
      Video.emotion == "anger" ~ "anger",
      Video.emotion == "disgust" ~ "disgust",
      Video.emotion == "fear" ~ "fear",
      Video.emotion == "joy" ~ "happiness",
      Video.emotion == "neutral" ~ "neutrality",
      Video.emotion == "sadness" ~ "sadness",
      Video.emotion == "surprise" ~ "surprise"),
    Video.emotion = factor(Video.emotion, levels = c("surprise","anger", "disgust", "sadness", "fear", "happiness", "neutrality")),
    Video.id = as.factor(Video.id)
  ) %>%
  arrange(Pt.id) %>%
  dplyr::select(-c(ID.subject, Video.intensity))

# correction demografic info 7_moebius
dataset$Pt.gender[dataset$Pt.code == "7_moebius"] <- "female"

# Load the Demographic Dataset from the Google Sheet
# Files are .csv placed in the "original_data/demography" folder
temp <- list.files("original_data/demography", pattern = 'Partecipanti') 
demographic <- read.csv(file.path("original_data/demography", temp), sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "aa") %>%
  filter(!is.na(Pt.id))%>%
  mutate_if(grepl("Asmt", names(.)), ~ as.numeric(gsub(",", ".", .)))%>%
  mutate(Plsy.locus = case_when(Plsy.locus == "Bilaterale"~ "bilateral",
                                Plsy.locus == "Monolaterale"~ "unilateral",
                                is.na(Plsy.locus) ~ NA),
         Surg.type = case_when(Surg.type == "DINAMICA"~ "dynamic",
                               Surg.type == "STATICA"~ "static",
                               is.na(Surg.type) ~ NA),
         Surg.date = ifelse(Surg.date == 0, NA, Surg.date),
         Surg.date = format(as.Date(as.character(Surg.date), format = "%Y"), "%Y"))
  

# Combine the Psychopy and Demographic datasets using Pt.code as the key
temp <- left_join(dataset, demographic%>%dplyr::select(-c(Pt.id,Pt.group)), by = "Pt.code")

# Setting the circular Coordinates
temp <- temp %>% 
  mutate(x_cen = Wheel.x,
         y_cen = Wheel.y,
         Resp.intensity = calc_dist(point_x = x_cen, point_y = y_cen),
         x_cen = ifelse(x_cen == 0, x_cen + 0.0001, x_cen),
         y_cen = ifelse(y_cen == 0, y_cen + 0.0001, y_cen),
         theta = atan(y_cen/x_cen),
         theta = correct_angle(theta, x_cen, y_cen), # correcting for quadrant
         degree = round(rad_to_deg(theta),2)) # convert to degrees

# Here we create a dataframe with the "correct" value for each emotion. In this way
# we can calculate the difference between the pressed angle and the correct one
coords <- tibble(
  emotion = c("satisfaction", "happiness", "elation", "pride", "anger", "contempt",
              "disgust", "envy", "guilt", "shame", "fear", "sadness", "surprise", 
              "interest", "hope", "relief")
)

coords <- coords %>% 
  mutate(theta_emo = (2 * 0:(nrow(.) - 1) * pi)/nrow(.), # see https://math.stackexchange.com/a/206662
         theta_emo = theta_emo + deg_to_rad((360/nrow(.))/2), # adding the shift for centering emotions
         x_emo = 300 * cos(theta_emo),
         y_emo = 300 * sin(theta_emo),
         degree_emo = rad_to_deg(theta_emo))

#Calculate coordinates for radial lines
coords$end_x <- cos(coords$degree_emo * pi / 180)
coords$end_y <- sin(coords$degree_emo * pi / 180)

# Create a circular plot with radial lines and labels
plot <- ggplot(coords) +
  geom_segment(aes(x = 0, y = 0, xend = end_x, yend = end_y), color = "steelblue", size = 1) +
  geom_text(aes(x = 1.1 * end_x, y = 1.1 * end_y, label = emotion), size = 4, color = "black") +
  geom_text(aes(x = 1.5 * end_x, y = 1.5 * end_y, label = degree_emo), size = 4, color = "black") +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  ggtitle("Degrees of Emotion") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# assigning order to coord
coords$order <- c(4, 3, 2, 1, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5)
coords$emo_order <- coords$emotion[coords$order]

# insert Wheel.angle referenses in dataset
names(coords$degree_emo) <- coords$emotion # renaming for expanding
temp$Wheel.angle <- coords$degree_emo[match(temp$Video.emotion, coords$emotion)]

# Adding variables
# 1 the angular difference between the angle reference and the pressed angle.
# 2 categorical GEW responses 
temp <- temp %>% 
  mutate(emotion = Video.emotion ,
         Resp.angle = degree,
         Resp.diff = ang_diff(Wheel.angle, Resp.angle),
         Resp.diff = unname(Resp.diff),        
         Wheel.angle = unname(Wheel.angle),
         Resp.category = seg_position(round(degree, 1)),
         Resp.category = factor(Resp.category,levels = c(coords$emo_order,"no resp")),
         Resp.level = level_int_position(Resp.intensity),
         Resp.level = factor(Resp.level),
         Pt.code = factor(Pt.code),
         Resp.correct = ifelse(as.character(Video.emotion) == as.character(Resp.category), 1, 0),
         Resp.correct = ifelse(Wheel.name == "GW2", NA, Resp.correct))%>%
  dplyr::select(-c("x_cen","y_cen", "degree", "emotion","theta" ))

# Arrange column order by names
dataset_full <- temp[ , rev(order(names(temp)))]

# select sub_datasets
dataset_gw1 <- dataset_full %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,Video.emotion != "neutrality")



dataset_neutral <- dataset_full %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,Video.emotion == "neutrality")

# Saving
save(coords,dataset_full,dataset_gw1,dataset_neutral,file = file.path("objects", "mbs_circular.RData"))

dataset_gw1 <- dataset_full %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1")
# Export Pt data gw1 csv for EEG
for(i in 1:length(dataset_gw1$Pt.code)){
  temp <- dataset_gw1$Pt.code[i]
write.csv(dataset_gw1%>%filter(Pt.code==temp), paste0("objects/",temp,"_behavioral.csv"), row.names=FALSE)
}
#################################################
# 
# END
#
################################ dataset_builder