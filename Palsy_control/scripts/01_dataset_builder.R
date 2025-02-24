###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        23/08/2022
#  Description: Generate the dataset from Gorilla
#  Experiment:  Palsycontrol
#
#  Update:      03/06/2023
###########################################################################

rm(list=ls()) # remove all objects

# Load custom functions using devtools package
devtools::load_all()


# Files are .csv placed in the "original_data" folder
folder_dir<-  file.path("original_data")
filetask<-list.files(folder_dir,pattern= 'task') # from gorilla
fileq<-list.files(folder_dir,pattern= 'questionnaire') # from gorilla
filedemo<-list.files(folder_dir,pattern= 'pazienti')

# Concatenate all files from Psychopy
temp <- read.csv(file.path(folder_dir,filetask), sep=",", header=TRUE,stringsAsFactors = FALSE)

# Create the "task" sub_dataset
dataset<-temp%>%
  filter(Response == "click"| Zone.Type ==  "timelimit_screen")%>%
  dplyr::select( "Local.Date.and.Time", "Trial.Number","Participant.Public.ID" ,
                 "Screen.Name", "Reaction.Time", "X.Coordinate", "Y.Coordinate", "display", 
                 "Videos", "intensity", "file_gender", "emotion", "identity")%>%
  'colnames<-'(c("Exp.date", "Exp.trial", "Pt.code",  
                 "Wheel.name", "Resp.rt", "Wheel.x", "Wheel.y", "Wheel.task", 
                 "Video.name", "Video.intensity", "Video.gender", "Video.emotion", "Video.id"))  %>%
  filter(Pt.code != "2ap2peoy" )
#pen6shs7 control no match
# 62cwsg59 palsy no match


# Load the Demographic Dataset from the Google Sheet
demographic <- read.csv(file.path(folder_dir,filedemo), sep=";", header=TRUE,stringsAsFactors = FALSE,na.strings= "aa")

unique(sort(unique(dataset$Pt.code)) == sort(unique(demographic$Pt.code)))
#2cgl5059 637owdws

# Combine the Psychopy and Demographic datasets using Pt.code as the key
temp <- left_join(dataset, demographic, by = "Pt.code")

# Create the "data_full" dataset by combining "Pct", "Gw1" and "Gw2" and set the vars types
dataset <- temp%>%
  mutate(Wheel.y = Wheel.y - 300,
         Wheel.y = Wheel.y * -1,  # flipping the y coordinates because gorilla use the upper left origin
         Wheel.x = Wheel.x - 300,
    Exp.date = strptime(Exp.date, format = "%d/%m/%Y %H:%M:%S"),
    Exp.date = as.Date(Exp.date, format = "%Y_%b_%d"),
    Exp.trial = as.numeric(Exp.trial),
    Pt.code = as.factor(Pt.code),
    Pt.gender = factor(Pt.gender, levels = c("female","male")),
    Pt.group = factor(Pt.group, levels = c("control","palsy")),
    Pt.study = as.numeric(Pt.study),
    Pt.age = as.numeric(Pt.age),
    Wheel.name = factor(Wheel.name, levels = c("GW1","GW2")),
    Wheel.task = ifelse(Wheel.task == "practice", "practice", "task"),
    Wheel.task = as.factor(Wheel.task),
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
  arrange(Pt.id, Exp.trial) %>%
  dplyr::select(-c( Video.intensity))



# Setting the circular Coordinates
temp <- dataset %>% 
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
  dplyr::select(-c("x_cen","y_cen", "degree", "emotion","theta" ))#%>%
  #drop_na(Wheel.x)

# Arrange column order by names
dataset_full <- temp[ , rev(order(names(temp)))]

# select sub_datasets
dataset_gw1 <- dataset_full %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,Video.emotion != "neutrality")


dataset_neutral <- dataset_full %>% 
  filter(Wheel.task == "task", Wheel.name == "GW1" ,Video.emotion == "neutrality")

# Saving
save(coords,dataset_full,dataset_gw1,dataset_neutral,file = file.path("objects", "palsy_circular.RData"))

#################################################
# 
# END
#
################################ dataset_builder