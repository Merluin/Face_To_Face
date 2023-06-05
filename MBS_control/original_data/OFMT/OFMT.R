
###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/2022
#  Description: Accuracy for OFMT
#
#  Update:      18/05/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# Functions ---------------------------------------------------------------

devtools::load_all()


# loading data ----
datasetname<-"dataset_OFMT"
OFMT_concatenation(datasetname)
load(paste0("objects/",datasetname,".RData") )



data<-dataset%>%
  filter(Attempt == 1, Zone.Type== "response_button_text", inclusionCheck == "test")%>%
  mutate(ANSWER = ifelse(pairType=="same","Della stessa persona","Di persone diverse"),
         Correct = ifelse(ANSWER == Response,1,0))%>%
  select(Participant.Public.ID, Reaction.Time, Correct,ANSWER,Response,face1, face2)%>%
  mutate(Correct = as.numeric(as.character(Correct)))%>%
  'colnames<-'(c("id" ,"rt.correct","correct","ref","resp","face1","face2"))

id<-dataset%>%
  filter(Question.Key == "response-3")%>%
  select(UTC.Date,Participant.Public.ID,Response)%>%
'colnames<-'(c("date" ,"id","subject"))


add_pt <- data.frame(
  date = "24/06/2022 16:58:38",
  id = "ium1nfma",
  subject = "01_moebius") # Pt with other exp version, v6

id<- rbind(id,add_pt)
  
scores <- data%>%
  group_by(id)%>%
  summarize(acc = sum(correct)/200) # 200 trials

id$subject[id$id == "7f34708g"] <- "02_control"
id$subject[id$id == "egrxe0fy"] <- "05_control"
id$subject[id$id == "izox3paa"] <- "08_moebius"

score <- left_join(scores,id, by = "id")%>%
  mutate(num = parse_number(subject),
         group = tolower(gsub( "[^A-Za-z]","",subject)))%>%
  arrange(subject,id)%>%
  mutate(subject = num)%>%
  select(date,id,subject,group,acc)


data%>%
  group_by(subject,ref)%>%
  summarise_at("correct",sum)%>%
  # group_by(ref)%>%
  # summarise_at("correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=correct,x=ref, fill= ref),stat='identity')+
  #geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_y_discrete(name="correct (%)")+
  scale_x_discrete(name="")

data%>%
  group_by(subject,ref)%>%
  summarise_at("rt.correct",mean)%>%
  group_by(ref)%>%
  summarise_at("rt.correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        legend.position = "none")+
  scale_y_discrete(name="RT correct (ms)",labels = c(0,3000))+
  scale_x_discrete(name="")

data%>%
  group_by(subject,ref)%>%
  summarise_at("resp.slider",mean)%>%
  group_by(ref)%>%
  summarise_at("resp.slider",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)

data%>%
  group_by(subject,ref)%>%
  summarise_at("rt.correct",mean)%>%
  group_by(ref)%>%
  summarise_at("rt.correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)





