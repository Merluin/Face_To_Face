###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs plots from data analysis for the CARIPARO experiment.
#
#  Experiment   Onlinecontrol
#
#  Update:      23/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggrain)
figure<-function(data, type){
  
  colors <- c("ADFES" = "#3271AD", "JeFEE" = "#C56637")  # blue and violet
  
  # stat summary
  if(type == "acc"){
    df <- data %>% dplyr::select(-intensity) %>%
      'colnames<-'(c("subject" , "video_set", "mean"))
    ytitle <- "Mean Accuracy (%)"
    
  } else if(type == "int"){
    df <- data %>% dplyr::select(-accuracy) %>%
      'colnames<-'(c("subject" , "video_set", "mean"))
    ytitle <- "Mean Intensity (px)"
  }
  
  descritive <- df %>%
    group_by(video_set) %>%
    summarise(mean = mean(mean),
              sd = sd(mean),
              se = sd(mean)/sqrt(n()),
              .groups = "drop")
  
  p <- ggplot(df[df$video_set %in% c("ADFES","JeFEE"),], 
              aes(video_set, mean, 
                  fill = video_set)) +
    geom_rain(alpha = .5, 
              rain.side = 'f1x1', 
              id.long.var = "subject") +
    geom_point(data = descritive, 
               aes(x = c(1.1, 1.9),
                   y = mean, 
                   fill = video_set), 
               fill = colors, 
               size = 3, 
               shape = 23, 
               color = "black") +
    geom_errorbar(data = descritive,
                  mapping = aes(x = c(1.1, 1.9), 
                                ymin = mean - se, 
                                ymax = mean + se),
                  inherit.aes = FALSE,
                  width = 0, 
                  color = "black", 
                  linewidth = 0.5) +
    scale_fill_manual(values = colors) +
    labs(x = NULL, 
         y = ytitle, 
         fill = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          text = element_text(size = 10, 
                              family = "Helvetica"),             # use Helvetica, size 16 for all text
          panel.background = element_blank(),                    # clean white background
          axis.line = element_line(colour = "black"),            # black axis lines
          strip.text.x = element_text(size = 6.6),               # facet labels (x): size 20
          strip.text.y = element_text(size = 6.6),
          panel.grid.major = element_blank(),                    # Rimuove le griglie principali
          panel.grid.minor = element_blank())+                   # facet labels (y): size 20
    geom_text(data = descritive, 
              aes(x = video_set, y = 0.1,
                  label = paste0("M = ", round(mean, 3), 
                                 "\nSD = ", round(sd, 3),"\n")),
              color = "black", 
              size = 2.5)
  
  return(p)
}

# Data --------------------------------------------------------------------
df_acc <- read_csv("data/study1_accuracy_behavioral_data.csv")%>%
  group_by(subject,video_set) %>%
  summarise( accuracy = mean(acc, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set))

df_int <- read_csv("data/study1_intensity_behavioral_data.csv")%>%
  group_by(subject,video_set) %>%
  summarise(intensity = mean(intensity, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set)) %>%
  ungroup() %>%
  dplyr::select(-c(subject,video_set))

df <- left_join(df_acc, df_int, by = "label") %>%
  dplyr::select(-label)


# Plots
p1 <- figure(df,"acc") # plot accuracy
p2 <- figure(df,"int") # plot intensity
p3 <- plot_grid(p1, p2, nrow = 1,labels = "AUTO") #combine plots

# Export
ggsave("figures/Fig_1_finale.png",  # export for draft         
       plot = p3,           
       device = "png",         
       dpi = 300,              
       width = 18,              
       height = 12,             
       units = "cm")

ggsave("figures/Fig_1.tiff",    # export for journal       
       plot = p3,           
       device = "tiff",         
       dpi = 1200,              
       width = 18,              
       height = 12,             
       units = "cm")
