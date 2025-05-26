###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        04/06/2023
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy and intensity measures of the Online student experiment. 
#
#  Experiment Online student
#
#  Update:      23/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
library(tidyverse)
library(afex)
library(effectsize)
library(BayesFactor)

load(file.choose())
# Demographic Table ---------------------------------------------------------------
# dataset for flextable object
demogaphic<- dataset_gw1%>%
  group_by(Pt.code)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  dplyr::select(Pt.code, Pt.gender, Pt.age)%>%
  'colnames<-'(c("Subject","Gender","Age" ))

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
  select(-c(subject,video_set))

df <- left_join(df_acc, df_int, by = "label") %>%
  select(-label)
# ---- 0. Descriptive statistics

descritive <- df %>%
  group_by(video_set) %>%
  summarise(mean_acc = mean(accuracy),
            sd_acc = sd(accuracy),
            se_acc = sd(accuracy)/sqrt(n()),
            mean_int = mean(intensity),
            sd_int = sd(intensity),
            se_int = sd(intensity)/sqrt(n()),
            .groups = "drop")
print(descritive)

percentili_90 <- df %>%
  group_by(video_set, subject) %>%
  summarise(mean_acc = mean(accuracy),
            mean_int = mean(intensity),
            .groups = "drop") %>%
  group_by(video_set) %>%
  summarise(p05_acc = quantile(mean_acc, 0.05),
            p95_acc = quantile(mean_acc, 0.95),
            p05_int = quantile(mean_int, 0.05),
            p95_int = quantile(mean_int, 0.95),)
print(percentili_90)

# ---- 1. Repeated Measures ANOVA (video_set as factor) ----

# Use afex::aov_ez for Recognition accuracy
anova_acc <- aov_ez(id = "subject",
                    dv = "accuracy",
                    within = "video_set",
                    data = df)
summary(anova_acc)

# Use effect_size::eta_squared for Partial eta squared 
eta2 <- eta_squared(anova_acc, partial = TRUE)
print(eta2)

# Use effect_size::cohens_d for d_rm (for repeated measures)
# First compute the mean difference and pooled SD
df_wide <- df %>% 
  select( -intensity) %>%
  pivot_wider(names_from = video_set, values_from = accuracy) %>%
  drop_na()%>%
  column_to_rownames("subject")

d_rm <- cohens_d(df_wide$ADFES, df_wide$JeFEE, paired = TRUE, within = TRUE)
print(d_rm)

# Use BayesFactor::ttestBF Bayesian Repeated Measures ANOVA ----
# Now assign subject as rownames for BayesFactor
bf_data <- df_wide 

# Run Bayesian paired t-test
bf <- ttestBF(x = bf_data$ADFES, y = bf_data$JeFEE, paired = TRUE)
print(bf)

#---------------------------------------------
# Use afex::aov_ez for Recognition intensity
anova_int <- aov_ez(id = "subject",
                    dv = "intensity",
                    within = "video_set",
                    data = df)
summary(anova_int)

# Use effect_size::eta_squared for Partial eta squared 
eta2 <- eta_squared(anova_int, partial = TRUE)
print(eta2)

# Use effect_size::cohens_d for d_rm (for repeated measures)
# First compute the mean difference and pooled SD
df_wide <- df %>% 
  select( -accuracy) %>%
  pivot_wider(names_from = video_set, values_from = intensity) %>%
  drop_na()%>%
  column_to_rownames("subject")

d_rm <- cohens_d(df_wide$ADFES, df_wide$JeFEE, paired = TRUE, within = TRUE)
print(d_rm)

# Use BayesFactor::ttestBF Bayesian Repeated Measures ANOVA ----
# Now assign subject as rownames for BayesFactor
bf_data <- df_wide 

# Run Bayesian paired t-test
bf <- ttestBF(x = bf_data$ADFES, y = bf_data$JeFEE, paired = TRUE)
print(bf)

#################################################
# 
# END
#
######################################## analysis