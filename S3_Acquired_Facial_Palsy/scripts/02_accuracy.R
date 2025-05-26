###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        04/06/2023
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the CPO_palsy_AMIM1 experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Accuracy.
#
#  Experiment CPO_palsy_AMIM1
#
#  Update:      21/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))

# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject, group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            n = n(),
            acc = correct/n) 
write_csv(accuracy, "study3_accuracy_behavioral_data.csv")
write.xlsx(accuracy%>% select(-correct), "objects/Acquired_accuracy.xlsx")

df <- accuracy%>%
  group_by( video_set,group,subject) %>%
  summarise(acc = mean(acc,na.rm=TRUE))

assessments <- dataset_gw1 %>%
  select(Pt.code, Asmt.sunnybrook) %>%
  distinct() %>%
  rename(subject = Pt.code)

df <- df %>%
  left_join(assessments, by = "subject")

write.xlsx(df, "objects/Acquired_accuracy_summary.xlsx")

# Convert to factors
#df$subject <- as.factor(df$subject)
df$video_set <- as.factor(df$video_set)

# ---- 1. Repeated Measures ANOVA (video_set as factor) ----

# Use afex::aov_ez for repeated measures
anova_res <- aov_ez(id = "subject",
                    dv = "acc",
                    within = "video_set",
                    between = "group",
                    data = df)
summary(anova_res)

# Partial eta squared
eta2 <- eta_squared(anova_res, partial = TRUE)
print(eta2)

# Post-hoc Welch's correction (variance == False -> apply correction)
# Subset data first
adfes_data <- df %>% filter(video_set == "ADFES")
jefee_data <- df %>% filter(video_set == "JeFEE")

# Run Welch t-tests
ADFES_ph <- t.test(acc ~ group, data = adfes_data, var.equal = FALSE)
JeFEE_ph <- t.test(acc ~ group, data = jefee_data, var.equal = FALSE)

# Compute Cohen's d from raw data
ADFES_d <- cohens_d(acc ~ group, data = adfes_data)
JeFEE_d <- cohens_d(acc ~ group, data = jefee_data)

# ---- 3. Bayesian Repeated Measures ANOVA ----
# Calcolo Bayes factor per ADFES e JeFEE
bf_adfes <- ttestBF(
  x = adfes_data$acc[adfes_data$group == "palsy"],
  y = adfes_data$acc[adfes_data$group == "control"],
  rscale = "medium"
)

bf_jeffe <- ttestBF(
  x = jefee_data$acc[jefee_data$group == "palsy"],
  y = jefee_data$acc[jefee_data$group == "control"],
  rscale = "medium"
)

# Estrai e stampa BF con 3 cifre significative
cat("\nBayes Factors (BF₁₀):\n")
cat("ADFES:\n")
print(signif(extractBF(bf_adfes)[, "bf"], 3))

cat("JeFEE:\n")
print(signif(extractBF(bf_jeffe)[, "bf"], 3))

#################################################
# 
# END
#
######################################## accuracy