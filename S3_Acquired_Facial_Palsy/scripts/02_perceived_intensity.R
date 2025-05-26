###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Perceived intensity measure of the Palsycontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Perceived intensity.
#
#  Experiment   palsycontrol
#
#  Update:      23/05/2023
###########################################################################
rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))

# Calculate mean intensity
intensity_mean <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity","correct"))

unique(intensity_mean$subject)

# Calculate intensity mean for neutral dataset
dat_neutral <- dataset_neutral %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion, Pt.group, Resp.intensity) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion", "group", "intensity")) %>%
  mutate(correct = 1)

data <- rbind(intensity_mean, dat_neutral) %>%
  group_by(subject,video_set ,group , emotion) %>%
  summarise(intensity = mean(intensity,na.rm=TRUE))
write_csv(data, "study3_intensity_behavioral_data.csv")
write.xlsx(data, "objects/congenital_intensity.xlsx")

df <- rbind(intensity_mean, dat_neutral) %>%
  group_by(subject,video_set ,group ) %>%
  summarise(intensity = mean(intensity,na.rm=TRUE))
write.xlsx(df, "objects/congenital_intensity_summary.xlsx")

# Generate a table summarizing the intensities
table_intensity <- data%>%
  group_by(group,emotion, video_set) %>% 
  summarise(intensity = mean(intensity, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = intensity) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")


# Generate aplot for emotions intensities
plot_emotion<- flexplot(intensity~emotion+group |  video_set,
                        spread = "stdev", # sterr, stdev, 
                        data = intensity_mean,
                        alpha = 0.07) +
  theme(legend.position = "bottom") +
  ylab("Perceived intensity (mean & Sd in px)") +
  xlab("")

# Convert to factors
df$video_set <- as.factor(df$video_set)

# ---- 1. Repeated Measures ANOVA (video_set as factor) ----

# Use afex::aov_ez for repeated measures
anova_res <- aov_ez(id = "subject",
                    dv = "intensity",
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
ADFES_ph <- t.test(intensity ~ group, data = adfes_data, var.equal = FALSE)
JeFEE_ph <- t.test(intensity ~ group, data = jefee_data, var.equal = FALSE)

# Compute Cohen's d from raw data
ADFES_d <- cohens_d(intensity ~ group, data = adfes_data)
JeFEE_d <- cohens_d(intensity ~ group, data = jefee_data)

# ---- 3. Bayesian Repeated Measures ANOVA ----
# Calcolo Bayes factor per ADFES e JeFEE
bf_adfes <- ttestBF(
  x = adfes_data$intensity[adfes_data$group == "palsy"],
  y = adfes_data$intensity[adfes_data$group == "control"],
  rscale = "medium"
)

bf_jeffe <- ttestBF(
  x = jefee_data$intensity[jefee_data$group == "palsy"],
  y = jefee_data$intensity[jefee_data$group == "control"],
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
############################# perceived intensity