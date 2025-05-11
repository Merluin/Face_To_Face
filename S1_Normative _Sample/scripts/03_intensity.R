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
#  Experiment   online
#
#  Update:      10/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","online_circular.RData"))

# Calculate mean intensity
intensity_mean <-  dataset_gw1 %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion,  Resp.intensity, Resp.correct) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion",  "intensity","correct"))

# Calculate intensity mean for neutral dataset
dat_neutral <- dataset_neutral %>%
  dplyr::select(Pt.code,  Video.set, Video.emotion,  Resp.intensity) %>%
  'colnames<-'(c("subject" ,"video_set", "emotion",  "intensity"))

# Generate a table summarizing the intensities
table_intensity <- intensity_mean%>%
  group_by(emotion, video_set) %>% 
  summarise(intensit = paste0(
    round(mean(intensity, na.rm = TRUE),0),
    " ± ",
    round(sd(intensity, na.rm = TRUE),0)))%>% 
  pivot_wider(names_from = emotion, values_from = intensit) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")




# Fit  correct for each emotion--------------------------------------------
df<-intensity_mean %>%
  group_by(subject,video_set,emotion) %>%
  summarise( intensity = mean(intensity, na.rm = TRUE))

# Save dataset for Jamovi in CSV format
write.csv(df, "objects/dataset_anova_intensity.csv", row.names = FALSE)

df <- df %>%
  group_by(subject,video_set) %>%
  summarise( intensity = mean(intensity, na.rm = TRUE))
write.csv(df, "objects/online_intensity_summary.csv", row.names = FALSE)

# Convert to factors
df$subject <- as.factor(df$subject)
df$video_set <- as.factor(df$video_set)

# ---- 1. Repeated Measures ANOVA (video_set as factor) ----
# Use afex::aov_ez for repeated measures
anova_res <- aov_ez(id = "subject",
                    dv = "intensity",
                    within = "video_set",
                    data = df)
summary(anova_res)

# Partial eta squared
eta2 <- eta_squared(anova_res, partial = TRUE)
print(eta2)

# Cohen's d_rm (for repeated measures)
# First compute the mean difference and pooled SD
df_wide <- df %>% 
  pivot_wider(names_from = video_set, values_from = intensity) %>%
  drop_na()

d_rm <- cohens_d(df_wide$ADFES, df_wide$JeFEE, paired = TRUE, within = TRUE)
print(d_rm)

# ---- 3. Bayesian Repeated Measures ANOVA ----

# Prepare data for BayesFactor (needs wide format)
# Summarize intensity per subject per video_set (average across emotions)
df_agg <- df 

# Pivot to wide format
df_wide <- df_agg %>%
  pivot_wider(names_from = video_set, values_from = intensity)

# Now assign subject as rownames for BayesFactor
bf_data <- df_wide %>%
  column_to_rownames("subject")

# Run Bayesian paired t-test
bf <- ttestBF(x = bf_data$ADFES, y = bf_data$JeFEE, paired = TRUE)
print(bf)

# ---- 4. Normative intensity Bands ----

# Percentile bands per video_set
descritive <- df %>%
  group_by(video_set) %>%
  summarise(mean_intensity = mean(intensity),
            sd_intensity = sd(intensity),
            se = sd(intensity)/sqrt(n()),
            .groups = "drop")
print(descritive)

percentili_90 <- df %>%
  group_by(video_set, subject) %>%
  summarise(mean_intensity = mean(intensity), .groups = "drop") %>%
  group_by(video_set) %>%
  summarise(p05 = quantile(mean_intensity, 0.05),
            p95 = quantile(mean_intensity, 0.95))
print(percentili_90)

#Plot

# Define colors
colors <- c("ADFES" = "blue", "JeFEE" = "red")  # blue and violet

# Create plot
p1 <- ggplot(df[df$video_set %in% c("ADFES","JeFEE"),], aes(video_set, intensity, fill = video_set)) +
  geom_rain(alpha = .5, rain.side = 'f1x1', id.long.var = "subject") +
  geom_point(data = descritive, aes(x = c(1.1, 1.9), y = mean_intensity), color = "black", size = 2.5, shape = 18) +
  geom_errorbar(data = descritive,
                mapping = aes(x = c(1.1, 1.9), ymin = mean_intensity - se, ymax = mean_intensity + se),
                inherit.aes = FALSE,
                width = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Mean Intensity (px)", fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = descritive, aes(x = video_set, y = 0.05,
                                   label = paste0("M = ", round(mean_intensity, 3), 
                                                  "\nSD = ", round(sd_intensity, 3))),
            color = "black", size = 4)

ggsave("files/recognition_intensity_plot.jpg", 
       plot = p1,     
       device = "jpeg", 
       dpi = 600,
       width = 18, height = 18, units = "cm")
#################################################
# 
# END
#
############################# perceived intensity