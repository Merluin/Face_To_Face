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
#  Update:      18/02/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","palsy_circular.RData"))

library(dplyr)
library(ggplot2)
library(ggpubr)

# Data Wrangling: Preparing the dataset
# We rename columns for better readability and filter only subjects with 'palsy' condition
summary_data <- dataset_gw1 %>%
  select(Pt.code, Video.set, Video.emotion, Pt.group, Resp.intensity, Resp.correct, Asmt.sunnybrook) %>%
  rename(
    subject_id = Pt.code,
    video_category = Video.set,
    video_emotion = Video.emotion,
    patient_group = Pt.group,
    response_intensity = Resp.intensity,
    response_correct = Resp.correct,
    sunnybrook_score = Asmt.sunnybrook
  ) %>%
  filter(patient_group == "palsy") %>%
  group_by(subject_id, video_category) %>%
  summarise(
    avg_intensity = mean(response_intensity, na.rm = TRUE),
    avg_accuracy = mean(response_correct, na.rm = TRUE),
    avg_sunnybrook = mean(sunnybrook_score, na.rm = TRUE)
  )

# Save dataset for Jamovi in CSV format
summary_jamovi <- summary_data %>%
  pivot_wider(
    names_from = video_category,  # Espandere la colonna "video_category"
    values_from = c(avg_intensity, avg_accuracy, avg_sunnybrook),  # Creare colonne separate per ogni variabile
    names_glue = "{.value}_{video_category}")  # Aggiungere suffisso per identificare la categoria
    
  
  
write.csv(summary_jamovi, "objects/dataset_correlation.csv", row.names = FALSE)

# Hypothesis:
# The Sunnybrook score measures the severity of facial palsy: higher scores indicate less paralysis.
# We hypothesize that the more severe the paralysis (lower sunnybrook score), the lower the perceived response intensity.

# Function to compute Pearson correlation (one-tailed test) and create a scatter plot with regression line
compute_and_plot_correlation <- function(data_subset, x_var, y_var, video_category_label) {
  # Perform Pearson correlation test (one-tailed: positive correlation expected)
  correlation_test <- cor.test(data_subset[[x_var]], data_subset[[y_var]], method = "pearson", alternative = "greater")
  
  # Extract correlation coefficient and p-value
  correlation_coefficient <- correlation_test$estimate
  p_value <- correlation_test$p.value
  
  # Generate scatter plot with regression line
  correlation_plot <- ggplot(data_subset, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue") +  # Add data points
    geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line with confidence interval
    annotate("text", x = min(data_subset[[x_var]], na.rm = TRUE), y = max(data_subset[[y_var]], na.rm = TRUE),
             label = paste("r =", round(correlation_coefficient, 3), "p =", round(p_value, 4)),
             hjust = 0, size = 5, color = "black") +
    labs(
      title = paste("Correlation in", video_category_label),
      x = x_var,
      y = y_var
    ) +
    theme_minimal()
  
  # Save plot to figures directory
  ggsave(filename = paste0("figures/correlation_", video_category_label, "_", x_var, "_", y_var, ".png"), 
         plot = correlation_plot, width = 8, height = 6, dpi = 300)
  
  return(list(correlation_coefficient = correlation_coefficient, p_value = p_value, plot = correlation_plot))
}

# Filter data for each video category
subset_adfes <- summary_data %>% filter(video_category == "ADFES")
subset_jefee <- summary_data %>% filter(video_category == "JeFEE")

# Compute correlations and generate plots
# Correlation between response intensity and sunnybrook score
correlation_intensity_adfes <- compute_and_plot_correlation(subset_adfes, "avg_intensity", "avg_sunnybrook", "ADFES")
correlation_intensity_jefee <- compute_and_plot_correlation(subset_jefee, "avg_intensity", "avg_sunnybrook", "JeFEE")

# Correlation between response accuracy and sunnybrook score
correlation_accuracy_adfes <- compute_and_plot_correlation(subset_adfes, "avg_accuracy", "avg_sunnybrook", "ADFES")
correlation_accuracy_jefee <- compute_and_plot_correlation(subset_jefee, "avg_accuracy", "avg_sunnybrook", "JeFEE")

# Arrange all plots into a single figure
final_plot <- ggarrange(
  correlation_intensity_adfes$plot, correlation_accuracy_adfes$plot, 
  correlation_intensity_jefee$plot, correlation_accuracy_jefee$plot, 
  ncol = 2, nrow = 2
)

# Save final arranged plot
ggsave("figures/final_correlation_plots.png", plot = final_plot, width = 12, height = 8, dpi = 300, bg = "white")


#################################################
# 
# END
#
######################################## correlation
  