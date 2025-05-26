# R script for accuracy and intensity analysis in facial expression recognition  
# Required libraries  
library(tidyverse)  # For data manipulation  
library(effectsize) # For effect size calculations  
library(rstatix)    # For additional statistics  
library(ggplot2)    # For visualizations  

###################  
# UTILITY FUNCTIONS  
###################  

# Function to calculate partial eta squared from ANOVA  
calculate_partial_eta_squared <- function(model) {  
  aov_table <- summary(model)[[1]]  
  F_value <- aov_table$`F value`[1]  
  df1 <- aov_table$Df[1]  
  df2 <- aov_table$Df[2]  
  partial_eta_squared <- (F_value * df1) / (F_value * df1 + df2)  
  return(partial_eta_squared)  
}  

# Function to calculate Cohen's d for repeated measures  
calculate_cohens_d_rm <- function(x1, x2) {  
  diff <- x1 - x2  
  d_rm <- mean(diff) / sd(diff)  
  return(d_rm)  
}  

# Function for complete descriptive statistics  
get_descriptive_stats <- function(data) {  
  list(  
    n = length(data),  
    mean = mean(data),  
    sd = sd(data),  
    percentile_5 = quantile(data, 0.05),  
    percentile_95 = quantile(data, 0.95)  
  )  
}  

###################  
# ACCURACY ANALYSIS  
###################  

# Load accuracy data  
df_acc <- df

# Calculate means by subject  
acc_by_subject <- df_acc %>%  
  group_by(subject, video_set) %>%  
  summarise(  
    mean_acc = mean(acc),  
    .groups = 'drop'  
  ) %>%  
  pivot_wider(  
    names_from = video_set,  
    values_from = mean_acc  
  )  

# Descriptive statistics for ADFES  
adfes_stats <- get_descriptive_stats(acc_by_subject$ADFES)  
cat("\nADFES Accuracy Statistics:\n")  
cat("N =", adfes_stats$n, "\n")  
cat("Mean =", round(adfes_stats$mean, 3), "\n")  
cat("SD =", round(adfes_stats$sd, 3), "\n")  
cat("5th percentile =", round(adfes_stats$percentile_5, 3), "\n")  
cat("95th percentile =", round(adfes_stats$percentile_95, 3), "\n")  

# Descriptive statistics for JeFEE  
jeffee_stats <- get_descriptive_stats(acc_by_subject$JeFEE)  
cat("\nJeFEE Accuracy Statistics:\n")  
cat("N =", jeffee_stats$n, "\n")  
cat("Mean =", round(jeffee_stats$mean, 3), "\n")  
cat("SD =", round(jeffee_stats$sd, 3), "\n")  
cat("5th percentile =", round(jeffee_stats$percentile_5, 3), "\n")  
cat("95th percentile =", round(jeffee_stats$percentile_95, 3), "\n")  

# Repeated measures ANOVA  
acc_aov <- aov(acc ~ video_set + Error(subject/video_set), data = df_acc)  
acc_summary <- summary(acc_aov)  

# Effect sizes for accuracy  
acc_eta_p2 <- calculate_partial_eta_squared(acc_aov)  
acc_d_rm <- calculate_cohens_d_rm(acc_by_subject$ADFES, acc_by_subject$JeFEE)  

# Print statistical results  
cat("\nAccuracy Statistical Tests:\n")  
print(acc_summary)  
cat("Partial eta squared =", round(acc_eta_p2, 3), "\n")  
cat("Cohen's d_rm =", round(acc_d_rm, 2), "\n")  

# Visualization for accuracy  
p1 <- ggplot(acc_by_subject) +  
  geom_histogram(aes(x = ADFES), fill = "blue", alpha = 0.5, bins = 20) +  
  geom_histogram(aes(x = JeFEE), fill = "red", alpha = 0.5, bins = 20) +  
  geom_vline(xintercept = adfes_stats$mean, color = "blue", linetype = "dashed") +  
  geom_vline(xintercept = jeffee_stats$mean, color = "red", linetype = "dashed") +  
  labs(title = "Distribution of Recognition Accuracy",  
       x = "Accuracy",  
       y = "Count") +  
  theme_minimal()  

###################  
# INTENSITY ANALYSIS  
###################  

# Load intensity data  
df_int <- read.csv("dataset_anova_intensity.csv")  

# Calculate means by subject  
int_by_subject <- df_int %>%  
  group_by(subject, video_set) %>%  
  summarise(  
    mean_intensity = mean(intensity),  
    .groups = 'drop'  
  ) %>%  
  pivot_wider(  
    names_from = video_set,  
    values_from = mean_intensity  
  )  

# Descriptive statistics for ADFES intensity  
adfes_int_stats <- get_descriptive_stats(int_by_subject$ADFES)  
cat("\nADFES Intensity Statistics:\n")  
cat("N =", adfes_int_stats$n, "\n")  
cat("Mean =", round(adfes_int_stats$mean, 2), "\n")  
cat("SD =", round(adfes_int_stats$sd, 2), "\n")  
cat("5th percentile =", round(adfes_int_stats$percentile_5, 2), "\n")  
cat("95th percentile =", round(adfes_int_stats$percentile_95, 2), "\n")  

# Descriptive statistics for JeFEE intensity  
jeffee_int_stats <- get_descriptive_stats(int_by_subject$JeFEE)  
cat("\nJeFEE Intensity Statistics:\n")  
cat("N =", jeffee_int_stats$n, "\n")  
cat("Mean =", round(jeffee_int_stats$mean, 2), "\n")  
cat("SD =", round(jeffee_int_stats$sd, 2), "\n")  
cat("5th percentile =", round(jeffee_int_stats$percentile_5, 2), "\n")  
cat("95th percentile =", round(jeffee_int_stats$percentile_95, 2), "\n")  

# Repeated measures ANOVA for intensity  
int_aov <- aov(intensity ~ video_set + Error(subject/video_set), data = df_int)  
int_summary <- summary(int_aov)  

# Effect sizes for intensity  
int_eta_p2 <- calculate_partial_eta_squared(int_aov)  
int_d_rm <- calculate_cohens_d_rm(int_by_subject$ADFES, int_by_subject$JeFEE)  

# Print statistical results  
cat("\nIntensity Statistical Tests:\n")  
print(int_summary)  
cat("Partial eta squared =", round(int_eta_p2, 3), "\n")  
cat("Cohen's d_rm =", round(int_d_rm, 2), "\n")  

# Visualization for intensity  
p2 <- ggplot(int_by_subject) +  
  geom_histogram(aes(x = ADFES), fill = "blue", alpha = 0.5, bins = 20) +  
  geom_histogram(aes(x = JeFEE), fill = "red", alpha = 0.5, bins = 20) +  
  geom_vline(xintercept = adfes_int_stats$mean, color = "blue", linetype = "dashed") +  
  geom_vline(xintercept = jeffee_int_stats$mean, color = "red", linetype = "dashed") +  
  labs(title = "Distribution of Perceived Intensity",  
       x = "Intensity",  
       y = "Count") +  
  theme_minimal()  

# Save results to a text file  
sink("results_summary.txt")  

cat("COMPLETE ANALYSIS RESULTS\n")  
cat("========================\n\n")  

cat("1. ACCURACY\n")  
cat("-----------\n")  
cat("ADFES:\n")  
cat(sprintf("Mean ± SD: %.3f ± %.3f\n", adfes_stats$mean, adfes_stats$sd))  
cat(sprintf("90%% interval (5th-95th percentile): %.3f - %.3f\n\n",   
            adfes_stats$percentile_5, adfes_stats$percentile_95))  

cat("JeFEE:\n")  
cat(sprintf("Mean ± SD: %.3f ± %.3f\n", jeffee_stats$mean, jeffee_stats$sd))  
cat(sprintf("90%% interval (5th-95th percentile): %.3f - %.3f\n\n",  
            jeffee_stats$percentile_5, jeffee_stats$percentile_95))  

cat("Statistical tests:\n")  
cat(sprintf("F(1,%d) = %.2f\n", length(acc_by_subject$ADFES)-1,   
            acc_summary[[2]][[1]]$`F value`[1]))  
cat(sprintf("p = %.2e\n", acc_summary[[2]][[1]]$`Pr(>F)`[1]))  
cat(sprintf("Partial eta squared = %.3f\n", acc_eta_p2))  
cat(sprintf("Cohen's d_rm = %.2f\n\n", acc_d_rm))  

cat("2. INTENSITY\n")  
cat("------------\n")  
cat("ADFES:\n")  
cat(sprintf("Mean ± SD: %.2f ± %.2f\n", adfes_int_stats$mean, adfes_int_stats$sd))  
cat(sprintf("90%% interval (5th-95th percentile): %.2f - %.2f\n\n",  
            adfes_int_stats$percentile_5, adfes_int_stats$percentile_95))  

cat("JeFEE:\n")  
cat(sprintf("Mean ± SD: %.2f ± %.2f\n", jeffee_int_stats$mean, jeffee_int_stats$sd))  
cat(sprintf("90%% interval (5th-95th percentile): %.2f - %.2f\n\n",  
            jeffee_int_stats$percentile_5, jeffee_int_stats$percentile_95))  

cat("Statistical tests:\n")  
cat(sprintf("F(1,%d) = %.2f\n", length(int_by_subject$ADFES)-1,  
            int_summary[[2]][[1]]$`F value`[1]))  
cat(sprintf("p = %.2e\n", int_summary[[2]][[1]]$`Pr(>F)`[1]))  
cat(sprintf("Partial eta squared = %.3f\n", int_eta_p2))  
cat(sprintf("Cohen's d_rm = %.2f\n", int_d_rm))  

sink()  

# Save plots  
ggsave("accuracy_distribution.png", p1, width = 8, height = 6)  
ggsave("intensity_distribution.png", p2, width = 8, height = 6)  
Collapse












