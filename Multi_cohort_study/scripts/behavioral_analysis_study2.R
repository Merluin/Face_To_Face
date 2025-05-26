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
library(boot)
library(glmnet)
library(mediation)
library(dplyr)

# Data --------------------------------------------------------------------
df_acc <- read_csv("data/study2_accuracy_behavioral_data.csv") %>%
  group_by(subject,video_set,group) %>%
  summarise( accuracy = mean(accuracy, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set,group))

df_int <- read_csv("data/study2_intensity_behavioral_data.csv")%>%
  group_by(subject,video_set, group) %>%
  summarise(intensity = mean(intensity, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set,group)) %>%
  ungroup() %>%
  dplyr::select(-c(subject,video_set,group))

df <- left_join(df_acc, df_int, by = "label") %>%
  dplyr::select(-label)

# ---- 0. Descriptive statistics

descritive <- df %>%
  group_by(video_set,group) %>%
  summarise(mean_acc = mean(accuracy),
            sd_acc = sd(accuracy),
            se_acc = sd(accuracy)/sqrt(n()),
            mean_int = mean(intensity),
            sd_int = sd(intensity),
            se_int = sd(intensity)/sqrt(n()),
            .groups = "drop")
print(descritive)

percentili_90 <- df %>%
  group_by(video_set,group, subject) %>%
  summarise(mean_acc = mean(accuracy),
            mean_int = mean(intensity),
            .groups = "drop") %>%
  group_by(video_set, group) %>%
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
                    between = "group",
                    data = df)
summary(anova_acc)

# Use effect_size::eta_squared for Partial eta squared 
eta2 <- eta_squared(anova_acc, partial = TRUE)
print(eta2)

# Post-hoc Welch's correction (variance == False -> apply correction)
# Subset data first
adfes_data <- df %>% filter(video_set == "ADFES")
jefee_data <- df %>% filter(video_set == "JeFEE")

# Run Welch t-tests
ADFES_ph <- t.test(accuracy ~ group, data = adfes_data, var.equal = FALSE)
JeFEE_ph <- t.test(accuracy ~ group, data = jefee_data, var.equal = FALSE)

# Compute Cohen's d from raw data
ADFES_d <- cohens_d(accuracy ~ group, data = adfes_data)
JeFEE_d <- cohens_d(accuracy ~ group, data = jefee_data)

# Assicurati che i fattori siano specificati come tali
df$group <- as.factor(df$group)
df$video_set <- as.factor(df$video_set)
df$subject <- as.factor(df$subject)

# Bayes ANOVA con soggetti ripetuti
bf_anova <- anovaBF(accuracy ~ video_set * subject, 
                    data = df, 
                    whichRandom = "subject", 
                    iterations = 10000) 

bf_anova <- anovaBF(intensity ~ video_set * group, 
                    data = df, 
                    iterations = 10000) 

# BF
acc_wide <- df %>%  
  select(-intensity) %>%
  pivot_wider(names_from = video_set, values_from = accuracy) 

bf_videoset <- ttestBF(x = acc_wide$ADFES, y = acc_wide$JeFEE,  paired = TRUE)

gr_wide <- df %>%  
  mutate(match = parse_number(as.character(subject))) %>%
  group_by(group,match)%>%
  summarise(accuracy = mean(accuracy))%>%
  pivot_wider(names_from = group, values_from = accuracy) 

bf_videoset <- ttestBF(x = gr_wide$Control, y = gr_wide$Moebius,  paired = FALSE)
1/0.3450541


# --- Normative values (from your data) ---
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

df_norm <- left_join(df_acc, df_int, by = "label") %>%
  select(-label) %>%
  group_by(video_set) %>%
  summarise(norm_acc = mean(accuracy),
           norm_int = mean(intensity),
           sd_acc = sd(accuracy),
           sd_int = sd(intensity))
norm_ADFES <- df_norm$norm_acc[df_norm$video_set == "ADFES"]
norm_JeFEE <- df_norm$norm_acc[df_norm$video_set == "JeFEE"]

# --- Subset your data ---
controls   <- df %>% filter(group == "Control")
moebius    <- df %>% filter(group == "Moebius")

# --- Compute per-subject means for each video_set ---
control_means <- controls %>%
  group_by(subject, video_set) %>%
  summarise(acc = mean(accuracy), 
            int = mean(intensity),
            .groups = "drop")

moebius_means <- moebius %>%
  group_by(subject, video_set) %>%
  summarise(acc = mean(accuracy), 
            int = mean(intensity),
            .groups = "drop")

# --- Split by ADFES / JeFEE ---
control_adfes <- control_means %>% filter(video_set == "ADFES") %>% pull(acc)
control_jefee <- control_means %>% filter(video_set == "JeFEE") %>% pull(acc)

moebius_adfes <- moebius_means %>% filter(video_set == "ADFES") %>% pull(acc)
moebius_jefee <- moebius_means %>% filter(video_set == "JeFEE") %>% pull(acc)

# --- 1. Welch one-sample t-tests vs normative means ---
# ADFES


t_ctrl_adfes <- t.test(control_adfes, mu = norm_ADFES)
d_ctrl_adfes <- cohens_d(control_adfes, mu = norm_ADFES)
bf_ctrl_adfes <- ttestBF(x = control_adfes, mu = norm_ADFES)

t_moeb_adfes <- t.test(moebius_adfes, mu = norm_ADFES)
d_moeb_adfes <- cohens_d(moebius_adfes, mu = norm_ADFES)
bf_moeb_adfes <- ttestBF(x = moebius_adfes, mu = norm_ADFES)

# JeFEE
t_ctrl_jefee <- t.test(control_jefee, mu = norm_JeFEE)
d_ctrl_jefee <- cohens_d(control_jefee, mu = norm_JeFEE)
bf_ctrl_jefee <- ttestBF(x = control_jefee, mu = norm_JeFEE)

t_moeb_jefee <- t.test(moebius_jefee, mu = norm_JeFEE)
d_moeb_jefee <- cohens_d(moebius_jefee, mu = norm_JeFEE)
bf_moeb_jefee <- ttestBF(x = moebius_jefee, mu = norm_JeFEE)

# --- 4. Between-group comparison of effect sizes (optional z-test) ---
# Sample sizes
n_ctrl_adfes <- length(control_adfes)
n_moeb_adfes <- length(moebius_adfes)

# Cohen's d values
d1 <- d_ctrl_adfes$Cohens_d
d2 <- d_moeb_adfes$Cohens_d

# Compute SEs manually
se1 <- sqrt(1/n_ctrl_adfes + (d1^2)/(2 * n_ctrl_adfes))
se2 <- sqrt(1/n_moeb_adfes + (d2^2)/(2 * n_moeb_adfes))

# Z-test for difference in d
z_adfes <- (d1 - d2) / sqrt(se1^2 + se2^2)
p_z_adfes <- 2 * pnorm(-abs(z_adfes))

# Repeat for JeFEE
n_ctrl_jefee <- length(control_jefee)
n_moeb_jefee <- length(moebius_jefee)

d3 <- d_ctrl_jefee$Cohens_d
d4 <- d_moeb_jefee$Cohens_d

se3 <- sqrt(1/n_ctrl_jefee + (d3^2)/(2 * n_ctrl_jefee))
se4 <- sqrt(1/n_moeb_jefee + (d4^2)/(2 * n_moeb_jefee))

z_jefee <- (d3 - d4) / sqrt(se3^2 + se4^2)
p_z_jefee <- 2 * pnorm(-abs(z_jefee))

cat("\nZ-tests comparing Cohen's d between groups (Moebius vs Controls):\n")
cat("ADFES: z =", round(z_adfes, 2), ", p =", round(p_z_adfes, 3), "\n")
cat("JeFEE: z =", round(z_jefee, 2), ", p =", round(p_z_jefee, 3), "\n")







#---------------------------------------------
# Use afex::aov_ez for Recognition intensity
anova_int <- aov_ez(id = "subject",
                    dv = "intensity",
                    within = "video_set",
                    between = "group",
                    data = df)
summary(anova_int)

# Use effect_size::eta_squared for Partial eta squared 
eta2 <- eta_squared(anova_int, partial = TRUE)
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


# ---- 2. Bayesian ----
bf_anova <- anovaBF(intensity ~ video_set * subject, 
                    data = df, 
                    whichRandom = "subject", 
                    iterations = 10000)

# --- Split by ADFES / JeFEE ---
norm_ADFES <- df_norm$norm_int[df_norm$video_set == "ADFES"]
norm_JeFEE <- df_norm$norm_int[df_norm$video_set == "JeFEE"]

control_adfes <- control_means %>% filter(video_set == "ADFES") %>% pull(int)
control_jefee <- control_means %>% filter(video_set == "JeFEE") %>% pull(int)

moebius_adfes <- moebius_means %>% filter(video_set == "ADFES") %>% pull(int)
moebius_jefee <- moebius_means %>% filter(video_set == "JeFEE") %>% pull(int)

# --- 1. Welch one-sample t-tests vs normative means ---
# ADFES


t_ctrl_adfes <- t.test(control_adfes, mu = norm_ADFES)
d_ctrl_adfes <- cohens_d(control_adfes, mu = norm_ADFES)
bf_ctrl_adfes <- ttestBF(x = control_adfes, mu = norm_ADFES)

t_moeb_adfes <- t.test(moebius_adfes, mu = norm_ADFES)
d_moeb_adfes <- cohens_d(moebius_adfes, mu = norm_ADFES)
bf_moeb_adfes <- ttestBF(x = moebius_adfes, mu = norm_ADFES)

# JeFEE
t_ctrl_jefee <- t.test(control_jefee, mu = norm_JeFEE)
d_ctrl_jefee <- cohens_d(control_jefee, mu = norm_JeFEE)
bf_ctrl_jefee <- ttestBF(x = control_jefee, mu = norm_JeFEE)

t_moeb_jefee <- t.test(moebius_jefee, mu = norm_JeFEE)
d_moeb_jefee <- cohens_d(moebius_jefee, mu = norm_JeFEE)
bf_moeb_jefee <- ttestBF(x = moebius_jefee, mu = norm_JeFEE)

# --- 4. Between-group comparison of effect sizes (optional z-test) ---
# Sample sizes
n_ctrl_adfes <- length(control_adfes)
n_moeb_adfes <- length(moebius_adfes)

# Cohen's d values
d1 <- d_ctrl_adfes$Cohens_d
d2 <- d_moeb_adfes$Cohens_d

# Compute SEs manually
se1 <- sqrt(1/n_ctrl_adfes + (d1^2)/(2 * n_ctrl_adfes))
se2 <- sqrt(1/n_moeb_adfes + (d2^2)/(2 * n_moeb_adfes))

# Z-test for difference in d
z_adfes <- (d1 - d2) / sqrt(se1^2 + se2^2)
p_z_adfes <- 2 * pnorm(-abs(z_adfes))

# Repeat for JeFEE
n_ctrl_jefee <- length(control_jefee)
n_moeb_jefee <- length(moebius_jefee)

d3 <- d_ctrl_jefee$Cohens_d
d4 <- d_moeb_jefee$Cohens_d

se3 <- sqrt(1/n_ctrl_jefee + (d3^2)/(2 * n_ctrl_jefee))
se4 <- sqrt(1/n_moeb_jefee + (d4^2)/(2 * n_moeb_jefee))

z_jefee <- (d3 - d4) / sqrt(se3^2 + se4^2)
p_z_jefee <- 2 * pnorm(-abs(z_jefee))

cat("\nZ-tests comparing Cohen's d between groups (Moebius vs Controls):\n")
cat("ADFES: z =", round(z_adfes, 2), ", p =", round(p_z_adfes, 3), "\n")
cat("JeFEE: z =", round(z_jefee, 2), ", p =", round(p_z_jefee, 3), "\n")

# ElasticNet
moebius <- read_csv("data/assesment_mbs.csv")%>% 
  filter(group == "moebius") %>%
  mutate(acc_ADFES = acc_ADFES*100,
         acc_JeFEE = acc_JeFEE*100)

# 4. Prepare variables for Elastic Net
# Sunnybrook = SFGS score (predictor)
# acc_ADFES = ADFES accuracy (outcome 1)
# acc_JeFEE = JeFEE accuracy (outcome 2)
# Remove any rows with missing values in these columns
moebius_clean <- na.omit(moebius[, c("Sunnybrook", "acc_ADFES", "acc_JeFEE")])
X <- as.matrix(moebius_clean$Sunnybrook)
X <- cbind(Sunnybrook = X[, 1], dummy = 0)
y_adfes <- moebius_clean$acc_ADFES
y_jefee <- moebius_clean$acc_JeFEE
# 5. Run Elastic Net regression for ADFES accuracy
set.seed(123)
cv_adfes <- cv.glmnet(X, y_adfes, alpha = 0.5)
best_lambda_adfes <- cv_adfes$lambda.min
model_adfes <- glmnet(X, y_adfes, alpha = 0.5, lambda = best_lambda_adfes)
# 6. Run Elastic Net regression for JeFEE accuracy
cv_jefee <- cv.glmnet(X, y_jefee, alpha = 0.5)
best_lambda_jefee <- cv_jefee$lambda.min
model_jefee <- glmnet(X, y_jefee, alpha = 0.5, lambda = best_lambda_jefee)
# 7. Extract coefficients and R² for each model
# ADFES
coef_adfes <- coef(model_adfes)[2]
pred_adfes <- predict(model_adfes, X)
r2_adfes <- cor(pred_adfes, y_adfes)^2
# JeFEE
coef_jefee <- coef(model_jefee)[2]
pred_jefee <- predict(model_jefee, X)
r2_jefee <- cor(pred_jefee, y_jefee)^2
cat("ADFES: R² =", round(r2_adfes, 2), ", coefficient =", round(coef_adfes, 4), "\n")
cat("JeFEE: R² =", round(r2_jefee, 2), ", coefficient =", round(coef_jefee, 4), "\n")
# 8. Visualize the results
par(mfrow = c(1,2))
plot(X, y_adfes, main = "ADFES Accuracy vs SFGS", xlab = "Sunnybrook (SFGS)", ylab = "ADFES Accuracy", pch = 19, col = "blue")
lines(sort(X), pred_adfes[order(X)], col = "black", lwd = 2)
plot(X, y_jefee, main = "JeFEE Accuracy vs SFGS", xlab = "Sunnybrook (SFGS)", ylab = "JeFEE Accuracy", pch = 19, col = "red")
lines(sort(X), pred_jefee[order(X)], col = "black", lwd = 2)
par(mfrow = c(1,1))



# Mediation analysis: AQ as mediator
data <- read_csv("data/assesment_mbs.csv") %>%
  filter(group == "moebius") %>%
  dplyr::select(subject, AQ_score, TAS_score, Sunnybrook, acc_ADFES, acc_JeFEE)


# Total effect: Sunnybrook → acc_ADFES
model_adfes <- lm(acc_ADFES ~ Sunnybrook, data = data)
summary(model_adfes)

# Total effect: Sunnybrook → acc_JeFEE
model_jefee <- lm(acc_JeFEE ~ Sunnybrook, data = data)
summary(model_jefee)


# Sunnybrook → TAS
model_tas <- lm(TAS_score ~ Sunnybrook, data = data)
summary(model_tas)

# Sunnybrook → AQ
model_aq <- lm(AQ_score ~ Sunnybrook, data = data)
summary(model_aq)

#Mediation AQ mediation for acc_ADFES
med_model_aq_adfes <- lm(AQ_score ~ Sunnybrook, data = data)
out_model_aq_adfes <- lm(acc_ADFES ~ Sunnybrook + AQ_score, data = data)

mediation_aq_adfes <- mediate(med_model_aq_adfes, out_model_aq_adfes,
                              treat = "Sunnybrook", mediator = "AQ_score",
                              boot = TRUE, sims = 1000)

summary(mediation_aq_adfes) 

#Mediation AQ mediation for acc_JeFEE
med_model_aq_jefee <- lm(AQ_score ~ Sunnybrook, data = data)
out_model_aq_jefee <- lm(acc_JeFEE ~ Sunnybrook + AQ_score, data = data)

mediation_aq_jefee <- mediate(med_model_aq_jefee, out_model_aq_jefee,
                              treat = "Sunnybrook", mediator = "AQ_score",
                              boot = TRUE, sims = 1000)

summary(mediation_aq_jefee)

#Mediation TAS mediation for acc_ADFES
med_model_tas_adfes <- lm(TAS_score ~ Sunnybrook, data = data)
out_model_tas_adfes <- lm(acc_ADFES ~ Sunnybrook + TAS_score, data = data)

mediation_tas_adfes <- mediate(med_model_tas_adfes, out_model_tas_adfes,
                               treat = "Sunnybrook", mediator = "TAS_score",
                               boot = TRUE, sims = 1000)

summary(mediation_tas_adfes)

#Mediation 4. TAS mediation for acc_JeFEE
med_model_tas_jefee <- lm(TAS_score ~ Sunnybrook, data = data)
out_model_tas_jefee <- lm(acc_JeFEE ~ Sunnybrook + TAS_score, data = data)

mediation_tas_jefee <- mediate(med_model_tas_jefee, out_model_tas_jefee,
                               treat = "Sunnybrook", mediator = "TAS_score",
                               boot = TRUE, sims = 1000)

summary(mediation_tas_jefee)


# demographic variables
data <- read_csv("data/assesment_mbs.csv") %>%
  filter(group == "moebius") %>%
  dplyr::select(acc_JeFEE, acc_ADFES,lateralisation, age, y_ed, gender )

#Palsy laterality & ADFES or JeFEE accuracy
x <- data$acc_ADFES[data$lateralisation == "monolateral"]
y <- data$acc_ADFES[data$lateralisation == "bilateral"]
t_lat_adfes <- t.test(x, y, paired = FALSE)
d_lat_adfes <- cohens_d(x, y)
bf_lat_adfes <- 1/extractBF(ttestBF(x = x, y =y))$bf

x <- data$acc_JeFEE[data$lateralisation == "monolateral"]
y <- data$acc_JeFEE[data$lateralisation == "bilateral"]
t_lat_jefee <- t.test(x, y, paired = FALSE)
d_lat_jefee <- cohens_d(x, y)
bf_lat_jefee <- 1/extractBF(ttestBF(x = x, y =y))$bf


#Age & ADFES or JeFEE accuracy
x <- data$acc_ADFES
y <- data$age
t_age_adfes <- cor.test(x, y, , method = "pearson")
bf_age_adfes <- 1/extractBF(correlationBF(x = x, y =y))$bf

x <- data$acc_JeFEE
t_age_jefee <- cor.test(x, y, , method = "pearson")
bf_age_jefee <- 1/extractBF(correlationBF(x = x, y =y))$bf

#education & ADFES or JeFEE accuracy
x <- data$acc_ADFES
y <- data$y_ed
t_edu_adfes <- cor.test(x, y, , method = "pearson")
bf_edu_adfes <- 1/extractBF(correlationBF(x = x, y =y))$bf

x <- data$acc_JeFEE
t_edu_jefee <- cor.test(x, y, , method = "pearson")
bf_edu_jefee <- 1/extractBF(correlationBF(x = x, y =y))$bf

#gende & ADFES or JeFEE accuracy
x <- data$acc_ADFES[data$gender == "m"]
y <- data$acc_ADFES[data$gender == "f"]
t_lat_adfes <- t.test(x, y, paired = FALSE)
d_lat_adfes <- cohens_d(x, y)
bf_lat_adfes <- 1/extractBF(ttestBF(x = x, y =y))$bf

x <- data$acc_JeFEE[data$gender == "m"]
y <- data$acc_JeFEE[data$gender == "f"]
t_lat_jefee <- t.test(x, y, paired = FALSE)
d_lat_jefee <- cohens_d(x, y)
bf_lat_jefee <- 1/extractBF(ttestBF(x = x, y =y))$bf


#################################################
# 
# END
#
######################################## analysis