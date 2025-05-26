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
df_acc <- read_csv("data/study3_accuracy_behavioral_data.csv") %>%
  group_by(subject,video_set,group) %>%
  summarise( accuracy = mean(acc, na.rm = TRUE)) %>%
  mutate(video_set = as.factor(video_set),
         label = paste0(subject,video_set,group))

df_int <- read_csv("data/study3_intensity_behavioral_data.csv")%>%
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
range(df$intensity)

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

# Run BF
x <- adfes_data$accuracy[adfes_data$group == "palsy"]
y <- adfes_data$accuracy[adfes_data$group == "control"]
ADFES_ph <- ttestBF(x = x, y = y, paired = FALSE)
x <- jefee_data$accuracy[jefee_data$group == "palsy"]
y <- jefee_data$accuracy[jefee_data$group == "control"]
JeFEE_ph <- ttestBF(x = x, y = y, paired = FALSE)


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
  dplyr::select(-c(subject,video_set))

df_norm <- left_join(df_acc, df_int, by = "label") %>%
  dplyr::select(-label) %>%
  group_by(video_set) %>%
  summarise(norm_acc = mean(accuracy),
           norm_int = mean(intensity),
           sd_acc = sd(accuracy),
           sd_int = sd(intensity))
norm_ADFES <- df_norm$norm_acc[df_norm$video_set == "ADFES"]
norm_JeFEE <- df_norm$norm_acc[df_norm$video_set == "JeFEE"]

# --- Subset data ---
controls   <- df %>% filter(group == "Control")
palsy    <- df %>% filter(group == "Palsy")

# --- Compute per-subject means for each video_set ---
control_means <- controls %>%
  group_by(subject, video_set) %>%
  summarise(acc = mean(accuracy), 
            int = mean(intensity),
            .groups = "drop")

palsy_means <- palsy %>%
  group_by(subject, video_set) %>%
  summarise(acc = mean(accuracy), 
            int = mean(intensity),
            .groups = "drop")

# --- Split by ADFES / JeFEE ---
control_adfes <- control_means %>% filter(video_set == "ADFES") %>% pull(acc)
control_jefee <- control_means %>% filter(video_set == "JeFEE") %>% pull(acc)

palsy_adfes <- palsy_means %>% filter(video_set == "ADFES") %>% pull(acc)
palsy_jefee <- palsy_means %>% filter(video_set == "JeFEE") %>% pull(acc)

# --- 1. Welch one-sample t-tests vs normative means ---
# ADFES


t_ctrl_adfes <- t.test(control_adfes, mu = norm_ADFES)
d_ctrl_adfes <- cohens_d(control_adfes, mu = norm_ADFES)
bf_ctrl_adfes <- ttestBF(x = control_adfes, mu = norm_ADFES)

t_pal_adfes <- t.test(palsy_adfes, mu = norm_ADFES)
d_pal_adfes <- cohens_d(palsy_adfes, mu = norm_ADFES)
bf_pal_adfes <- ttestBF(x = palsy_adfes, mu = norm_ADFES)

# JeFEE
t_ctrl_jefee <- t.test(control_jefee, mu = norm_JeFEE)
d_ctrl_jefee <- cohens_d(control_jefee, mu = norm_JeFEE)
bf_ctrl_jefee <- ttestBF(x = control_jefee, mu = norm_JeFEE)

t_pal_jefee <- t.test(palsy_jefee, mu = norm_JeFEE)
d_pal_jefee <- cohens_d(palsy_jefee, mu = norm_JeFEE)
bf_pal_jefee <- ttestBF(x = palsy_jefee, mu = norm_JeFEE)

# --- 4. Between-group comparison of effect sizes (optional z-test) ---
# Sample sizes
n_ctrl_adfes <- length(control_adfes)
n_pal_adfes <- length(palsy_adfes)

# Cohen's d values
d1 <- d_ctrl_adfes$Cohens_d
d2 <- d_pal_adfes$Cohens_d

# Compute SEs manually
se1 <- sqrt(1/n_ctrl_adfes + (d1^2)/(2 * n_ctrl_adfes))
se2 <- sqrt(1/n_pal_adfes + (d2^2)/(2 * n_pal_adfes))

# Z-test for difference in d
z_adfes <- (d1 - d2) / sqrt(se1^2 + se2^2)
p_z_adfes <- 2 * pnorm(-abs(z_adfes))

# Repeat for JeFEE
n_ctrl_jefee <- length(control_jefee)
n_pal_jefee <- length(palsy_jefee)

d3 <- d_ctrl_jefee$Cohens_d
d4 <- d_pal_jefee$Cohens_d

se3 <- sqrt(1/n_ctrl_jefee + (d3^2)/(2 * n_ctrl_jefee))
se4 <- sqrt(1/n_pal_jefee + (d4^2)/(2 * n_pal_jefee))

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

# Run BF
x <- adfes_data$intensity[adfes_data$group == "palsy"]
y <- adfes_data$intensity[adfes_data$group == "control"]
ADFES_ph <- ttestBF(x = x, y = y, paired = FALSE)
x <- jefee_data$intensity[jefee_data$group == "palsy"]
y <- jefee_data$intensity[jefee_data$group == "control"]
JeFEE_ph <- ttestBF(x = x, y = y, paired = FALSE)

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

palsy_adfes <- palsy_means %>% filter(video_set == "ADFES") %>% pull(int)
palsy_jefee <- palsy_means %>% filter(video_set == "JeFEE") %>% pull(int)

# --- 1. Welch one-sample t-tests vs normative means ---
# ADFES


t_ctrl_adfes <- t.test(control_adfes, mu = norm_ADFES)
d_ctrl_adfes <- cohens_d(control_adfes, mu = norm_ADFES)
bf_ctrl_adfes <- ttestBF(x = control_adfes, mu = norm_ADFES)

t_pal_adfes <- t.test(palsy_adfes, mu = norm_ADFES)
d_pal_adfes <- cohens_d(palsy_adfes, mu = norm_ADFES)
bf_pal_adfes <- ttestBF(x = palsy_adfes, mu = norm_ADFES)

# JeFEE
t_ctrl_jefee <- t.test(control_jefee, mu = norm_JeFEE)
d_ctrl_jefee <- cohens_d(control_jefee, mu = norm_JeFEE)
bf_ctrl_jefee <- ttestBF(x = control_jefee, mu = norm_JeFEE)

t_pal_jefee <- t.test(palsy_jefee, mu = norm_JeFEE)
d_pal_jefee <- cohens_d(palsy_jefee, mu = norm_JeFEE)
bf_pal_jefee <- ttestBF(x = palsy_jefee, mu = norm_JeFEE)


# contrast
t_group_jefee <- t.test(control_jefee,palsy_jefee, paired = FALSE)

df_z <- data.frame(
  score = c(control_jefee, palsy_jefee),
  group = factor(c(rep("control", length(control_jefee)),
                   rep("palsy", length(palsy_jefee))))
)

wilcox_coin <- coin::wilcox_test(score ~ group,
                           data = df_z,
                           distribution = approximate(B = 10000))  # or exact()

z_stat <- statistic(wilcox_coin, type = "standardized")
p_val <- pvalue(wilcox_coin)

# --- 4. Between-group comparison of effect sizes (optional z-test) ---
# Sample sizes
n_ctrl_adfes <- length(control_adfes)
n_moeb_adfes <- length(palsy_adfes)

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
n_moeb_jefee <- length(palsy_jefee)

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
palsy <- read_csv("data/assesment_palsy.csv")%>% 
  filter(Group == "Palsy") %>%
  mutate(acc_ADFES = Accuracy_ADFES*100,
         acc_JeFEE = Accuracy_JeFEE*100)

# 4. Prepare variables for Elastic Net
# Sunnybrook = SFGS score (predictor)
# acc_ADFES = ADFES accuracy (outcome 1)
# acc_JeFEE = JeFEE accuracy (outcome 2)
# Remove any rows with missing values in these columns
palsy_clean <- na.omit(palsy[, c("Sunnybrook", "acc_ADFES", "acc_JeFEE")])
X <- as.matrix(palsy_clean$Sunnybrook)
X <- cbind(Sunnybrook = X[, 1], dummy = 0)
y_adfes <- palsy_clean$acc_ADFES
y_jefee <- palsy_clean$acc_JeFEE
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




# Mediation analysis: AQ as mediator
data <- read_csv("data/assesment_palsy.csv")%>% 
  filter(Group == "Palsy") %>%
  dplyr::select(Subject_ID, MADRS_num,  Sunnybrook, Accuracy_ADFES, Accuracy_JeFEE)


# Total effect: Sunnybrook → acc_ADFES
model_adfes <- lm(Accuracy_ADFES ~ Sunnybrook, data = data)
summary(model_adfes)

# Total effect: Sunnybrook → acc_JeFEE
model_jefee <- lm(Accuracy_JeFEE ~ Sunnybrook, data = data)
summary(model_jefee)

# Model for mediator (MADRS as outcome of Sunnybrook) model.m.adfes
model.m.adfes <- lm(MADRS_num ~ Sunnybrook, data = data)
model.y.adfes <- lm(Accuracy_ADFES ~ Sunnybrook + MADRS_num, data = data)
set.seed(123)
med.out.adfes <- mediate(model.m.adfes, model.y.adfes,
                         treat = "Sunnybrook", mediator = "MADRS_num", boot = TRUE)
summary(med.out.adfes)

# Model for mediator (MADRS as outcome of Sunnybrook) model.m.jefee
model.m.jefee <- lm(MADRS_num ~ Sunnybrook, data = data)
model.y.jefee <- lm(Accuracy_JeFEE ~ Sunnybrook + MADRS_num, data = data)
set.seed(123)
med.out.jefee <- mediate(model.m.jefee, model.y.jefee,
                         treat = "Sunnybrook", mediator = "MADRS_num", boot = TRUE)
summary(med.out.jefee)


# demographic variables
data <- read_csv("data/assesment_palsy.csv")%>% 
  filter(Group == "Palsy") %>%
  dplyr::select( Subject_ID, Accuracy_ADFES, Accuracy_JeFEE,Side, Age, Education, Gender , Etiology)%>%
  drop_na() 

#Palsy laterality & ADFES or JeFEE accuracy
x <- data$Accuracy_ADFES[data$Side == "sn"]
y <- data$Accuracy_ADFES[data$Side == "ds"]
t_lat_adfes <- t.test(x, y, paired = FALSE)
d_lat_adfes <- cohens_d(x, y)
bf_lat_adfes <- 1/extractBF(ttestBF(x = x, y =y))$bf

x <- data$Accuracy_JeFEE[data$Side == "sn"]
y <- data$Accuracy_JeFEE[data$Side == "ds"]
t_lat_jefee <- t.test(x, y, paired = FALSE)
d_lat_jefee <- cohens_d(x, y)
bf_lat_jefee <- 1/extractBF(ttestBF(x = x, y =y))$bf


#Age & ADFES or JeFEE accuracy
x <- data$Accuracy_ADFES
y <- data$Age
t_age_adfes <- cor.test(x, y, , method = "pearson")
d_age_adfes <- cohens_d(x, y)
bf_age_adfes <- 1/extractBF(correlationBF(x = x, y =y))$bf

x <- data$Accuracy_JeFEE
t_age_jefee <- cor.test(x, y, , method = "pearson")
d_age_jefee <- cohens_d(x, y)
bf_age_jefee <- 1/extractBF(correlationBF(x = x, y =y))$bf

#education & ADFES or JeFEE accuracy
x <- data$Accuracy_ADFES
y <- data$Education
t_edu_adfes <- cor.test(x, y, , method = "pearson")
d_edu_adfes <- cohens_d(x, y)
bf_edu_adfes <- 1/extractBF(correlationBF(x = x, y =y))$bf

x <- data$Accuracy_JeFEE
t_edu_jefee <- cor.test(x, y, , method = "pearson")
d_edu_jefee <- cohens_d(x, y)
bf_edu_jefee <- 1/extractBF(correlationBF(x = x, y =y))$bf

#gende & ADFES or JeFEE accuracy
x <- data$Accuracy_ADFES[data$Gender == "M"]
y <- data$Accuracy_ADFES[data$Gender == "F"]
t_gen_adfes <- t.test(x, y, paired = FALSE)
d_gen_adfes <- cohens_d(x, y)
bf_gen_adfes <- 1/extractBF(ttestBF(x = x, y =y))$bf

x <- data$Accuracy_JeFEE[data$Gender == "M"]
y <- data$Accuracy_JeFEE[data$Gender == "F"]
t_gen_jefee <- t.test(x, y, paired = FALSE)
d_gen_jefee <- cohens_d(x, y)
bf_gen_jefee <- 1/extractBF(ttestBF(x = x, y =y))$bf



data <- read_csv("data/assesment_palsy.csv")%>% 
  filter(Group == "Palsy") %>%
  dplyr::select(Subject_ID, Accuracy_JeFEE, Accuracy_ADFES,Etiology )
data$Etiology <- as.factor(data$Etiology)
data$Subject_ID <- as.factor(data$Subject_ID)

lm_gen_adfes <- lm(Accuracy_ADFES ~ Etiology, data = data)
summary(lm_gen_adfes)
bf_gen_adfes <- anovaBF(Accuracy_ADFES ~ Etiology, 
            data = data,  
            whichRandom = "Subject_ID", 
            iterations = 10000) 

lm_gen_jefee <- lm(Accuracy_JeFEE ~ Etiology, data = data)
summary(lm_gen_jefee)
bf_gen_jefee <- anovaBF(Accuracy_JeFEE ~ Etiology, 
                        data = data,  
                        whichRandom = "Subject_ID", 
                        iterations = 10000) 

# 4. Prepare variables for Elastic Net
# Sunnybrook = SFGS score (predictor)
# acc_ADFES = ADFES intensity (outcome 1)
# acc_JeFEE = JeFEE intensity (outcome 2)
# Remove any rows with missing values in these columns
palsy <- read_csv("data/assesment_palsy.csv")%>% 
  filter(Group == "Palsy") %>%
  mutate(int_ADFES = Intensity_ADFES,
         int_JeFEE = Intensity_JeFEE)

palsy_clean <- na.omit(palsy[, c("Sunnybrook", "int_ADFES", "int_JeFEE")])
X <- as.matrix(palsy_clean$Sunnybrook)
X <- cbind(Sunnybrook = X[, 1], dummy = 0)
y_adfes <- palsy_clean$int_ADFES
y_jefee <- palsy_clean$int_JeFEE
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

diff <- df %>% filter(video_set == "JeFEE") %>%
  group_by(group) %>%
  summarise(range_min = min(intensity), range_max = max(intensity)) %>%
  mutate(delta = range_max - range_min)


ctrl_mean <- descritive %>% 
  filter(video_set == "JeFEE", group == "control") %>%
  pull(mean_int)

n_palsy <- df %>% filter(video_set == "JeFEE", group == "palsy") %>% nrow()

n_above_ctrl <- df %>%
  filter(video_set == "JeFEE", group == "palsy", intensity >= ctrl_mean) %>%
  nrow()

prop_above_ctrl <- n_above_ctrl / n_palsy * 100
sprintf("%.1f%% of Palsy participants had intensity ≥ Control mean", prop_above_ctrl)

df_med <- df %>% 
  filter(group == "palsy", video_set == "JeFEE") %>%
  drop_na(Sunnybrook, intensity, accuracy)



# mediation 
palsy <- read_csv("data/assesment_palsy.csv")%>% 
  filter(Group == "Palsy") %>%
  mutate(intensity = (Intensity_ADFES +Intensity_JeFEE)/2,
         accuracy = (Accuracy_JeFEE + Accuracy_ADFES)/2)

model.m.jefee <- lm(intensity ~ Sunnybrook, data = palsy)
model.y.jefee <- lm(accuracy ~ Sunnybrook + intensity, data = palsy)
set.seed(123)
med.out.jefee <- mediate(model.m.jefee, model.y.jefee,
                         treat = "Sunnybrook", mediator = "intensity", boot = TRUE)
summary(med.out.jefee)

#################################################
# 
# END
#
######################################## analysis