###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Perceived intensity measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Perceived intensity.
#
#  Experiment   MBScontrol
#
#  Update:      09/05//2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))

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


# ---- 2. Bayesian ----
# Calcolo Bayes factor per ADFES e JeFEE
bf_adfes <- ttestBF(
  x = adfes_data$intensity[adfes_data$group == "moebius"],
  y = adfes_data$intensity[adfes_data$group == "control"],
  rscale = "medium"
)

bf_jeffe <- ttestBF(
  x = jefee_data$intensity[jefee_data$group == "moebius"],
  y = jefee_data$intensity[jefee_data$group == "control"],
  rscale = "medium"
)

# Estrai e stampa BF con 3 cifre significative
cat("\nBayes Factors (BF₁₀):\n")
cat("ADFES:\n")
print(signif(extractBF(bf_adfes)[, "bf"], 3))

cat("JeFEE:\n")
print(signif(extractBF(bf_jeffe)[, "bf"], 3))



# --- Normative values (from your data) ---
norm_ADFES <- 204.952
sd_ADFES   <- 26.666
norm_JeFEE <- 175.756
sd_JeFEE   <- 29.28

# --- Subset your data ---
controls   <- df %>% filter(group == "control")
moebius    <- df %>% filter(group == "moebius")

# --- Compute per-subject means for each video_set ---
control_means <- controls %>%
  group_by(subject, video_set) %>%
  summarise(int = mean(intensity), .groups = "drop")

moebius_means <- moebius %>%
  group_by(subject, video_set) %>%
  summarise(int = mean(intensity), .groups = "drop")

# --- Split by ADFES / JeFEE ---
control_adfes <- control_means %>% filter(video_set == "ADFES") %>% pull(int)
control_jefee <- control_means %>% filter(video_set == "JeFEE") %>% pull(int)

moebius_adfes <- moebius_means %>% filter(video_set == "ADFES") %>% pull(int)
moebius_jefee <- moebius_means %>% filter(video_set == "JeFEE") %>% pull(int)

# --- 1. Welch one-sample t-tests vs normative means ---
# ADFES
t_ctrl_adfes <- t.test(control_adfes, mu = norm_ADFES)
t_moeb_adfes <- t.test(moebius_adfes, mu = norm_ADFES)

# JeFEE
t_ctrl_jefee <- t.test(control_jefee, mu = norm_JeFEE)
t_moeb_jefee <- t.test(moebius_jefee, mu = norm_JeFEE)

# --- 2. Cohen’s d (one-sample) ---
d_ctrl_adfes <- cohens_d(control_adfes, mu = norm_ADFES)
d_moeb_adfes <- cohens_d(moebius_adfes, mu = norm_ADFES)

d_ctrl_jefee <- cohens_d(control_jefee, mu = norm_JeFEE)
d_moeb_jefee <- cohens_d(moebius_jefee, mu = norm_JeFEE)

# --- 3. Bayes Factors (one-sample, default Cauchy prior r = 0.707) ---
bf_ctrl_adfes <- ttestBF(x = control_adfes, mu = norm_ADFES)
bf_moeb_adfes <- ttestBF(x = moebius_adfes, mu = norm_ADFES)

bf_ctrl_jefee <- ttestBF(x = control_jefee, mu = norm_JeFEE)
bf_moeb_jefee <- ttestBF(x = moebius_jefee, mu = norm_JeFEE)

# --- Output summary ---
cat("ADFES - Controls:\n")
print(t_ctrl_adfes)
print(d_ctrl_adfes)
print(bf_ctrl_adfes)

cat("\nADFES - Moebius:\n")
print(t_moeb_adfes)
print(d_moeb_adfes)
print(bf_moeb_adfes)

cat("\nJeFEE - Controls:\n")
print(t_ctrl_jefee)
print(d_ctrl_jefee)
print(bf_ctrl_jefee)

cat("\nJeFEE - Moebius:\n")
print(t_moeb_jefee)
print(d_moeb_jefee)
print(bf_moeb_jefee)

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

#Plot
# Ricrea la variabile di match
df <- df %>%
  ungroup() %>%
  mutate(match = paste0(parse_number(as.character(subject)), video_set))

# Calcola medie, sd, se
descritive <- df %>%
  filter(group %in% c("moebius", "control")) %>%
  group_by(video_set, group) %>%
  summarise(mean_acc = mean(intensity),
            sd_acc = sd(intensity),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "moebius" & video_set == "ADFES" ~ 1 + 0.1,
    group == "moebius" & video_set == "JeFEE" ~ 1 + 0.1,
    group == "control" & video_set == "ADFES" ~ 2 - 0.1,
    group == "control" & video_set == "JeFEE" ~ 2 - 0.1
  ))

# Colori per le condizioni
colors <- c("ADFES" = "blue", "JeFEE" = "red")

# Plot finale
p1 <- ggplot(df %>% filter(group %in% c("moebius", "control")),
             aes(x = group, y = intensity, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc),
             color = "black",
             size = 2.5, shape = 18, inherit.aes = FALSE) +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Mean intensity (px)", fill = "Expression Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")



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