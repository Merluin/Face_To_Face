###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Accuracy measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Accuracy.
#
#  Experiment   MBScontrol
#
#  Update:      11/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
#replace_csv("group", "control")

devtools::load_all()


# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))

# Calculate mean intensity
correct_data <-  dataset_gw1 %>%
  dplyr::select(Pt.code, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"match","video_set", "emotion", "group", "correct"))

accuracy<-correct_data%>%
  group_by(subject,match,group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks

correct_data_neu <-  dataset_neutral %>%
  dplyr::select(Pt.code, Pt.match, Video.set, Video.emotion, Pt.group, Resp.correct) %>%
  'colnames<-'(c("subject" ,"match","video_set", "emotion", "group", "correct"))

accuracy_neu<-correct_data_neu%>%
  group_by(subject,match,group, emotion, video_set) %>%
  summarise(correct = sum(correct),
            acc = correct/8) # 8 = 4 video id * 2 blocks

write.xlsx(accuracy%>% select(-correct), "objects/congenital_accuracy.xlsx")

df <- accuracy%>%
  group_by( video_set,group,subject) %>%
  summarise(acc = mean(acc,na.rm=TRUE))

assessments <- dataset_gw1 %>%
  select(Pt.code, Asmt.sunnybrook, Asmt.omft) %>%
  distinct() %>%
  rename(subject = Pt.code)

df <- df %>%
  left_join(assessments, by = "subject")

write.xlsx(df, "objects/congenital_accuracy_summary.xlsx")


# Plot accuracy GWE 1 vs GWE 2 ----------------------------------------------
dat_summ <- dataset_full %>%
  filter( Video.emotion != "neutrality" , Wheel.task == "task") %>% # no match
  drop_na(Video.emotion)%>%
  mutate(count = 1)%>%
  group_by(Pt.code,Video.emotion,Video.set,Wheel.name, Resp.category,Pt.group) %>%
  summarise(n = sum(count))%>%
  group_by(Video.emotion,Video.set,Wheel.name, Resp.category,Pt.group) %>%
  summarise(n = mean(n))

plot_freq_gw1 <- accuracy %>% 
  mutate(video_set = stringr::str_to_title(video_set))%>%
  ggplot(aes(x = emotion, y = acc, fill = group)) +
  geom_col(position = position_dodge()) +
  facet_grid(emotion~video_set) +
  cowplot::theme_minimal_hgrid() +
  theme_paper(font_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
                                   face = ifelse(levels(dataset_full$Resp.category) %in% unique(dat_summ$Video.emotion),
                                                 "bold", "plain"),
                                   size = ifelse(levels(dataset_full$Resp.category) %in% unique(dat_summ$Video.emotion),
                                                 10, 8)),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank()) +
  labs(fill = "group")

plot_freq_gw1 <- dat_summ %>% 
  filter(Wheel.name == "GW1" )%>%
  mutate(Video.set = stringr::str_to_title(Video.set))%>%
  plotaccuracy()

plot_freq_gw2 <- dat_summ %>% 
  filter(Wheel.name == "GW2" )%>%
  mutate(Video.set = stringr::str_to_title(Video.set))%>%
  plotaccuracy()


# Accuracy GEW ------------------------------------------------------------
table_accuracy <- accuracy %>% 
  group_by(group,emotion, video_set) %>% 
  summarise(acc = mean(acc, na.rm = TRUE)) %>% 
  pivot_wider(names_from = emotion, values_from = acc) %>% 
  flextable() %>% 
  colformat_double(digits = 2) %>% 
  autofit() %>% 
  merge_v(j = 1:2) %>% 
  theme_vanilla() %>% 
  align(align = "center")


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

# ---- 2. Bayesian based on BIC ----

# 1. Expression type effect on accuracy (within-subjects)
model_null_expr <- lm(acc ~ subject, data = df)
model_alt_expr <- lm(acc ~ subject + video_set, data = df)
BF_expr <- exp((BIC(model_null_expr) - BIC(model_alt_expr)) / 2)

# 2. Group differences in ADFES accuracy
adfes_data <- df %>% filter(video_set == "ADFES")
model_null_adfes_acc <- lm(acc ~ 1, data = adfes_data)
model_alt_adfes_acc  <- lm(acc ~ group, data = adfes_data)
BF_group_acc_adfes <- exp((BIC(model_null_adfes_acc) - BIC(model_alt_adfes_acc)) / 2)
BF01_group_acc_adfes <- 1 / BF_group_acc_adfes

# ---- Print Results ----
cat("BF₁₀ for expression type (video_set) effect on accuracy: ", signif(BF_expr, 3), "\n")
cat("BF₀₁ for group effect on ADFES accuracy: ", signif(BF01_group_acc_adfes, 3), "\n")


# --- Normative values (from your data) ---
norm_ADFES <- 0.821
sd_ADFES   <- 0.093
norm_JeFEE <- 0.551
sd_JeFEE   <- 0.138

# --- Subset your data ---
controls   <- df %>% filter(group == "control")
moebius    <- df %>% filter(group == "moebius")

# --- Compute per-subject means for each video_set ---
control_means <- controls %>%
  group_by(subject, video_set) %>%
  summarise(acc = mean(acc), .groups = "drop")

moebius_means <- moebius %>%
  group_by(subject, video_set) %>%
  summarise(acc = mean(acc), .groups = "drop")

# --- Split by ADFES / JeFEE ---
control_adfes <- control_means %>% filter(video_set == "ADFES") %>% pull(acc)
control_jefee <- control_means %>% filter(video_set == "JeFEE") %>% pull(acc)

moebius_adfes <- moebius_means %>% filter(video_set == "ADFES") %>% pull(acc)
moebius_jefee <- moebius_means %>% filter(video_set == "JeFEE") %>% pull(acc)

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
  summarise(mean_acc = mean(acc),
            sd_acc = sd(acc),
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
             aes(x = group, y = acc, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc),
             color = "black",
             size = 2.5, shape = 18, inherit.aes = FALSE) +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Mean Accuracy", fill = "Expression Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") 



ggsave("files/recognition_accuracy_plot.jpg", 
       plot = p1,     
       device = "jpeg", 
       dpi = 600,
       width = 18, height = 18, units = "cm")


#################################################
# 
# END
#
######################################## accuracy