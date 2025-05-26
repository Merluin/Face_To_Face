###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs plots from data analysis for the CARIPARO experiment.
#
#  Experiment   Onlinecontrol
#
#  Update:      23/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggrain)
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

#plots figure 2
df <- df %>%
  ungroup() %>%
  mutate(match = paste0(parse_number(as.character(subject)), video_set))

descritive <- df %>%
  filter(group %in% c("Moebius", "Control")) %>%
  group_by(video_set, group) %>%
  summarise(mean_acc = mean(accuracy),
            sd_acc = sd(accuracy),
            se_acc = sd_acc / sqrt(n()),
            mean_int = mean(intensity),
            sd_int = sd(intensity),
            se_int = sd_int / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "Moebius" & video_set == "ADFES" ~ 1 + 0.1,
    group == "Moebius" & video_set == "JeFEE" ~ 1 + 0.1,
    group == "Control" & video_set == "ADFES" ~ 2 - 0.1,
    group == "Control" & video_set == "JeFEE" ~ 2 - 0.1
  ))

# Colori per le condizioni
colors <- c("ADFES" = "#657d9a", "JeFEE" = "#b37269")
colorgroup <- c(Moebius = "#F0E442", Control = "#C07DA5")

# Plot finale
p1 <- ggplot(df %>% filter(group %in% c("Moebius", "Control")),
             aes(x = group, y = accuracy, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match", cov = "group") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc, fill = video_set),
             size = 3, shape = 23, color = "black") +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colorgroup) +
  labs(x = NULL, y = "Mean Accuracy (%)", fill = "Expression Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") + 
  theme(text=element_text(size=10,  family="Helvetica"),
                                          panel.background = element_blank(),
                                          axis.line = element_line(colour = "black"),
                                          strip.text.x = element_text(size = 6.6),
                                          strip.text.y = element_text(size = 6.6),
                                          panel.grid.major = element_blank(),  # Rimuove le griglie principali
                                          panel.grid.minor = element_blank() )

p2 <- ggplot(df %>% filter(group %in% c("Moebius", "Control")),
             aes(x = group, y = intensity, fill = video_set)) +
  geom_rain(alpha = 0.5, rain.side = 'f2x2', id.long.var = "match", cov = "group") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_int, fill = video_set),
             size = 3, shape = 23, color = "black") +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_int - se_int, ymax = mean_int + se_int),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colorgroup) +
  labs(x = NULL, y = "Mean intensity (px)", fill = "Expression Type") +
  theme_minimal() +
  theme(legend.position = "none")+ theme(text=element_text(size=10,  family="Helvetica"),
                                         panel.background = element_blank(),
                                         axis.line = element_line(colour = "black"),
                                         strip.text.x = element_text(size = 6.6),
                                         strip.text.y = element_text(size = 6.6),
                                         panel.grid.major = element_blank(),  # Rimuove le griglie principali
                                         panel.grid.minor = element_blank() )

p3 <- cowplot::plot_grid(p1, p2, nrow = 1,labels = "AUTO")

ggsave("figures/Fig_8_finale.png",           
       plot = p3,           
       device = "png",         
       dpi = 300,              
       width = 18,              
       height = 9,             
       units = "cm")

ggsave("figures/Fig_8.tiff",           
       plot = p3,           
       device = "tiff",         
       dpi = 1200,              
       width = 18,              
       height = 9,             
       units = "cm")

# plots figure 3
# correlation
  
moebius_data <- read_csv("data/assesment_mbs.csv")%>% 
    filter(group == "moebius") %>%
  mutate(acc_ADFES = acc_ADFES*100,
         acc_JeFEE = acc_JeFEE*100)
  
  # ADFES
  cor_test_adfes <- cor.test(moebius_data$Sunnybrook, moebius_data$acc_ADFES, method = "pearson")
  print(cor_test_adfes)
  bf_test_adfes <- correlationBF(x = moebius_data$Sunnybrook, y = moebius_data$acc_ADFES)
  posterior_samples <- posterior(bf_test_adfes, iterations = 10000)
  p_r_greater_0 <- mean(posterior_samples[, "rho"] > 0)
  sprintf("P(r > 0) = %.3f", p_r_greater_0)
  # JeFEE
  cor_test_jefee <- cor.test(moebius_data$Sunnybrook, moebius_data$acc_JeFEE, method = "pearson")
  print(cor_test_jefee)
  bf_test_jefee <- correlationBF(x = moebius_data$Sunnybrook, y = moebius_data$acc_JeFEE)
  posterior_samples <- posterior(bf_test_jefee, iterations = 10000)
  p_r_greater_0 <- mean(posterior_samples[, "rho"] > 0)
  sprintf("P(r > 0) = %.3f", p_r_greater_0)
  
  p1 <- ggplot(moebius_data, aes(x = Sunnybrook, y = acc_ADFES)) +
    geom_point(color = "#3271AD") +
    geom_smooth(method = "lm", color = "#3271AD", fill = "#3271AD", alpha = 0.3) +
    labs(title = " ", x = "SFGS Score", y = "ADFES Accuracy (x)") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 100))+
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          text = element_text(size = 10, 
                              family = "Helvetica"),             # use Helvetica, size 16 for all text
          panel.background = element_blank(),                    # clean white background
          axis.line = element_line(colour = "black"),            # black axis lines
          strip.text.x = element_text(size = 6.6),               # facet labels (x): size 20
          strip.text.y = element_text(size = 6.6),
          panel.grid.major = element_blank(),                    # Rimuove le griglie principali
          panel.grid.minor = element_blank())
  
  p2 <- ggplot(moebius_data, aes(x = Sunnybrook, y = acc_JeFEE)) +
    geom_point(color = "#C56637") +
    geom_smooth(method = "lm", color = "#C56637", fill = "#C56637", alpha = 0.3) +
    labs(title = "", x = "SFGS Score", y = "JeFEE Accuracy (%)") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 100))+
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          text = element_text(size = 10, 
                              family = "Helvetica"),             # use Helvetica, size 16 for all text
          panel.background = element_blank(),                    # clean white background
          axis.line = element_line(colour = "black"),            # black axis lines
          strip.text.x = element_text(size = 6.6),               # facet labels (x): size 20
          strip.text.y = element_text(size = 6.6),
          panel.grid.major = element_blank(),                    # Rimuove le griglie principali
          panel.grid.minor = element_blank())
  
  
  
  # Bootstrap 
  
  
  set.seed(123)  # for reproducibility
  
  # Generic function to bootstrap Pearson r
  cor_fun <- function(data, indices, x, y) {
    d <- data[indices, ]
    return(cor(d[[x]], d[[y]], method = "pearson"))
  }
  
  
  # Bootstrap ADFES
  boot_adfes <- boot(data = moebius_data, statistic = cor_fun, R = 10000, x = "Sunnybrook", y = "acc_ADFES")
  ci_adfes <- boot.ci(boot_adfes, type = "perc")$percent[4:5]
  
  # Bootstrap JeFEE
  boot_jefee <- boot(data = moebius_data, statistic = cor_fun, R = 10000, x = "Sunnybrook", y = "acc_JeFEE")
  ci_jefee <- boot.ci(boot_jefee, type = "perc")$percent[4:5]

  # Prepare data frame
  df_boot <- data.frame(
    r = c(boot_adfes$t, boot_jefee$t),
    test = rep(c("ADFES", "JeFEE"), each = length(boot_adfes$t))
  )
  
  # Prepare data frame
  df_boot <- data.frame(
    r = c(boot_adfes$t, boot_jefee$t),
    test = rep(c("ADFES", "JeFEE"), each = length(boot_adfes$t))
  )
  
  # Define colors
  color_map <- c("ADFES" = "#3271AD", "JeFEE" = "#C56637")  # blue and violet


p3 <- ggplot(df_boot, aes(x = r, fill = test)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = boot_adfes$t0, color = "#3271AD", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = ci_adfes, color = "#3271AD", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = boot_jefee$t0, color = "#C56637", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = ci_jefee, color = "#C56637", linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values = color_map) +
  labs(
    title = "",
    x = "Bootstrapped Correlation Coefficient (r)",
    y = "Density",
    fill = "Test"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        text = element_text(size = 10, 
                            family = "Helvetica"),             # use Helvetica, size 16 for all text
        panel.background = element_blank(),                    # clean white background
        axis.line = element_line(colour = "black"),            # black axis lines
        strip.text.x = element_text(size = 6.6),               # facet labels (x): size 20
        strip.text.y = element_text(size = 6.6),
        panel.grid.major = element_blank(),                    # Rimuove le griglie principali
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0, y = 3, 
           label = "ADFES statistics:\nMean r = 0.618\n 95% CI: [0.439, 0.834]\nP(r > 0) = 0.987", color = "#3271AD", size = 3) +
  annotate("text", x = 0, y = 1.2, 
           label = "JeFEE statistics:\nMean r = 0.644\n 95% CI: [0.374, 0.836]\nP(r > 0) = 0.972", color = "#C56637", size = 3)

# Plots

p4 <- cowplot::plot_grid(cowplot::plot_grid(p1, p2), p3,  nrow = 2, labels = "AUTO")

# Export
ggsave("figures/Fig_2_finale.png",  # export for draft         
       plot = p4,           
       device = "png",         
       dpi = 300,              
       width = 18,              
       height = 14,             
       units = "cm")

ggsave("figures/Fig_2.tiff",    # export for journal       
       plot = p4,           
       device = "tiff",         
       dpi = 1200,              
       width = 18,              
       height = 14,             
       units = "cm")



# plot figure 4
df <- read_csv("data/assesment_mbs.csv") %>%
  dplyr::select(subject, group, Sunnybrook,ofmt ) %>%
  mutate(match = parse_number(subject))


# Panel A: Scatterplot (no correlation)
pA <- ggplot(df %>% filter(group == "moebius"), aes(x = Sunnybrook, y = ofmt)) +
  geom_point(fill = "#F0E442", color = "gray" , shape = 21 ) +
  geom_smooth(method = "lm", se = TRUE, color = "#D1C436", fill = "#FAF6C2", alpha = 0.3) +
  labs(
    x = "Sunnybrook Score (Facial Motor Function)",
    y = "OFMT Accuracy (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        text = element_text(size = 10, 
                            family = "Helvetica"),             # use Helvetica, size 16 for all text
        panel.background = element_blank(),                    # clean white background
        axis.line = element_line(colour = "black"),            # black axis lines
        strip.text.x = element_text(size = 6.6),               # facet labels (x): size 20
        strip.text.y = element_text(size = 6.6),
        panel.grid.major = element_blank(),                    # Rimuove le griglie principali
        panel.grid.minor = element_blank())

# Colori per le condizioni
colors <- c("moebius" = "#F0E442", "control" = "purple")

descritive <- df %>%
  filter(group %in% c("moebius", "control")) %>%
  group_by( group) %>%
  summarise(mean_acc = mean(ofmt, na.rm = TRUE),
            sd_acc = sd(ofmt, na.rm = TRUE),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    group == "moebius" ~ 1 + 0.1,
    group == "control" ~ 2 - 0.1,
  ))


# Panel B: Group boxplot
pB <- ggplot(df,
             aes(x = group, y = ofmt, fill = group)) +
  geom_rain(alpha = 0.5, rain.side = 'f1x1', id.long.var = "match") +
  geom_point(data = descritive,
             aes(x = x_pos, y = mean_acc),
             color = "black",
             size = 2.5, shape = 18, inherit.aes = FALSE) +
  geom_errorbar(data = descritive,
                aes(x = x_pos, ymin = mean_acc - se, ymax = mean_acc + se),
                width = 0, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = colors) +
  labs(
    x = "",
    y = "OFMT Accuracy (%)"
  ) +  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        text = element_text(size = 10, 
                            family = "Helvetica"),             # use Helvetica, size 16 for all text
        panel.background = element_blank(),                    # clean white background
        axis.line = element_line(colour = "black"),            # black axis lines
        strip.text.x = element_text(size = 6.6),               # facet labels (x): size 20
        strip.text.y = element_text(size = 6.6),
        panel.grid.major = element_blank(),                    # Rimuove le griglie principali
        panel.grid.minor = element_blank())+
  geom_text(data = descritive, aes(x = group, y = 0.62,
                                   label = paste0("M = ", round(mean_acc, 3), 
                                                  "\nSD = ", round(sd_acc, 3),"\n")),
            color = "black", size = 2.5)

# Combine both panels
figure4 <- cowplot::plot_grid(pA, pB, labels = "AUTO", nrow = 1)

ggsave("figures/Fig_3_finale.png",  # export for draft         
       plot = figure4,           
       device = "png",         
       dpi = 300,              
       width = 18,              
       height = 9,             
       units = "cm") 

ggsave("figures/Fig_3.tiff",  # export for draft         
       plot = figure4,           
       device = "tiff",         
       dpi = 1200,              
       width = 18,              
       height = 9,             
       units = "cm") 
# correlation
df <- df %>%
  filter(group == "moebius")
cor_test_Sunny <- cor.test(df$Sunnybrook, df$ofmt, method = "pearson")
print(cor_test_jefee)
bf_test_Sunny <- correlationBF(x = df$Sunnybrook, y = df$ofmt, paired = TRUE)
