###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        03/08/2022
#     This script performs data analysis for the CARIPARO experiment.
#
#  Experiment   MBS
#
#  Update:      11/05/2025
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------
#replace_csv("group", "control")

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))
data <- read.xlsx(file.choose())
OFMT <- read.xlsx(file.choose()) %>%
  select(subject,mean_acc) %>%
  'colnames<-'(c("subject","ofmt"))
data <- left_join(data, OFMT, by = "subject")

write_csv(data, "assesment_mbs.csv")

moebius_data <- data %>% 
  filter(group == "moebius")

# ADFES
cor_test_adfes <- cor.test(moebius_data$Sunnybrook, moebius_data$acc_ADFES, method = "pearson")
print(cor_test_adfes)

# JeFEE
cor_test_jefee <- cor.test(moebius_data$Sunnybrook, moebius_data$acc_JeFEE, method = "pearson")
print(cor_test_jefee)


p1 <- ggplot(moebius_data, aes(x = Sunnybrook, y = acc_ADFES)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "blue", fill = "blue", alpha = 0.3) +
  labs(title = " ", x = "SFGS Score", y = "ADFES Accuracy") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))+
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p2 <- ggplot(moebius_data, aes(x = Sunnybrook, y = acc_JeFEE)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "red", fill = "red", alpha = 0.3) +
  labs(title = "", x = "SFGS Score", y = "JeFEE Accuracy") +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 1))+
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")




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

# Define colors
color_map <- c("ADFES" = "blue", "JeFEE" = "red")

# Main plot
p3 <- ggplot(df_boot, aes(x = r, fill = test)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = boot_adfes$t0, color = "blue", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = ci_adfes, color = "blue", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = boot_jefee$t0, color = "red", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = ci_jefee, color = "red", linetype = "dashed", linewidth = 0.8) +
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
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 3, 
           label = "ADFES statistics:\nMean r = 0.635\n 95% CI: [0.436, 0.830]\nP(r > 0) = 1.000", color = "blue", size = 3) +
  annotate("text", x = 0, y = 1.2, 
           label = "JeFEE statistics:\nMean r = 0.647\n 95% CI: [0.376, 0.839]\nP(r > 0) = 0.999", color = "red", size = 3)


# Combine
p4 <- cowplot::plot_grid(cowplot::plot_grid(p1, p2), p3,  nrow = 2, labels = "AUTO")

ggsave("files/Figure_3.jpg", 
       plot = p4,     
       device = "jpeg", 
       dpi = 600,
       width = 18, height = 18, units = "cm")




# correlation sunnybrook & OFMT
dataset_gw1 <- left_join(dataset_gw1,OFMT%>%
                           'colnames<-'(c("Pt.code","ofmt")),by = "Pt.code")

df <- dataset_gw1 %>%
  select(Pt.id, Pt.group,Pt.match, ofmt) %>%
  group_by(Pt.id, Pt.group,Pt.match) %>%
  summarise(OFMT = mean(ofmt, na.rm = TRUE))


# Panel A: Scatterplot (no correlation)
pA <- ggplot(moebius_data, aes(x = Sunnybrook, y = OFMT)) +
  geom_point(color = "#CC5500"  ) +
  geom_smooth(method = "lm", se = TRUE, color = "#FFA64D", fill = "#FFDAB3", alpha = 0.3) +
  labs(
    x = "Sunnybrook Score (Facial Motor Function)",
    y = "OFMT Accuracy (%)"
  ) +
  theme_minimal()

# Colori per le condizioni
colors <- c("moebius" = "#FFA64D", "control" = "purple")

descritive <- df %>%
  filter(Pt.group %in% c("moebius", "control")) %>%
  group_by( Pt.group) %>%
  summarise(mean_acc = mean(OFMT, na.rm = TRUE),
            sd_acc = sd(OFMT, na.rm = TRUE),
            se = sd_acc / sqrt(n()),
            .groups = "drop") %>%
  mutate(x_pos = case_when(
    Pt.group == "moebius" ~ 1 + 0.1,
    Pt.group == "control" ~ 2 - 0.1,
  ))


# Panel B: Group boxplot
pB <- ggplot(df,
             aes(x = Pt.group, y = OFMT, fill = Pt.group)) +
  geom_rain(alpha = 0.5, rain.side = 'f1x1', id.long.var = "Pt.match") +
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
  theme(legend.position = "non")+
  geom_text(data = descritive, aes(x = Pt.group, y = 0.62,
                                   label = paste0("M = ", round(mean_acc, 3), 
                                                  "\nSD = ", round(sd_acc, 3))),
            color = "black", size = 4)

# Combine both panels
figure4 <- cowplot::plot_grid(pA, pB, labels = "AUTO", ncol = 1)

ggsave("files/Figure_4.jpg", 
       plot = figure4,     
       device = "jpeg", 
       dpi = 600,
       width = 18, height = 28, units = "cm")
