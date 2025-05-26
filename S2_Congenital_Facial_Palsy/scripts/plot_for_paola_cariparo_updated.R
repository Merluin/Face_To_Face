library(tidyverse)
library(ggh4x)
library(patchwork)

# ---- Prepare data
data_paola <- readxl::read_excel("C:/Users/maffei/Downloads/Studio2_Complete_Dataset.xlsx")
data_plot <- data_paola %>% 
  select(subject, group, starts_with("connectivity")) %>% 
  pivot_longer(cols = 3:6, names_to = "temp", values_to = "conn_strength") %>% 
  separate(temp, into = c("seed", "movie_set"), sep = "S_") %>% 
  mutate(seed = paste0(seed, "S") %>% str_remove("connectivity_")) %>% 
  mutate(group = str_to_title(group),
         group = factor(group, levels = c("Moebius", "Control")),
         movie_set = ifelse(movie_set == "ADFES",
                            "Prototypical Expressions (ADFES)",
                            "Graded Expressions (JeFEE)") %>% 
           factor(levels = c("Prototypical Expressions (ADFES)",
                             "Graded Expressions (JeFEE)")))

data_plot_mean <- data_plot %>% 
  group_by(group, seed, movie_set) %>% 
  summarise(M = mean(conn_strength), SE = sd(conn_strength)/15) %>% 
  ungroup()

# ---- Create separate plots for adfes and jefee nd then combine
adfes_plot <- data_plot_mean %>% 
  filter(movie_set == "Prototypical Expressions (ADFES)") %>% 
  ggplot(aes(group, M, ymin = M - SE, ymax = M + SE)) + # change in x = seed for original Paola's plot
  geom_col(aes(fill = movie_set), width = .65) +
  geom_errorbar(width = .2) +
  geom_jitter(data = data_plot %>% filter(movie_set == "Prototypical Expressions (ADFES)"),
              aes(group, conn_strength, color = group), width = .15, inherit.aes = F, alpha = .9) +
  facet_nested_wrap(~movie_set*seed, nrow = 1, strip.position = "top", trim_blank = F) +
  scale_fill_manual(values = "#657d9a") +
  scale_color_manual(values = rev(c("#FF9900", "#7C3AED"))) + # Moebius e Controls)
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank()) +
  guides(fill = "none", color = "none") +
  labs(x = NULL, 
       y = "\U03BC-band Functional Connectivity") +
  ylim(c(0, 13))

jefee_plot <- data_plot_mean %>% 
  filter(movie_set != "Prototypical Expressions (ADFES)") %>% 
  ggplot(aes(group, M, ymin = M - SE, ymax = M + SE)) +
  geom_col(aes(fill = movie_set), width = .65) +
  geom_errorbar(width = .2) +
  geom_jitter(data = data_plot %>% filter(movie_set != "Prototypical Expressions (ADFES)"),
              aes(group, conn_strength, color = group), width = .15, inherit.aes = F, alpha = .9) +
  facet_nested_wrap(~movie_set*seed, nrow = 1, strip.position = "top", trim_blank = F) +
  scale_fill_manual(values = "#b37269") +
  scale_color_manual(values = rev(c("#FF9900", "#7C3AED"))) + # Moebius e Controls)
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank()) +
  guides(fill = "none", color = "none") +
  labs(x = NULL, 
       y = "\U03BC-band Functional Connectivity") +
  ylim(c(0, 13))


plot_combined <- adfes_plot + jefee_plot