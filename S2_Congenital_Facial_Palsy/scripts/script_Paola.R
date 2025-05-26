# Carica i pacchetti necessari  
library(tidyverse)  
library(afex)        # Per ANOVA mista  
library(emmeans)     # Per post-hoc e stime marginali  
library(BayesFactor) # Per Bayes Factor  
library(scales)      # Per formattazione scientifica  

# Imposta la visualizzazione dei numeri senza notazione scientifica  
options(scipen = 999)  

# Carica il dataset integrato  
df <- readxl::read_excel("congenital_accuracy_with_connectivity.xlsx")  

# Filtra solo Moebius e controlli  
df <- df %>% filter(group %in% c("moebius", "control"))  

# Calcola l'accuracy media per soggetto, gruppo e video_set  
acc_df <- df %>%  
  group_by(ID, group, video_set) %>%  
  summarise(Accuracy = mean(acc, na.rm = TRUE), .groups = "drop")  

# Trasforma in formato wide per t-test post-hoc  
acc_wide <- acc_df %>%  
  pivot_wider(names_from = video_set, values_from = Accuracy)  

# ANOVA mista per l'accuratezza  
acc_df$Expression <- factor(acc_df$video_set, levels = c("ADFES", "JeFEE"))  
acc_df$group <- factor(acc_df$group)  

# ANOVA con afex e stampa risultati dettagliati  
anova_acc <- aov_ez(  
  id = "ID",  
  dv = "Accuracy",  
  data = acc_df,  
  within = "Expression",  
  between = "group",  
  type = 3,  
  detailed = TRUE  
)  

# Funzione per formattare i p-value  
format_p <- function(p) {  
  if (p < 0.001) {  
    return(format(p, scientific = TRUE, digits = 3))  
  } else {  
    return(round(p, 6))  
  }  
}  

# Stampa risultati ANOVA con p-value esatti  
print("Mixed ANOVA Results:")  
anova_results <- nice(anova_acc, sig_symbols = c("", "", ""))  
print(anova_results)  

# Calcola e stampa eta squared parziale  
anova_table <- anova_acc$anova_table  
anova_table$pes <- anova_table$SSn / (anova_table$SSn + anova_table$SSd)  
print("\nDetailed effect sizes:")  
print(anova_table)  

# Post-hoc con p-value esatti  
emm_acc <- emmeans(anova_acc, ~ Expression * group)  
post_hoc <- pairs(emm_acc, simple = "each")  
post_hoc_df <- as.data.frame(post_hoc)  
post_hoc_df$p.value <- sapply(post_hoc_df$p.value, format_p)  
print("\nPost-hoc comparisons:")  
print(post_hoc_df)  

# T-test con p-value esatti  
t_adfes <- t.test(ADFES ~ group, data = acc_wide, var.equal = FALSE)  
t_jeffe <- t.test(JeFEE ~ group, data = acc_wide, var.equal = FALSE)  

print("\nWelch's t-tests:")  
print("ADFES comparison:")  
print(paste("t(", round(t_adfes$parameter, 2), ") = ",   
            round(t_adfes$statistic, 3),  
            ", p = ", format_p(t_adfes$p.value), sep=""))  

print("JeFEE comparison:")  
print(paste("t(", round(t_jeffe$parameter, 2), ") = ",   
            round(t_jeffe$statistic, 3),  
            ", p = ", format_p(t_jeffe$p.value), sep=""))  

# Cohen's d con intervalli di confidenza  
library(effsize)  
d_adfes <- cohen.d(acc_wide$ADFES[acc_wide$group == "moebius"],   
                   acc_wide$ADFES[acc_wide$group == "control"],  
                   conf.level = 0.95)  
d_jeffe <- cohen.d(acc_wide$JeFEE[acc_wide$group == "moebius"],   
                   acc_wide$JeFEE[acc_wide$group == "control"],  
                   conf.level = 0.95)  

print("\nEffect sizes (Cohen's d):")  
print(d_adfes)  
print(d_jeffe)  

# Bayes Factor con più dettagli  
bf_adfes <- ttestBF(  
  x = acc_wide$ADFES[acc_wide$group == "moebius"],  
  y = acc_wide$ADFES[acc_wide$group == "control"],  
  rscale = "medium"  
)  
bf_jeffe <- ttestBF(  
  x = acc_wide$JeFEE[acc_wide$group == "moebius"],  
  y = acc_wide$JeFEE[acc_wide$group == "control"],  
  rscale = "medium"  
)  

print("\nBayes Factors:")  
print("ADFES:")  
print(extractBF(bf_adfes))  
print("JeFEE:")  
print(extractBF(bf_jeffe))  

# Grafico più dettagliato  
p <- ggplot(acc_df, aes(x = Expression, y = Accuracy, fill = group)) +  
  geom_boxplot(alpha = 0.7) +  
  geom_point(position = position_dodge(width = 0.75), alpha = 0.4) +  
  theme_minimal() +  
  scale_fill_brewer(palette = "Set2") +  
  labs(  
    title = "Accuracy by Expression Type and Group",  
    x = "Expression Type",  
    y = "Accuracy",  
    fill = "Group"  
  ) +  
  theme(  
    plot.title = element_text(hjust = 0.5),  
    axis.text = element_text(size = 10),  
    axis.title = element_text(size = 12)  
  )  

# Salva il grafico  
ggsave("accuracy_analysis_detailed.pdf", p, width = 10, height = 6)  

# Se hai la colonna Intensity, ripeti le stesse analisi  
if ("Intensity" %in% names(df)) {  
  # ... [stesso codice di prima ma con le nuove formattazioni] ...  
}  