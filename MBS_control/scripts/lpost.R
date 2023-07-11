###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        0382022
#     This script performs data analysis for the CARIPARO experiment, 
#     specifically for the Perceived intensity measure of the MBScontrol experiment. 
#     It computes datasets, generates plots, tables, and fits a 
#     mixed-effects model to analyze the response Lpost.
#
#  Experiment   MBScontrol
#
#  Update:      23/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions & libraries---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------
load(file.path("objects","mbs_circular.RData"))

# Calculate mean intensity
lpost_data <-  dataset_gw1 %>%
  dplyr::select( Pt.code, Pt.group,contains("Asmt.lpost")) %>%
  distinct()%>%
  gather(lpost,score,contains("Asmt.lpost"))

# Generate aplot for emotions intensities
plot_emotion<- flexplot(score ~ Pt.group | lpost,
                        spread = "stdev", # sterr, stdev, 
                        data = lpost_data,
                        alpha = 0.07) +
  theme(legend.position = "bottom") +
  ylab("Biological movement (mean & Sd)") +
  xlab("lpost")

# Fit  correct for each emotion--------------------------------------------

# Fit linear mixed-effects model
fit <- lmer(score ~ Pt.group * lpost + (1|Pt.code),
            data = lpost_data%>%filter(Pt.code != "10_control" & lpost != "Asmt.lpost.tot"))

fit9 <- glm(score ~ Pt.group ,
            data = lpost_data%>%filter(Pt.code != "10_control" & lpost == "Asmt.lpost.9"))

# Generate table summary
table <- tab_model(fit) #, show.df = FALSE, string.p = "p adjusted", p.adjust = "bonferroni")
visualize(fit, plot = "residuals")
# Create ANOVA table
# Perform ANOVA
chiquadro <- car::Anova(fit, test.statistic= "Chisq", type = "3") # test.statistic=c("Chisq", "F")

chi_table <- chiquadro %>%
  drop_na(`Pr(>Chisq)`) %>%
  mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
  kbl(caption = "Anova(model, type = 3)") %>%
  column_spec(4, color = ifelse(chiquadro$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Contrasts
emotion <- testInteractions(fit, pairwise = "emotion", adjustment = "fdr")
video<- testInteractions(fit, pairwise = "video_set", adjustment = "fdr")
emotionvideo<- testInteractions(fit, pairwise = "video_set", fixed = "emotion", adjustment = "fdr")

# data table of contrast
temp<-rbind(emotion,
            video,
            emotionvideo)

# kable table object
contrast<-temp%>%
  drop_na(`Pr(>Chisq)`) %>%
  mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
  kbl(caption = "Contrasts (FDR corrected)") %>%
  column_spec(5, color = ifelse(temp$`Pr(>Chisq)` <= 0.05, "red", "black")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Generate plot
themeperso <- theme_paper(font_size = 10) +
  theme(legend.position = "bottom")

plotemotion <- ggpredict(fit, terms = c( "emotion"))%>%
  plot()+ 
  labs(title = "Main effect emotion",
       x = "Emotions",
       y = "Intensity (px)") +
  themeperso

plotvideo <- ggpredict(fit, terms = c( "video_set"))%>%
  plot()+ 
  labs(title = "Main effect Video set",
       x = "Emotions",
       y = "Intensity (px)") +
  themeperso

plotvideoemotion <- ggpredict(fit, terms = c( "emotion", "video_set"))%>%
  plot()+ 
  labs(title = "Interaction Video set * emotion",
       x = "Emotions",
       y = "Intensity (px)") +
  themeperso

ploteffect<-ploteffect<-cowplot::plot_grid(plotemotion,
                                           plotvideo,plotvideoemotion,
                                           ncol = 1,
                                           scale = c(.9, .9, .9))


# Save the results
save(fit, table, chi_table, contrast, ploteffect,table_intensity, plot_emotion,  file = file.path("models/perceived_intensity.RData"))

#################################################
# 
# END
#
############################# perceived intensity