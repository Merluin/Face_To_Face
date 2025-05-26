############################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS 
#  Date:        21/05/2025
#
#  Purpose:
#     This script defines the graphical standards and output format
#     for all plots included in the CARIPARO paper. It ensures that
#     all figures follow a consistent color scheme, font style, layout,
#     and export format (TIFF for high-quality publishing).
#
#  Update:      11/05/2025
#
############################################################################

#  FIGURE STYLE GUIDE — IN LINE WITH NATURE GUIDELINES
#  https://research-figure-guide.nature.com/figures/preparing-figures-our-specifications/

#  Required Elements:
#  ------------------
#  ✓ Axis lines and tick marks must be visible
#  ✓ All axes must be labeled and include **units in parentheses** 
#      → Example: "Response Time (ms)", "Accuracy (%)", "Force (N)"
#  ✓ Use an **accessible, color-blind–friendly palette**
#  ✓ Minimum font size must ensure **≥ 5 pt in final printed size**
#  ✓ Use **standard sans-serif fonts** such as Arial or Helvetica

#  Avoid the Following:
#  ---------------------
#  ✗ Background gridlines (use `panel.background = element_blank()`)
#  ✗ Superfluous icons, clipart, or decorative shapes
#  ✗ Drop shadows or 3D effects
#  ✗ Patterns (e.g., hatch lines) — use color or keylines instead
#  ✗ Text placed over images or noisy backgrounds — ensure readability
#  ✗ Overlapping text labels — use `ggrepel::geom_text_repel()` if needed
#  ✗ Colored text — instead, use black text with keys, labels, or borders

##########################
# COLOR PALETTE STANDARD #
##########################

# Colors optimized for color-blind individuals
# https://www.nature.com/articles/nmeth.1618/figures/2

# Group colors (use consistently across all figures):
"#F0E442" # Moebius group (orange, rgb: 230, 159, 0)
"#C07DA5" # Control group (Reddish purple rgb: 204, 121, 167)

# Stimulus set colors:
"#3271AD" # ADFES (blue rgb: 0, 114, 178)
"#C56637" # JeFEE (vermillon rgb: 213,94, 0 )
"#479D75" # Palsy (Bluish green rgb: 0, 158, 115 )

# Make sure to use these colors in manual scales:
+ scale_fill_manual(values = c("Moebius" = "#F0E442", "Control" = "#C07DA5"))

#####################
# TEXT & FORMATTING #
#####################

# Text formatting rules (apply in each plot script):
# - Axis titles: Title Case (first letter uppercase for each word)
# - Axis tick labels: all lowercase
# - No gridlines unless justified by data clarity
# - Minimal visual clutter, favor data over decoration

+ labs(x = NULL, y = "Mean Intensity (px)", fill = "Expression Type")

# General formatting to ensure uniform appearance:
+ theme(
  text = element_text(size = 10, family = "Helvetica"),  # use Helvetica, size 16 for all text
  panel.background = element_blank(),                    # clean white background
  axis.line = element_line(colour = "black"),            # black axis lines
  strip.text.x = element_text(size = 6.5),                # facet labels (x): size 20
  strip.text.y = element_text(size = 6.5)                 # facet labels (y): size 20
)


#####################
# PLOT COMBINATION  #
#####################

# Use `cowplot` to assemble multi-panel figures with consistent labels
# Example 1: Combine two plots vertically
p3 <- cowplot::plot_grid(fig1, fig2, ncol = 1, labels = "AUTO") # Vertical layout

# Example 2: Combine two plots horizontally
p3 <- cowplot::plot_grid(fig1, fig2, nrow = 1, labels = "AUTO") # Horizontal layout

# Labels will be automatically assigned (A, B, C...) for publication panels


#################
# EXPORT FIGURE #
#################

# Always export as TIFF for publication (e.g., Nature requires 1100 dpi)
ggsave("Fig.XX.tiff",           # Replace 'XX' with figure number (e.g., Fig.2.tiff)
       plot = myplot,           # Replace with your plot object (e.g., p3)
       device = "tiff",         # Ensure high-quality format
       dpi = 1200,              # High resolution (Nature, etc.)
       width = XX,              # Replace with final size in cm (e.g., 18)
       height = XX,             # Replace with final size in cm (e.g., 28)
       units = "cm")            # Keep units in cm for consistency with Word layout

# Export LOW-RAM Figure Version for Word Processing (e.g., for drafts) 
ggsave("Fig.XX_word.png",           
       plot = myplot,           
       device = "png",         
       dpi = 300,              
       width = XX,              
       height = XX,             
       units = "cm")

# Single-column figures: 8.8 cm
# Double-column figures: 18 cm
# Maximum figure size: 18 mm (w) x 21.5 mm (h).

# IMPORTANT:
# - Keep the final image size the same as in the manuscript (do not resize in Word)

#################################################
# 
# END
#
############################# graphical standards