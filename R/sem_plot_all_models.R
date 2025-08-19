# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FINAL SCRIPT TO GENERATE SEMPLOTS FOR ALL 13 TESTED UWES-9 CFA MODELS
# Final version with corrected layouts, gray edges, plotted residuals, clean boxes, and no self-loops.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. PREAMBLE: LOAD LIBRARIES
# -----------------------------------------------------------------------------
# Ensure these packages are installed: install.packages(c("lavaan", "semPlot"))
library(lavaan)
library(semPlot)

# 2. DATA
# -----------------------------------------------------------------------------
# This script assumes your data frame is already loaded and named 'dat'.

# 3. DEFINE ALL 13 UNIQUE CFA MODELS
# -----------------------------------------------------------------------------
model_1_one_factor_schaufeli <- "WE =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_6 + UWES_7 + UWES_8 + UWES_9"
model_2_two_factor_willmer <- "VigDed =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_7\n Abs =~  UWES_6 + UWES_8 + UWES_9"
model_3_two_factor_chaudhary <- "VigAbs =~ UWES_1 + UWES_2 + UWES_5 + UWES_6 + UWES_8 + UWES_9\n Dedication =~ UWES_3 + UWES_4 + UWES_7"
model_4_two_factor_panthee <- "DedAbs =~ UWES_3 + UWES_4 + UWES_7 + UWES_6 + UWES_8 + UWES_9\n Vigor =~ UWES_1 + UWES_2 + UWES_5"
model_5_three_factor_schaufeli <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9"
model_6_three_factor_corr_dominguez <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_8 ~~ UWES_9"
model_7_three_factor_corr_balducci <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_2 ~~ UWES_5\n UWES_6 ~~ UWES_8\n UWES_8 ~~ UWES_9"
model_8_three_factor_corr_chaudhary <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_5 ~~ UWES_6"
model_9_three_factor_corr_littman <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2"
model_10_three_factor_corr_seppala <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_8 ~~ UWES_9"
model_11_three_factor_corr_zecca <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_3 ~~ UWES_4\n UWES_8 ~~ UWES_9"
model_12_hierarchical_dominguez <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n WE =~ Vigor + Dedication + Absorption"
model_13_partial_bifactor_debruin <- "WE =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_6 + UWES_7 + UWES_8 + UWES_9\n Vigor =~ UWES_1 + UWES_2 + UWES_5\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n WE ~~ 0*Vigor\n WE ~~ 0*Absorption\n Vigor ~~ 0*Absorption"
model_14_combined_modified <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_3 ~~ UWES_4\n UWES_8 ~~ UWES_9"
model_15_replicated_errors <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_8 ~~ UWES_9"

# 4. FIT ALL MODELS USING CFA (using 'dat' and ML estimator as per user's log)
# -----------------------------------------------------------------------------
fit_1  <- cfa(model_1_one_factor_schaufeli, data = dat, std.lv = TRUE)
fit_2  <- cfa(model_2_two_factor_willmer, data = dat, std.lv = TRUE)
fit_3  <- cfa(model_3_two_factor_chaudhary, data = dat, std.lv = TRUE)
fit_4  <- cfa(model_4_two_factor_panthee, data = dat, std.lv = TRUE)
fit_5  <- cfa(model_5_three_factor_schaufeli, data = dat, std.lv = TRUE)
fit_6  <- cfa(model_6_three_factor_corr_dominguez, data = dat, std.lv = TRUE)
fit_7  <- cfa(model_7_three_factor_corr_balducci, data = dat, std.lv = TRUE)
fit_8  <- cfa(model_8_three_factor_corr_chaudhary, data = dat, std.lv = TRUE)
fit_9  <- cfa(model_9_three_factor_corr_littman, data = dat, std.lv = TRUE)
fit_10 <- cfa(model_10_three_factor_corr_seppala, data = dat, std.lv = TRUE)
fit_11 <- cfa(model_11_three_factor_corr_zecca, data = dat, std.lv = TRUE)
fit_12 <- cfa(model_12_hierarchical_dominguez, data = dat, std.lv = TRUE)
fit_13 <- cfa(model_13_partial_bifactor_debruin, data = dat, std.lv = TRUE, orthogonal=TRUE)
fit_14 <- cfa(model_14_combined_modified, data = dat, std.lv = TRUE)
fit_15 <- cfa(model_15_replicated_errors, data = dat, std.lv = TRUE)

# 5. GENERATE AND ARRANGE PLOTS
# -----------------------------------------------------------------------------
# Define common plot settings for consistency
plot_settings <- list(what = "paths", style = "ram", # Use "paths" instead of "std" to hide numbers
                      edge.color = "gray40", 
                      intercepts = FALSE, # Hide intercepts
                      selfLoops = FALSE,  # Hide latent variances
                      edge.width = 1.2,   # Set a uniform, consistent edge width for all paths
                      nCharNodes = 0,     
                      sizeMan = 8,
                      sizeLat = 10, 
                      edge.label.cex = 0, # Set edge label size to 0 to hide all numbers
                      bifactor = "g",
                      label.cex = 1.2       # Hide all labels including factor loadings
)

# Create plots with explicit spacing using layout function
# Save the final figure to a high-resolution PNG file
svg("Figure_1_All_CFA_Models_Final.svg", width = 15, height = 35)

# Set layout matrix with explicit spacing for 5 rows, 3 columns (15 models total)
layout_matrix <- matrix(1:15, nrow = 5, ncol = 3, byrow = TRUE)
layout(layout_matrix, heights = c(1, 1, 1, 1, 1))

# Function to create plot with title spacing
create_plot_with_title <- function(fit_obj, title_text, ...) {
  par(mar = c(1, 1, 8, 1))  # Further increased top margin
  do.call(semPaths, c(list(object = fit_obj, ...), plot_settings))
  
  # Add significant spacing with empty space at the top and between title and plot
  mtext("", side = 3, line = 7)  # Empty space at top of page
  mtext("", side = 3, line = 6)  # Additional empty space
  mtext(title_text, side = 3, line = 4.5, cex = 1.2, font = 2)  # Title position
  mtext("", side = 3, line = 3)  # Empty space between title and plot
}

# Create each plot with spacing
create_plot_with_title(fit_1, "A) One-Factor\n(Schaufeli et al., 2006)", layout = "tree")
create_plot_with_title(fit_2, "B) Two-Factor (Vig/Ded vs Abs)\n(Willmer et al., 2019)", rotation = 1)
create_plot_with_title(fit_3, "C) Two-Factor (Vig/Abs vs Ded)\n(Chaudhary et al., 2012)", rotation = 1)
create_plot_with_title(fit_4, "D) Two-Factor (Ded/Abs vs Vig)\n(Panthee et al., 2014)", rotation = 1)
create_plot_with_title(fit_5, "E) Three-Factor Correlated\n(Schaufeli et al., 2006)", layout = "tree2")
create_plot_with_title(fit_6, "F) Modified 3-Factor (1~~2, 8~~9)\n(Domínguez-Salas et al., 2022)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_7, "G) Modified 3-Factor (Balducci)\n(Balducci et al., 2010)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_8, "H) Modified 3-Factor (5~~6)\n(Chaudhary et al., 2012)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_9, "I) Modified 3-Factor (1~~2)\n(Littman-Ovadia & Balducci, 2013)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_10, "J) Modified 3-Factor (8~~9)\n(Seppälä et al., 2009)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_11, "K) Modified 3-Factor (Zecca)\n(Zecca et al., 2015)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_12, "L) Hierarchical Model\n(Domínguez-Salas et al., 2022)", layout = "tree2")
create_plot_with_title(fit_13, "M) Partial Bi-Factor Model\n(de Bruin & Henn, 2013)", layout = "tree2")
create_plot_with_title(fit_14, "N) Combined Modified 3-Factor\n(1~~2, 3~~4, 8~~9)", layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_15, "O) Replicated Errors Model\n(1~~2, 8~~9)", layout = "tree2", residuals = TRUE)

# No empty plots needed - all 15 positions are filled

dev.off() # Close the PNG device and save the file