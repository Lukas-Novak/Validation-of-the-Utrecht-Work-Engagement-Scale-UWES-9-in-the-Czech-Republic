# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FINAL SCRIPT TO GENERATE SEMPLOTS FOR ALL 14 UNIQUE TESTED UWES-9 CFA MODELS
# Final version with corrected layouts, gray edges, plotted residuals, clean boxes, no self-loops,
# and a thematic numbering system for models.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. PREAMBLE: LOAD LIBRARIES
# -----------------------------------------------------------------------------
# Ensure these packages are installed: install.packages(c("lavaan", "semPlot"))
library(lavaan)
library(semPlot)

# 2. DATA
# -----------------------------------------------------------------------------
# This script assumes your data frame is already loaded and named 'data'.
# For reproducibility, we will create a dummy data frame.
# In your actual use, you would have your own data loaded.
set.seed(123)
data <- as.data.frame(matrix(rnorm(100 * 9), ncol = 9))
colnames(data) <- paste0("UWES_", 1:9)

# 3. DEFINE ALL 14 UNIQUE CFA MODELS
# -----------------------------------------------------------------------------
# Models are defined and grouped thematically.
# Group 1: One-Factor
model_one_factor <- "WE =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_6 + UWES_7 + UWES_8 + UWES_9"

# Group 2: Hierarchical
model_hierarchical <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n WE =~ Vigor + Dedication + Absorption"

# Group 3: Three-Factor
model_three_factor <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9"

# Group 4: Two-Factor Models
model_two_factor_willmer <- "VigDed =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_7\n Abs =~  UWES_6 + UWES_8 + UWES_9"
model_two_factor_chaudhary <- "VigAbs =~ UWES_1 + UWES_2 + UWES_5 + UWES_6 + UWES_8 + UWES_9\n Dedication =~ UWES_3 + UWES_4 + UWES_7"
model_two_factor_panthee <- "DedAbs =~ UWES_3 + UWES_4 + UWES_7 + UWES_6 + UWES_8 + UWES_9\n Vigor =~ UWES_1 + UWES_2 + UWES_5"

# Group 5: Modified Three-Factor Models (with correlated errors)
model_three_factor_corr_dominguez <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_8 ~~ UWES_9"
model_three_factor_corr_balducci <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_2 ~~ UWES_5\n UWES_6 ~~ UWES_8\n UWES_8 ~~ UWES_9"
model_three_factor_corr_chaudhary <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_5 ~~ UWES_6"
model_three_factor_corr_littman <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2"
model_three_factor_corr_seppala <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_8 ~~ UWES_9"
model_three_factor_corr_zecca <- "Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n UWES_1 ~~ UWES_2\n UWES_3 ~~ UWES_4\n UWES_8 ~~ UWES_9"

# Group 6: Bi-Factor Models
model_partial_bifactor <- "WE =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_6 + UWES_7 + UWES_8 + UWES_9\n Vigor =~ UWES_1 + UWES_2 + UWES_5\n Absorption =~ UWES_6 + UWES_8 + UWES_9\n WE ~~ 0*Vigor\n WE ~~ 0*Absorption\n Vigor ~~ 0*Absorption"
model_full_bifactor <- "WE =~ UWES_1 + UWES_2 + UWES_3 + UWES_4 + UWES_5 + UWES_6 + UWES_7 + UWES_8 + UWES_9\n Vigor =~ UWES_1 + UWES_2 + UWES_5\n Dedication =~ UWES_3 + UWES_4 + UWES_7\n Absorption =~ UWES_6 + UWES_8 + UWES_9"

# 4. FIT ALL MODELS USING CFA
# -----------------------------------------------------------------------------
fit_one_factor <- cfa(model_one_factor, data = data, std.lv = TRUE)
fit_hierarchical <- cfa(model_hierarchical, data = data, std.lv = TRUE)
fit_three_factor <- cfa(model_three_factor, data = data, std.lv = TRUE)
fit_two_factor_willmer <- cfa(model_two_factor_willmer, data = data, std.lv = TRUE)
fit_two_factor_chaudhary <- cfa(model_two_factor_chaudhary, data = data, std.lv = TRUE)
fit_two_factor_panthee <- cfa(model_two_factor_panthee, data = data, std.lv = TRUE)
fit_three_factor_corr_dominguez <- cfa(model_three_factor_corr_dominguez, data = data, std.lv = TRUE)
fit_three_factor_corr_balducci <- cfa(model_three_factor_corr_balducci, data = data, std.lv = TRUE)
fit_three_factor_corr_chaudhary <- cfa(model_three_factor_corr_chaudhary, data = data, std.lv = TRUE)
fit_three_factor_corr_littman <- cfa(model_three_factor_corr_littman, data = data, std.lv = TRUE)
fit_three_factor_corr_seppala <- cfa(model_three_factor_corr_seppala, data = data, std.lv = TRUE)
fit_three_factor_corr_zecca <- cfa(model_three_factor_corr_zecca, data = data, std.lv = TRUE)
fit_partial_bifactor <- cfa(model_partial_bifactor, data = data, std.lv = TRUE, orthogonal = TRUE)
fit_full_bifactor <- cfa(model_full_bifactor, data = data, std.lv = TRUE, orthogonal = TRUE)

# 5. DEFINE PUBLICATION-READY NAMES FOR PLOT TITLES
# -----------------------------------------------------------------------------
publication_name_map <- c(
  "fit_one_factor" = "1. One-Factor\n(Schaufeli et al., 2006)",
  "fit_hierarchical" = "2. Hierarchical\n(Domínguez-Salas et al., 2022)",
  "fit_three_factor" = "3. Three-Factor Correlated\n(Schaufeli et al., 2006)",
  "fit_two_factor_willmer" = "4a. Two-Factor (Vig/Ded vs. Abs)\n(Willmer et al., 2019)",
  "fit_two_factor_chaudhary" = "4b. Two-Factor (Vig/Abs vs. Ded)\n(Chaudhary et al., 2012)",
  "fit_two_factor_panthee" = "4c. Two-Factor (Ded/Abs vs. Vig)\n(Panthee et al., 2014)",
  "fit_three_factor_corr_dominguez" = "5a. Modified 3F (1~~2, 8~~9)\n(Domínguez-Salas et al., 2022)",
  "fit_three_factor_corr_balducci" = "5b. Modified 3F (Balducci)\n(Balducci et al., 2010)",
  "fit_three_factor_corr_chaudhary" = "5c. Modified 3F (5~~6)\n(Chaudhary et al., 2012)",
  "fit_three_factor_corr_littman" = "5d. Modified 3F (1~~2)\n(Littman-Ovadia & Balducci, 2013)",
  "fit_three_factor_corr_seppala" = "5e. Modified 3F (8~~9)\n(Seppälä et al., 2009)",
  "fit_three_factor_corr_zecca" = "5f. Modified 3F (Zecca)\n(Zecca et al., 2015)",
  "fit_partial_bifactor" = "6a. Partial Bi-Factor\n(de Bruin & Henn, 2013)",
  "fit_full_bifactor" = "6b. Full Bi-Factor Model"
)

# 6. GENERATE AND ARRANGE PLOTS
# -----------------------------------------------------------------------------
# Define common plot settings for consistency
plot_settings <- list(
  what = "paths", style = "ram", # Use "paths" instead of "std" to hide numbers
  edge.color = "gray40",
  intercepts = FALSE, # Hide intercepts
  selfLoops = FALSE, # Hide latent variances
  edge.width = 1.2, # Set a uniform, consistent edge width for all paths
  nCharNodes = 0,
  sizeMan = 8,
  sizeLat = 10,
  edge.label.cex = 0, # Set edge label size to 0 to hide all numbers
  label.cex = 1.2 # Hide all labels including factor loadings
)

# Function to create plot with title spacing
create_plot_with_title <- function(fit_obj, title_text, ...) {
  par(mar = c(1, 1, 8, 1)) # Increased top margin for title
  do.call(semPaths, c(list(object = fit_obj, ...), plot_settings))
  mtext(title_text, side = 3, line = 4.5, cex = 1.5, font = 2) # Positioned Title
}

# Save the final figure to a high-resolution SVG file
svg("Figure_1_All_CFA_Models_Final.svg", width = 24, height = 35)

# Add a global outer margin at the TOP of the figure to prevent text cutoff
par(oma = c(0, 0, 5, 0)) # Sets Outer Margin Area in lines of text c(bottom, left, top, right)

# Set layout matrix for 14 models in a wider 5x3 grid
layout_matrix <- matrix(c(1:14, 0), nrow = 5, ncol = 3, byrow = TRUE)
layout(layout_matrix)

# Create each plot in the specified order with its new title
create_plot_with_title(fit_one_factor, publication_name_map["fit_one_factor"], layout = "tree")
create_plot_with_title(fit_hierarchical, publication_name_map["fit_hierarchical"], layout = "tree2")
create_plot_with_title(fit_three_factor, publication_name_map["fit_three_factor"], layout = "tree2")

create_plot_with_title(fit_two_factor_willmer, publication_name_map["fit_two_factor_willmer"], rotation = 1)
create_plot_with_title(fit_two_factor_chaudhary, publication_name_map["fit_two_factor_chaudhary"], rotation = 1)
create_plot_with_title(fit_two_factor_panthee, publication_name_map["fit_two_factor_panthee"], rotation = 1)

create_plot_with_title(fit_three_factor_corr_dominguez, publication_name_map["fit_three_factor_corr_dominguez"], layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_three_factor_corr_balducci, publication_name_map["fit_three_factor_corr_balducci"], layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_three_factor_corr_chaudhary, publication_name_map["fit_three_factor_corr_chaudhary"], layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_three_factor_corr_littman, publication_name_map["fit_three_factor_corr_littman"], layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_three_factor_corr_seppala, publication_name_map["fit_three_factor_corr_seppala"], layout = "tree2", residuals = TRUE)
create_plot_with_title(fit_three_factor_corr_zecca, publication_name_map["fit_three_factor_corr_zecca"], layout = "tree2", residuals = TRUE)

create_plot_with_title(fit_partial_bifactor, publication_name_map["fit_partial_bifactor"], layout = "tree2", bifactor = "WE")
create_plot_with_title(fit_full_bifactor, publication_name_map["fit_full_bifactor"], layout = "tree2", bifactor = "WE")

dev.off() # Close the SVG device and save the file