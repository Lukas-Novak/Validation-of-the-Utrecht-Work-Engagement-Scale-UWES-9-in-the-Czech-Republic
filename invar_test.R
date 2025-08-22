# --- Step 1: Define the models with MANUAL scaling ---

# Model for the single-group (overall) baseline fit
# We still standardize the latent variance to 1 for interpretability
overall.model.UWES <- "
    Vigor =~ UWES_1 + UWES_2 + UWES_5
    Dedication =~ UWES_3 + UWES_4 + UWES_7
    Absorption =~ UWES_6 + UWES_8 + UWES_9
    # UWES_1 ~~ UWES_2
    # UWES_8 ~~ UWES_9
"
# --- Step 2: Run the single-group baseline model ---
baseline.cfa <- cfa(overall.model.UWES,
                    data = data,
                    ordered = TRUE,
                    # std.lv is now FALSE because we do it manually
                    estimator = "WLSMV",
                    meanstructure = TRUE)

# --- Step 3: Prepare data for multi-group analysis (Unchanged) ---
data.invar <- data %>%
  drop_na(UWES)

# --- Step 4: Manually conduct multi-group invariance testing ---
# We now use the MANUAL model and remove std.lv=TRUE from all calls

# Configural Invariance
fit.configural <- cfa(overall.model.UWES,
                      data = data.invar,
                      group = "Gender",
                      ordered = TRUE,
                      estimator = "WLSMV",
                      meanstructure = TRUE)

# Metric Invariance
fit.metric <- cfa(overall.model.UWES,
                  data = data.invar,
                  group = "Gender",
                  group.equal = "loadings",
                  ordered = TRUE,
                  estimator = "WLSMV",
                  meanstructure = TRUE)

# Scalar Invariance
fit.scalar <- cfa(overall.model.UWES,
                  data = data.invar,
                  group = "Gender",
                  group.equal = c("loadings", "intercepts"),
                  ordered = TRUE,
                  estimator = "WLSMV",
                  meanstructure = TRUE)

# Strict Invariance
fit.strict <- cfa(overall.model.UWES,
                  data = data.invar,
                  group = "Gender",
                  group.equal = c("loadings", "intercepts", "residuals"),
                  ordered = TRUE,
                  estimator = "WLSMV",
                  meanstructure = TRUE)


# --- Step 5: Build the fit table (This part should now work correctly) ---
fit.indices <- c('chisq.scaled', 'df.scaled', 'pvalue.scaled', 'cfi.scaled',
                 'tli.scaled', 'rmsea.scaled', "rmsea.ci.lower.scaled",
                 "rmsea.ci.upper.scaled", 'srmr')

tab.fit <- matrix(nrow = 5, ncol = 10)
colnames(tab.fit) <- c("Model", "x2", "df", "pvalue", "CFI", "TLI", "rmsea",
                       "rmsea.ci.lower", "rmsea.ci.upper", "SRMR")

# Populate the table
tab.fit[1,] <- c("Baseline", round(fitMeasures(baseline.cfa, fit.indices), 3))
tab.fit[2,] <- c("Configural", round(fitMeasures(fit.configural, fit.indices), 3))
tab.fit[3,] <- c("Metric", round(fitMeasures(fit.metric, fit.indices), 3))
tab.fit[4,] <- c("Scalar", round(fitMeasures(fit.scalar, fit.indices), 3))
tab.fit[5,] <- c("Strict", round(fitMeasures(fit.strict, fit.indices), 3))

tab.fit = tab.fit %>% 
  as_tibble() %>% 
  mutate(pvalue = as.numeric(pvalue)) %>% 
  mutate(pvalue = format_p(pvalue)) %>% 
  mutate(rmsea = paste0(rmsea," 90% CI (",rmsea.ci.lower,"-",rmsea.ci.upper,")")) %>% 
  select(!starts_with(c("rmsea.ci.","rmsea.ci.lower","rmsea.ci.upper")))

# --- Step 6: Perform Chi-Square Difference Testing using a ROBUST method ---
# The default anova() method can fail with WLSMV.
# Using method = "satorra.bentler.2010" is the correct, robust way.
# cat("\n--- Model Comparisons (Robust Chi-Square Difference Tests) ---\n")
# print(lavTestLRT(fit.configural, fit.metric, method = "satorra.bentler.2010"))
# print(lavTestLRT(fit.metric, fit.scalar, method = "satorra.bentler.2010"))
# print(lavTestLRT(fit.scalar, fit.strict, method = "satorra.bentler.2010"))
# 
# 
# # --- Step 7: Format and display the final results table ---
# cat("\n--- Summary of Model Fit Indices ---\n")
# print(tab.fit)