

library(tidyverse)
library(cowplot)
library(ggplot2)
library(gridExtra)
library(snowfall)
library(BaFTA)

#load and prep data
setwd("C:/Users/luisaf/OneDrive - University of Tasmania/Postdoc/senescence paper/2023 senescence")

data <- read.csv(file = "OcellatusNatPop_FullDataset_2025.csv", na = c("", "NA", ".", "?")
)[ ,c('Year', 'FemID', 'Site', 'Age', 'Clutch')]

data <- data %>%
  rename(
    year = Year,
    indID = FemID,
    site = Site,
    nOffspring = Clutch)

data$indID <- as.factor(data$indID)
data$site <- as.factor(data$site)
data$Age <- as.integer(data$Age)
data$nOffspring <- as.integer(data$nOffspring)

data <- subset(data,
              !is.na(indID) & indID != 0 &
              !is.na(Age) & Age != 0 &
              !is.na(nOffspring) & nOffspring != 0)  #n = 1898

head(data)

dataCP <- subset(data, site == "CP")
dataOR <- subset(data, site == "OR")


######################################################
###  CP ##############################################
######################################################

# Plot raw data to see the actual pattern
ggplot(dataCP, aes(x = Age, y = nOffspring)) +
  geom_point() +
  stat_summary(fun = mean, geom = "line", color = "cornflowerblue") +
  theme_minimal()
# this looks like early increase then plateau, 
# possibly decrease at old ages
# compare quadratic, gamma, Colchero-Muller models

# 1. Quadratic 
# initially failed to converge, so run longer chains
out_quad_CP <- bafta(object = dataCP, model = "quadratic",
                dataType = "indivSimple", nsim = 4, ncpus = 4,
                niter = 50000, burnin = 10000, thinning = 50)

out_quad_CP

#Convergence: Some parameter chains did not converge.
#Model fit: DIC = 4128.42
#Predictive loss: Good. Fit   Penalty   Deviance
#                       1318    4460     5778
# smallest Neff = 300. Should be over 400

plot(out_quad_CP)

plot(out_quad_CP, type = "fertility")

plot(out_quad_CP, type = "predictive")


# 2. Gamma 
out_gamma_CP <- bafta(object = dataCP, model = "gamma",
                dataType = "indivSimple", nsim = 4, ncpus = 4,
                niter = 50000, burnin = 10000, thinning = 50)

out_gamma_CP

#Convergence: All parameter chains converged.
#Model fit: DIC = 4237.65
#Predictive loss: Good. Fit   Penalty   Deviance
#                       1537    4356     5894
# smallest Neff = 820 - OK

plot(out_gamma_CP)

plot(out_gamma_CP, type = "fertility")

plot(out_gamma_CP, type = "predictive")

# 3. ColcheroMuller
out_CM_CP <- bafta(object = dataCP, model = "ColcheroMuller",
                      dataType = "indivSimple", nsim = 4, ncpus = 4,
                   niter = 50000, burnin = 10000, thinning = 50)

out_CM_CP

#Convergence: All parameter chains converged.
#Model fit: DIC = 4129.03
#Predictive loss: Good. Fit   Penalty   Deviance
#                       1311    4468     5779
# smallest Neff = 642 - OK

plot(out_CM_CP)

plot(out_CM_CP, type = "fertility")

plot(out_CM_CP, type = "predictive")

#Save result so no need to run again
dir.create("bafta outputs", showWarnings = FALSE)
save_path <- file.path(getwd(), "bafta outputs", "out_CM_CP.rds")
saveRDS(out_CM_CP, save_path)

# CM model best fit for CP data. DIC less than 2
# from quadratic model which had convergence problems
# visually, both quadratic and CM fit data well. 

######################################################
###  OR ##############################################
######################################################

# Plot raw data to see the actual pattern
ggplot(dataOR, aes(x = Age, y = nOffspring)) +
  geom_point() +
  stat_summary(fun = mean, geom = "line", color = "coral") +
  theme_minimal()
# this looks like early increase then plateau, 
# possibly decrease at old ages
# compare quadratic, gamma, Colchero-Muller models

# 1. Quadratic 
# failed to converge, so run longer chains
out_quad_OR <- bafta(object = dataOR, model = "quadratic",
                     dataType = "indivSimple", nsim = 4, ncpus = 4,
                     niter = 50000, burnin = 10000, thinning = 50)

out_quad_OR

#Convergence: All parameter chains converged.

#Model fit: DIC = 2223.6
#Predictive loss: Good. Fit   Penalty   Deviance
#                       526    1859     2385
# smallest Neff = 792. Good

plot(out_quad_OR)

plot(out_quad_OR, type = "fertility")

plot(out_quad_OR, type = "predictive")


# 2. Gamma 
out_gamma_OR <- bafta(object = dataOR, model = "gamma",
                      dataType = "indivSimple", nsim = 4, ncpus = 4,
                      niter = 50000, burnin = 10000, thinning = 50)

out_gamma_OR

#Convergence: All parameter chains converged.
#Model fit: DIC = 2269.45
#Predictive loss: Good. Fit   Penalty   Deviance
#                       592    1804     2396
# smallest Neff = 845 - OK

plot(out_gamma_OR)

plot(out_gamma_OR, type = "fertility")

plot(out_gamma_OR, type = "predictive")

# 3. ColcheroMuller
out_CM_OR <- bafta(object = dataOR, model = "ColcheroMuller",
                   dataType = "indivSimple", nsim = 4, ncpus = 4,
                   niter = 50000, burnin = 10000, thinning = 50)

out_CM_OR

#Convergence: All parameter chains converged.
#Model fit: DIC = 2226.09
#Predictive loss: Good. Fit   Penalty   Deviance
#                       530    1864     2394
# smallest Neff = 642 - OK

plot(out_CM_OR)

plot(out_CM_OR, type = "fertility")

plot(out_CM_OR, type = "predictive")

# quadratic model best fit for OR data. DIC 2.49 from CM model
# visually, best fit. CM has artifact at young ages. 

#######################################################

# BaFTA supports previous modelling on litter size with age 
#- no real difference between sites
#- no evidence of senescence, rather increase and plateau

#use colchero-mueller for both populations so we can compare terms across both.


##################################################################
# Extract fertility predictions from both models

# Extract fertility data from both models
# CP population
fert_CP <- data.frame(
  Age = out_CM_CP$x,
  Mean = out_CM_CP$fert[, "Mean"],
  Lower = out_CM_CP$fert[, "Lower"],
  Upper = out_CM_CP$fert[, "Upper"],
  Population = "Cool Highlands (CP)"
)

# OR population
fert_OR <- data.frame(
  Age = out_CM_OR$x,
  Mean = out_CM_OR$fert[, "Mean"],
  Lower = out_CM_OR$fert[, "Lower"],
  Upper = out_CM_OR$fert[, "Upper"],
  Population = "Warm Lowlands (OR)"
)

# Combine
fert_combined <- rbind(fert_CP, fert_OR)

# Create the plot
ggplot(fert_combined, aes(x = Age, y = Mean, color = Population, fill = Population)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Cool Highlands (CP)" = "cornflowerblue", 
                                "Warm Lowlands (OR)" = "coral")) +
  scale_fill_manual(values = c("Cool Highlands (CP)" = "cornflowerblue", 
                               "Warm Lowlands (OR)" = "coral")) +
  labs(x = "Age (years)", 
       y = "Expected litter size",
       color = "Population",
       fill = "Population") +
  theme_classic() +
  theme(legend.position = NULL)

# Save if needed
ggsave("fertility_comparison.pdf", width = 7, height = 5)
ggsave("fertility_comparison.png", width = 7, height = 5)

### With raw data
ggplot() +
  # Raw data points (semi-transparent)
  geom_jitter(data = dataCP, aes(x = Age, y = nOffspring), 
             color = "cornflowerblue", alpha = 0.2, size = 1) +
  geom_jitter(data = dataOR, aes(x = Age, y = nOffspring), 
             color = "coral", alpha = 0.2, size = 1) +
  # Fitted curves with confidence intervals
  geom_ribbon(data = fert_combined, 
              aes(x = Age, ymin = Lower, ymax = Upper, fill = Population), 
              alpha = 0.3) +
  geom_line(data = fert_combined, 
            aes(x = Age, y = Mean, color = Population), 
            size = 1.2) +
  scale_color_manual(values = c("Cool Highlands (CP)" = "cornflowerblue", 
                                "Warm Lowlands (OR)" = "coral")) +
  scale_fill_manual(values = c("Cool Highlands (CP)" = "cornflowerblue", 
                               "Warm Lowlands (OR)" = "coral")) +
  labs(x = "Age (years)", 
       y = "Litter size",
       color = "Population",
       fill = "Population") +
  theme_classic() +
  theme(legend.position = NULL)

#######################################
### Plot with posterior parameter densities

# Define colors and theme
site_colors <- c("Warm lowland" = "coral1", "Cool highland" = "cornflowerblue")

theme_publication <- theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10))

# ============== EXTRACT FERTILITY TRAJECTORIES ==============
fert_OR <- data.frame(
  age = out_CM_OR$x,
  median = out_CM_OR$fert[, "Mean"],
  lower = out_CM_OR$fert[, "Lower"],
  upper = out_CM_OR$fert[, "Upper"],
  site = "Warm lowland"
)

fert_CP <- data.frame(
  age = out_CM_CP$x,
  median = out_CM_CP$fert[, "Mean"],
  lower = out_CM_CP$fert[, "Lower"],
  upper = out_CM_CP$fert[, "Upper"],
  site = "Cool highland"
)

fert_data <- bind_rows(fert_OR, fert_CP)

# ============== EXTRACT KEY PARAMETER POSTERIORS ==============
params_OR <- as.data.frame(out_CM_OR$theta) %>%
  mutate(site = "Warm lowland")
params_CP <- as.data.frame(out_CM_CP$theta) %>%
  mutate(site = "Cool highland")

# Focus on key parameters only (b0 = peak fertility, b1 = senescence rate)
params_combined <- bind_rows(params_OR, params_CP) %>%
  pivot_longer(cols = c(b0, b3),
               names_to = "parameter", 
               values_to = "value")

# ============== PANEL A: FERTILITY TRAJECTORIES ==============
p_fertility <- ggplot(fert_data, aes(x = age, y = median, color = site, fill = site)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  scale_x_continuous(breaks = seq(2, 12, 2), limits = c(2, 12)) +
  labs(y = "Expected litter size", x = "Age (years)") +
  theme_publication +
  theme(legend.position = "none")

# ============== PANEL B: KEY PARAMETER DISTRIBUTIONS ==============
p_params <- ggplot(params_combined, aes(x = value, fill = site, color = site)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  facet_wrap(~parameter, scales = "free", ncol = 1,
             labeller = labeller(parameter = c(
               "b0" = "b0 (baseline fertility)",
               "b3" = "b3 (senescence rate)"))) +
  scale_fill_manual(values = site_colors) +
  scale_color_manual(values = site_colors) +
  labs(x = "Parameter value", y = "Posterior density") +
  theme_publication +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 10, hjust = 0))

# ============== COMBINE PANELS ==============
combined_figure <- plot_grid(
  p_fertility, p_params,
  ncol = 2,
  labels = c("C", "D"),
  label_size = 14,
  rel_widths = c(1, 1))

combined_figure

# ============== SAVE ==============
ggsave("Figure_fertility_and_parameters.png", 
       combined_figure,
       width = 10, height = 5,
       units = "in",
       dpi = 600)

ggsave("Figure_fertility_and_parameters.pdf", 
       combined_figure,
       width = 10, height = 5,
       units = "in")


##################################



###
# Extract coefficient table
coef_CP <- out_CM_CP$coefficients
coef_OR <- out_CM_OR$coefficients

# View them
print("CP coefficients:")
print(coef_CP)

print("OR coefficients:")
print(coef_OR)


# Save outputs to word table
library(flextable)
library(officer)
library(tidyverse)

# Extract coefficients from both models
coef_CP <- out_CM_CP$coefficients
coef_OR <- out_CM_OR$coefficients

# Function to round to significant figures
signif_format <- function(x, digits = 3) {
  signif(x, digits)
}

# Create a combined dataframe with UNIQUE column names
coef_table <- data.frame(
  Parameter = rownames(coef_CP),
  CP_Mean = signif_format(coef_CP[, "Mean"]),
  CP_SD = signif_format(coef_CP[, "SD"]),
  CP_Lower = signif_format(coef_CP[, "Lower"]),
  CP_Upper = signif_format(coef_CP[, "Upper"]),
  OR_Mean = signif_format(coef_OR[, "Mean"]),
  OR_SD = signif_format(coef_OR[, "SD"]),
  OR_Lower = signif_format(coef_OR[, "Lower"]),
  OR_Upper = signif_format(coef_OR[, "Upper"])
)

# Create flextable
ft <- flextable(coef_table) %>%
  set_header_labels(
    Parameter = "Parameter",
    CP_Mean = "Mean",
    CP_SD = "SD",
    CP_Lower = "Lower 95% CI",
    CP_Upper = "Upper 95% CI",
    OR_Mean = "Mean",
    OR_SD = "SD",
    OR_Lower = "Lower 95% CI",
    OR_Upper = "Upper 95% CI"
  ) %>%
  add_header_row(
    values = c("", "Cool Highlands (CP)", "Warm Lowlands (OR)"),
    colwidths = c(1, 4, 4)
  ) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border_outer(border = fp_border(width = 2), part = "all") %>%
  border_inner_h(border = fp_border(width = 1), part = "all") %>%
  border_inner_v(border = fp_border(width = 1), part = "all") %>%
  autofit()

# Save to Word document in current working directory
save_as_docx(ft, path = "BaFTA_coefficients.docx")

# Also print to console to verify
print(coef_table)





## # Create model comparison table
model_comparison <- data.frame(
  Population = rep(c("Cool Highlands", "Warm Lowlands"), each = 3),
  Model = rep(c("Quadratic", "Gamma", "Colchero-Muller"), 2),
  DIC = c(4128.42, 4237.65, 4129.03,  # CP
          2223.60, 2269.45, 2226.09),  # OR
  Predictive_Loss = c(5778, 5894, 5779,  # CP
                      2385, 2396, 2394),  # OR
  Converged = c("No", "Yes", "Yes",  # CP
                "Yes", "Yes", "Yes")   # OR
)

# Save as supplementary table
library(flextable)
ft_models <- flextable(model_comparison) %>%
  set_header_labels(
    Population = "Population",
    Model = "Model",
    DIC = "DIC",
    Predictive_Loss = "Predictive Loss",
    Converged = "Convergence"
  ) %>%
  bold(part = "header") %>%
  autofit()

save_as_docx(ft_models, path = "Supplementary_Table_ModelComparison.docx")
