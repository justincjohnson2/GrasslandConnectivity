# ------------------------------------------------------------------------------
# Description:  Script to process bulk density data from Grassland Connectivity
#               study in Walnut Gulch Experimental Watershed (2018). Soils were
#               sampled using the displacement methods (Voluvessel).
# Date:         1/5/21
# Author:       Justin Johnson
# ------------------------------------------------------------------------------
# Install and load packages
pkgs <- c('tidyverse','car')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

# ------------------------------------------------------------------------------
# Inputs
filename <- "Data/rg47_finescale_bulk_density.csv"

# ------------------------------------------------------------------------------
# Read in bulk density data
fine_bd <- read_csv(filename, col_types = "ffffn")

# Add a column for all four combinations of microsites and treatments
fine_bd <- fine_bd %>%
  unite(micro, treatment, microsite, remove = FALSE) %>%
  mutate(micro = factor(micro))

# Create linear model
bd_model <- aov(lm(bulk_density.gcm3 ~ micro, data = fine_bd))

# View diagnostic plots
plot(bd_model)

# Test for homogeneity of variance (Levene's Test)
leveneTest(bd_model)

# Test for normality of data
aov_residuals <- residuals(object = bd_model)
shapiro.test(aov_residuals)

# Assumptions of normality and homogeneity of variance are met.
# Perform one-way ANOVA.
summary(bd_model)

# All microsites are statistically comparable
# ------------------------------------------------------------------------------
# Get means and standard error by microsite

bd_summary <- fine_bd %>%
  group_by(micro) %>%
  summarize(mean = mean(bulk_density.gcm3),
            se = sd(bulk_density.gcm3)/sqrt(n())) 
