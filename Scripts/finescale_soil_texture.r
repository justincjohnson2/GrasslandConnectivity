# ------------------------------------------------------------------------------
# Description:  Script to process soil texture data from Grassland Connectivity
#               study in Walnut Gulch Experimental Watershed (2018). Texture
#               was determined using hydrometer method.
# Date:         1/6/21
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
filename <- "Data/rg47_finescale_soil_texture.csv"

# ------------------------------------------------------------------------------
# Read in soil texture data
fine_texture <- read_csv(filename, col_types = "fffffnnnf")

# Add a column for all four combinations of microsites and treatments
fine_texture <- fine_texture %>%
  unite(micro, treatment, microsite, remove = FALSE) %>%
  mutate(micro = factor(micro))

# ------------------------------------------------------------------------------
# Sand analysis
# ------------------------------------------------------------------------------
# Create linear model
sand_model <- aov(lm(sand.pct ~ micro, data = fine_texture))

# View diagnostic plots
plot(sand_model)

# Test for homogeneity of variance (Levene's Test)
leveneTest(sand_model)

# Test for normality of data
aov_residuals <- residuals(object = sand_model)
shapiro.test(aov_residuals)

# Assumptions of normality and homogeneity of variance are met.
# Perform one-way ANOVA.
summary(sand_model)

# All microsites are statistically comparable

# ------------------------------------------------------------------------------
# Silt analysis
# ------------------------------------------------------------------------------
# Assuming normality and homogeneity of variance based on sand results

# Create linear model
silt_model <- aov(lm(silt.pct ~ micro, data = fine_texture))

# Perform one-way ANOVA.
summary(silt_model)

# All microsites are statistically comparable

# ------------------------------------------------------------------------------
# Clay analysis
# ------------------------------------------------------------------------------
# Assuming normality and homogeneity of variance based on sand results

# Create linear model
clay_model <- aov(lm(clay.pct ~ micro, data = fine_texture))

# Perform one-way ANOVA.
summary(clay_model)

# All microsites are statistically comparable

# ------------------------------------------------------------------------------
# Summarize data
# ------------------------------------------------------------------------------
texture_summary <- fine_texture %>%
  summarize(sand = mean(sand.pct),
            silt = mean(silt.pct),
            clay = mean(clay.pct))