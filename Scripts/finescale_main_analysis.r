# ------------------------------------------------------------------------------
# Description:  Script to process fine-scale soils, cover, and rainfall
#               simulation data from Grassland Connectivity study in Walnut 
#               Gulch Experimental Watershed (2018).
# Date:         1/6/21
# Author:       Justin Johnson
# ------------------------------------------------------------------------------
# Install and load packages
pkgs <- c('tidyverse','car', 'MASS')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

# ------------------------------------------------------------------------------
# Inputs
filename <- "Data/rg47_finescale_main.csv"

timefilename <- "Data/rg47_finescale_rainfall_time_series.csv"

# ------------------------------------------------------------------------------
# Set col_types for read_csv
col_typesa <- "fcnfffc"
col_typesb <- paste(rep("n", 82), collapse = "")
col_types <- paste(col_typesa, col_typesb, sep = "")

# Read in fine-scale data
fine <- read_csv(filename, col_type = col_types)

# Add a column for all four combinations of microsites and treatments
fine <- fine %>%
  unite(micro, treatment, microsite, remove = FALSE) %>%
  mutate(micro = factor(micro))

# ------------------------------------------------------------------------------
# Create functions to check assumptions, run ANOVA, and Kruskal-Wallis

# Check for normality and heteroscedasticity
diagnose <- function(response) {
  model <- lm(as.formula(paste(response, "~ micro")), data = fine)
  residuals <- residuals(object = model)
  shapiro <- shapiro.test(residuals)
  levene <- leveneTest(model)
  plots <- plot(model)
  return_list <- list(shapiro = shapiro, levene = levene, plots = plots)
  return(return_list)
}

# Run ANOVA
anovate <- function(response) {
  anova <- Anova(aov(lm(as.formula(paste(response, "~ micro")), data = fine)), type = "III")
  means <- fine %>%
    group_by(micro) %>%
    summarize(mean = mean(!!as.name(response), na.rm = TRUE),
              se = sd(!!as.name(response), na.rm = TRUE)/sqrt(n()))
  return_list <- list(means = means, anova = anova)
  return(return_list)
}

# Run Tukey's HSD
tukey <- function(response) {
  result <- TukeyHSD(aov(lm(as.formula(paste(response, "~ micro")), data = fine)))
  means <- fine %>%
    group_by(micro) %>%
    summarize(mean = mean(!!as.name(response), na.rm = TRUE),
              se = sd(!!as.name(response), na.rm = TRUE)/sqrt(n()))
  return_list <- list(means = means, result = result)
  return(return_list)
}

# Run Kruskal-Wallis Rank Sum Test
kruskalize <- function(response) {
  kruskal <- kruskal.test(as.formula(paste(response, "~ micro")), data = fine)
  means <- fine %>%
    group_by(micro) %>%
    summarize(mean = mean(!!as.name(response), na.rm = TRUE),
              se = sd(!!as.name(response), na.rm = TRUE)/sqrt(n()))
  return_list <- list(means = means, kruskal = kruskal)
  return(return_list)
}

# Run Mann-Whitney U test
wilcoxidize <- function(response) {
  wilcox <- pairwise.wilcox.test(fine[[response]], fine$micro, p.adjust.method = "holm")
  means <- fine %>%
    group_by(micro) %>%
    summarize(mean = mean(!!as.name(response), na.rm = TRUE),
              se = sd(!!as.name(response), na.rm = TRUE)/sqrt(n()))
  return_list <- list(means = means, wilcox = wilcox)
  return(return_list)
}

# Summary stats for table
mean_se <- function(response) {
  fine %>%
    group_by(micro) %>%
    summarize(mean = mean(!!as.name(response), na.rm = TRUE),
              se = sd(!!as.name(response), na.rm = TRUE)/sqrt(n()))
}

# ------------------------------------------------------------------------------
# Table 1 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Total canopy cover
diagnose("can_total.pct") # Heterogeneous variance
boxcox(lm(can_total.pct ~ micro, data = fine), lambda=seq(-3,3)) # ^2 transformation
diagnose("can_total.pct^2") # Assumptions satisfied
model <- aov(lm(can_total.pct^2 ~ micro, data = fine))
summary(model) # ANOVA significant
TukeyHSD(model) # Pairwise comparisons
mean_se("can_total.pct")

# Shrub canopy cover
diagnose("can_shrub.pct") # Non-normal, heterogeneous variance
boxcox(lm(can_shrub.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # log transformation
diagnose("log(can_shrub.pct+0.0001)") # Transformation ineffective
kruskalize("can_shrub.pct") # Significant
wilcoxidize("can_shrub.pct") # Pairwise comparisons

# Grass canopy cover
diagnose("can_grass.pct") # Heterogeneous variance
boxcox(lm(can_grass.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # log transformation
diagnose("log(can_grass.pct+0.0001)") # Transformation ineffective
kruskalize("can_grass.pct") # Significant
wilcoxidize("can_grass.pct") # Pairwise comparisons

# ERLE canopy cover
diagnose("can_ERLE.pct") # Non-normal, heterogeneous variance
boxcox(lm(can_ERLE.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # log transformation
diagnose("log(can_ERLE.pct+0.0001)") # Transformation ineffective
kruskalize("can_ERLE.pct") # Significant
wilcoxidize("can_ERLE.pct") # Pairwise comparisons

# MUPO2 canopy cover
diagnose("can_MUPO2.pct") # Non-normal, heterogeneous variance
boxcox(lm(can_MUPO2.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # log transformation
diagnose("log(can_MUPO2.pct+0.0001)") # Transformation ineffective
kruskalize("can_MUPO2.pct") # Significant
wilcoxidize("can_MUPO2.pct") # Pairwise comparisons

# Basal cover
diagnose("grd_basal.pct") # Non-normal, heterogeneous variance
boxcox(lm(grd_basal.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # log transformation
diagnose("log(grd_basal.pct+0.0001)") # Transformation ineffective
kruskalize("grd_basal.pct") # Significant
wilcoxidize("grd_basal.pct") # Pairwise comparisons

# Ground litter cover
diagnose("grd_litter.pct") # Non-normal
boxcox(lm(grd_litter.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # log transformation
diagnose("log(grd_litter.pct+0.0001)") # Transformation effective
model <- aov(lm(log(grd_litter.pct + 0.0001) ~ micro, data = fine))
summary(model) # ANOVA significant
##################     TukeyHSD(model) # Pairwise comparisons
mean_se("grd_litter.pct")

# Bare soil
diagnose("grd_bare_soil.pct") # Assumptions met
anovate("grd_bare_soil.pct") # Not significant

# Bare ground
diagnose("grd_bare_ground.pct") # Non-normal, heterogeneous variance
boxcox(lm(grd_bare_ground.pct+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # ^0.5 transformation
diagnose("(grd_bare_ground.pct+0.0001)^0.5") # Transformation effective
model <- aov(lm((grd_bare_ground.pct + 0.0001)^0.5 ~ micro, data = fine))
summary(model) # ANOVA significant
TukeyHSD(model) # Pairwise comparisons
mean_se("grd_bare_ground.pct")

# Cryptogram cover was not observed within any of the microsites.

# Litter depth
diagnose("litter_depth.cm") # Non-normal, heterogeneous variance
boxcox(lm(litter_depth.cm+0.0001 ~ micro, data = fine), lambda=seq(-3,3))# ^0.5 transformation
diagnose("(litter_depth.cm+0.0001)^0.5") # Transformation effective
model <- aov(lm((litter_depth.cm + 0.0001)^0.5 ~ micro, data = fine))
summary(model) # ANOVA significant
TukeyHSD(model) # Pairwise comparisons
mean_se("litter_depth.cm")

# Aggregate stability
diagnose("aggregate_stability") # heterogeneous variance
boxcox(lm(aggregate_stability ~ micro, data = fine), lambda=seq(-3,6))# ^3 transformation
diagnose("aggregate_stability^3") # Transformation effective
model <- aov(lm(aggregate_stability^3 ~ micro, data = fine))
summary(model) # ANOVA significant
TukeyHSD(model) # Pairwise comparisons
mean_se("aggregate_stability")

# Surface roughness
diagnose("surface_roughness.cm") # Assumptions met
anovate("surface_roughness.cm") # Not significant

# Slope
diagnose("slope.pct") # Heterogeneous variance
boxcox(lm(slope.pct ~ micro, data = fine), lambda=seq(-3,3))# log transformation
diagnose("log(slope.pct)") # Transformation ineffective
kruskalize("slope.pct") # Significant
wilcoxidize("slope.pct") # Pairwise comparisons

# ------------------------------------------------------------------------------
# Table 3 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 64 mm/hr rainfall simulations had limited hydrologic response, so only 
# selected variables are tested.

# Time to runoff
kruskalize("runoff_start_64mmhr.min") # Not significant

# Mean sediment concentration
kruskalize("Sed_mean_64mmhr.gL") # Not significant

#-------------------------------------------------------------------------------
# 100 mm/hr rainfall simulations
# Remove 

fine$Sed_cum_100mmhr.g
# Cumulative runoff
diagnose("Runoff_cum_100mmhr.mm") # Non-normal
boxcox(lm(Runoff_cum_100mmhr.mm+0.0001 ~ micro, data = fine), lambda=seq(-3,3))# ^0.5 transformation
diagnose("(Runoff_cum_100mmhr.mm+0.0001)^0.5") # Transformation ineffective
kruskalize("Runoff_cum_100mmhr.mm") # Significant
wilcoxidize("Runoff_cum_100mmhr.mm")

# Runoff to rainfall ratio
diagnose("Runoff_cum_100mmhr.mm/(rainfall_intensity_100mmhr.mmhr*0.75)")
boxcox(lm(Runoff_cum_100mmhr.mm/(rainfall_intensity_100mmhr.mmhr*0.75)+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # ^0.5 transformation
diagnose("(Runoff_cum_100mmhr.mm/(rainfall_intensity_100mmhr.mmhr*0.75))^0.5") # Transformation ineffective
kruskalize("Runoff_cum_100mmhr.mm/(rainfall_intensity_100mmhr.mmhr*0.75)")
kruskal.test(Runoff_cum_100mmhr.mm/(rainfall_intensity_100mmhr.mmhr*0.75) ~ micro, data = fine) # Significant
pairwise.wilcox.test(fine$Runoff_cum_100mmhr.mm/(fine$rainfall_intensity_100mmhr.mmhr*0.75), fine$micro, p.adjust.method = "holm")
fine %>%
  mutate(runoff_ratio = Runoff_cum_100mmhr.mm/(rainfall_intensity_100mmhr.mmhr*0.75)) %>%
  group_by(micro) %>%
  summarize(mean = mean(runoff_ratio, na.rm = TRUE), 
            se = sd(runoff_ratio, na.rm = TRUE)/sqrt(n()))

# Time to runoff
diagnose("runoff_start_100mmhr.min") # Assumptions met
anovate("runoff_start_100mmhr.min") # Significant
tukey("runoff_start_100mmhr.min")

# Cumulative sediment
diagnose("Sed_cum_100mmhr.g") # Assumptions met
anovate("Sed_cum_100mmhr.g") # Significant
tukey("Sed_cum_100mmhr.g")

# Sediment concentration
diagnose("Sed_mean_100mmhr.gL") # Assumptions met
anovate("Sed_mean_100mmhr.gL") # Not significant - possible outlier
kruskalize("Sed_mean_100mmhr.gL") # Non-parametric to see if different from ANOVA

#-------------------------------------------------------------------------------
# 120 mm/hr rainfall simulations

# Cumulative runoff
diagnose("Runoff_cum_120mmhr.mm") # Assumptions met
anovate("Runoff_cum_120mmhr.mm") # Significant
tukey("Runoff_cum_120mmhr.mm")

# Runoff to rainfall ratio
diagnose("Runoff_cum_120mmhr.mm/(rainfall_intensity_120mmhr.mmhr*0.75)") # Assumptions met
model <- aov(lm(Runoff_cum_120mmhr.mm/(rainfall_intensity_120mmhr.mmhr*0.75) ~ micro, fine))
summary(model) # Significant
TukeyHSD(aov(lm(Runoff_cum_120mmhr.mm/(rainfall_intensity_120mmhr.mmhr*0.75) ~ micro, data = fine)))
fine %>%
  mutate(runoff_ratio = Runoff_cum_120mmhr.mm/(rainfall_intensity_120mmhr.mmhr*0.75)) %>%
  group_by(micro) %>%
  summarize(mean = mean(runoff_ratio, na.rm = TRUE), 
            se = sd(runoff_ratio, na.rm = TRUE)/sqrt(n()))

# Time to runoff
diagnose("runoff_start_120mmhr.min") # Assumptions met
boxcox(lm(runoff_start_120mmhr.min ~ micro, data = fine), lambda=seq(-3,3)) # ^0.5 transformation
diagnose("runoff_start_120mmhr.min^0.5") # Transformation effective
model <- aov(lm(runoff_start_120mmhr.min^0.5 ~ micro, fine))
summary(model) # Significant
TukeyHSD(model)
mean_se("runoff_start_120mmhr.min")

# Cumulative sediment
diagnose("Sed_cum_120mmhr.g") # Assumptions met
boxcox(lm(Sed_cum_120mmhr.g+0.0001 ~ micro, data = fine), lambda=seq(-3,3)) # ^0.5 transformation
diagnose("Sed_cum_120mmhr.g+0.0001^0.5") # Transformation ineffective
kruskalize("Sed_cum_120mmhr.g") # Significant
wilcoxidize("Sed_cum_120mmhr.g")

# Sediment concentration
diagnose("Sed_mean_120mmhr.gL") # Non-normal
boxcox(lm(Sed_mean_120mmhr.gL ~ micro, data = fine), lambda=seq(-3,3)) # Log transformation
diagnose("log(Sed_mean_120mmhr.gL)") # Assumptions met
model <- aov(lm(log(Sed_mean_120mmhr.gL) ~ micro, fine)) # Significant
summary(model) # Significant
TukeyHSD(model)
mean_se("Sed_mean_120mmhr.gL")

#-------------------------------------------------------------------------------
# Cumulative rainfall simulations

fine_sum <- fine %>%
  rowwise() %>%
  mutate(Runoff_cum.mm = sum(Runoff_cum_64mmhr.mm, 
                             Runoff_cum_100mmhr.mm, 
                             Runoff_cum_120mmhr.mm),
         Sed_cum.g = sum(Sed_cum_64mmhr.g,
                         Sed_cum_100mmhr.g,
                         Sed_cum_120mmhr.g))

# Cumulative runoff
model <- aov(lm(Runoff_cum.mm ~ micro, data = fine_sum))
plot(model) # Assumuptions met
summary(model) # Significant
TukeyHSD(model)
fine_sum %>%
  group_by(micro) %>%
  summarize(mean = mean(Runoff_cum.mm, na.rm = TRUE), 
            se = sd(Runoff_cum.mm, na.rm = TRUE)/sqrt(n()))

# Cumulative sediment
model <- aov(lm(Sed_cum.g ~ micro, data = fine_sum))
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions met
summary(model) # Significant
TukeyHSD(model)
fine_sum %>%
  group_by(micro) %>%
  summarize(mean = mean(Sed_cum.g, na.rm = TRUE), 
            se = sd(Sed_cum.g, na.rm = TRUE)/sqrt(n()))

#-------------------------------------------------------------------------------
# Terminal infiltration rates

# Read in rainfall simulation time series
fine_time <- read_csv(timefilename, col_types = "fffffnfnnnnn")

# Determine terminal infiltration rate
terminal_infiltration <-  fine_time %>%
  group_by(plot_ID, target_rainfall_intensity.mmhr) %>%
  summarize(terminal_infiltration_mmhr = last(Inf.mmhr)) %>%
  spread(target_rainfall_intensity.mmhr, terminal_infiltration_mmhr) %>%
  rename(`Inf_terminal_64mmhr.mmhr`="64",
         `Inf_terminal_100mmhr.mmhr`="100",
         `Inf_terminal_120mmhr.mmhr`="120")

# Join data sets
fine_full <- full_join(fine, terminal_infiltration, by = "plot_ID")

# 64 mm/hr terminal infiltration rate
model <- aov(lm(Inf_terminal_64mmhr.mmhr ~ micro, data = fine_full))
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals) # Non-normal
leveneTest(model) # Homogeneous variance
boxcox(model, lambda=seq(-3,10)) # ^9 transformation
model <- aov(lm(Inf_terminal_64mmhr.mmhr^9 ~ micro, data = fine_full))
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions met
summary(model) # Not significant
fine_full %>%
  group_by(micro) %>%
  summarize(mean = mean(Inf_terminal_64mmhr.mmhr, na.rm = TRUE), 
            se = sd(Inf_terminal_64mmhr.mmhr, na.rm = TRUE)/sqrt(n()))

# 100 mm/hr terminal infiltration rate
model <- aov(lm(Inf_terminal_100mmhr.mmhr ~ micro, data = fine_full))
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions met
summary(model) # Significant
TukeyHSD(model)

# 120 mm/hr terminal infiltration rate
model <- aov(lm(Inf_terminal_120mmhr.mmhr ~ micro, data = fine_full))
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions met
summary(model) # Significant
TukeyHSD(model)

# ------------------------------------------------------------------------------
# Figure 5 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Panel a
fine_canopy <- fine %>%
  filter(microsite == "Canopy")

model <- lm(Runoff_cum_120mmhr.mm ~ can_grass.pct, data = fine_canopy)
summary(model)

fine_interspace <- fine %>%
  filter(microsite == "Interspace")

model <- lm(Runoff_cum_120mmhr.mm ~ can_grass.pct, data = fine_interspace)
summary(model)

# Panel b
#Gather all intensities for cumulative runoff
fine_runoff <- gather(fine, key="rainfall_intensity.mmhr", value="Runoff_cum.mm", 
                     Runoff_cum_64mmhr.mm, 
                     Runoff_cum_100mmhr.mm, 
                     Runoff_cum_120mmhr.mm) %>%
  dplyr::select(plot_ID, rainfall_intensity.mmhr, Runoff_cum.mm) %>%
  mutate(rainfall_intensity.mmhr=recode_factor(rainfall_intensity.mmhr, 
                                 Runoff_cum_64mmhr.mm ="64", 
                                 Runoff_cum_100mmhr.mm ="100",
                                 Runoff_cum_120mmhr.mm ="120"))

#Gather all intensities for cumulative sediment
fine_sed <- gather(fine, key="rainfall_intensity.mmhr", value="Sed_cum.g", 
                   Sed_cum_64mmhr.g, 
                   Sed_cum_100mmhr.g, 
                   Sed_cum_120mmhr.g) %>%
  dplyr::select(plot_ID, rainfall_intensity.mmhr, Sed_cum.g) %>%
  mutate(rainfall_intensity.mmhr=recode_factor(rainfall_intensity.mmhr, 
                                               Sed_cum_64mmhr.g ="64", 
                                               Sed_cum_100mmhr.g ="100",
                                               Sed_cum_120mmhr.g ="120"))

#Join combined datasets
fine_all <- full_join(fine_runoff, fine_sed, by=c("plot_ID", "rainfall_intensity.mmhr"))

model <- lm(Sed_cum.g ~ Runoff_cum.mm, data = fine_all)
summary(model)

# ------------------------------------------------------------------------------
# Supplemental Figure 1 --------------------------------------------------------
# ------------------------------------------------------------------------------

# Gather wetted areas.
wet <- fine %>%
  gather(key="depth.cm", value="wetted.pct", wet_2cm.pct:wet_20cm.pct) %>%
  mutate(depth.cm=recode_factor(depth.cm, wet_2cm.pct="2",
                                wet_4cm.pct="4",
                                wet_6cm.pct="6",
                                wet_8cm.pct="8",
                                wet_10cm.pct="10",
                                wet_12cm.pct="12",
                                wet_14cm.pct="14",
                                wet_16cm.pct="16",
                                wet_18cm.pct="18",
                                wet_20cm.pct="20"))%>%
  dplyr::select(plot_ID, micro, depth.cm, wetted.pct)

#Gather rock distribution.
rock <- fine %>%
  gather(key="depth.cm", value="rock.pct", rock_2cm.pct:rock_20cm.pct)%>%
  mutate(depth.cm=recode_factor(depth.cm, rock_2cm.pct="2",
                                rock_4cm.pct="4",
                                rock_6cm.pct="6",
                                rock_8cm.pct="8",
                                rock_10cm.pct="10",
                                rock_12cm.pct="12",
                                rock_14cm.pct="14",
                                rock_16cm.pct="16",
                                rock_18cm.pct="18",
                                rock_20cm.pct="20"))%>%
  dplyr::select(plot_ID, depth.cm, rock.pct)

#Calculate the percent of soil that was available to be wetted (deal with rocks).
soil_map <- full_join(wet,rock, by=c("plot_ID", "depth.cm"))%>%
  mutate(wet.pct=wetted.pct/(100-rock.pct))

# 2 cm 
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "2"),]) # All 100%

# 4 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "4"),]) # All 100%

# 6 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "6"),]) # All 100%

# 8 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "8"),]) # All 100%

# 10 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "10"),]) # Not significant

# 12 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "12"),]) # Not significant

# 14 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "14"),]) # Not significant

# 16 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "16"),]) # Not significant

# 18 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "18"),]) # Not significant

# 20 cm
kruskal.test(wet.pct ~ micro, data = soil_map[which(soil_map$depth.cm == "20"),]) # Not significant
