# ------------------------------------------------------------------------------
# Description:  Script to process coarse-scale cover, topography, and
#               concentrated flow simulation data from Grassland Connectivity 
#               study in Walnut Gulch Experimental Watershed (2018).
# Date:         1/6/21
# Author:       Justin Johnson
# ------------------------------------------------------------------------------
# Install and load packages
pkgs <- c('tidyverse','car', 'MASS', 'haven')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

# ------------------------------------------------------------------------------
# Inputs
filename <- "Data/rg47_coarsescale_main.csv"

# ------------------------------------------------------------------------------
# Set col_types for read_csv
col_typesa <- "fcnfff"
col_typesb <- paste(rep("n", 88), collapse = "")
col_typesc <- "l"
col_typesd <- paste(rep("n", 48), collapse = "")
col_types <- paste(col_typesa, col_typesb, col_typesc, col_typesd, sep = "")

# Read in fine-scale data
coarse <- read_csv(filename, col_type = col_types)

# ------------------------------------------------------------------------------
# Create functions to check assumptions

# Check for normality and heteroscedasticity
diagnose <- function(response) {
  model <- lm(as.formula(paste(response, "~ treatment")), data = coarse)
  residuals <- residuals(object = model)
  shapiro <- shapiro.test(residuals)
  levene <- leveneTest(model)
  plots <- plot(model)
  return_list <- list(shapiro = shapiro, levene = levene, plots = plots)
  return(return_list)
}

# Summary stats for table
mean_se <- function(response) {
  coarse %>%
    group_by(treatment) %>%
    summarize(mean = mean(!!as.name(response), na.rm = TRUE),
              se = sd(!!as.name(response), na.rm = TRUE)/sqrt(n()))
}

# ------------------------------------------------------------------------------
# Table 2 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Total canopy cover
diagnose("can_total.pct") # Assumptions met
t.test(can_total.pct ~ treatment, data = coarse) # Significant
mean_se("can_total.pct")

# Shrub canopy cover
diagnose("can_shrub.pct") # Assumptions not met
wilcox.test(can_total.pct ~ treatment, data = coarse) # Significant
mean_se("can_shrub.pct")

# Grass canopy cover
diagnose("can_grass.pct") # Non-normal
boxcox(lm(can_grass.pct ~ treatment, data = coarse), lambda=seq(-3,3)) # ^1 recommended
wilcox.test(can_grass.pct ~ treatment, data = coarse) # Significant
mean_se("can_grass.pct")

# ERLE canopy cover
diagnose("can_ERLE.pct") # Assumptions not met
wilcox.test(can_ERLE.pct ~ treatment, data = coarse) # Significant
mean_se("can_ERLE.pct")

# MUPO2 canopy cover
diagnose("can_MUPO2.pct") # Non-normal
boxcox(lm(can_MUPO2.pct+0.0001 ~ treatment, data = coarse), lambda=seq(-3,3)) # ^0.5 recommended
diagnose("can_MUPO2.pct+0.0001") # Assumptions not met
wilcox.test(can_MUPO2.pct ~ treatment, data = coarse) # Not significant
mean_se("can_MUPO2.pct")

# Basal cover
diagnose("grd_basal.pct") # Assumptions met
t.test(grd_basal.pct ~ treatment, data = coarse) # Significant
mean_se("grd_basal.pct")

# Litter ground cover
diagnose("grd_litter.pct") # Assumptions met
t.test(grd_litter.pct ~ treatment, data = coarse) # Significant
mean_se("grd_litter.pct")

# Rock cover
diagnose("grd_rock.pct") # Assumptions met
t.test(grd_rock.pct ~ treatment, data = coarse) # Significant
mean_se("grd_rock.pct")

# Bare soil
diagnose("grd_bare_soil.pct") # Assumptions met
t.test(grd_bare_soil.pct ~ treatment, data = coarse) # Not significant
mean_se("grd_bare_soil.pct")

# Bare ground
diagnose("grd_bare_ground.pct") # Assumptions met
t.test(grd_bare_ground.pct ~ treatment, data = coarse) # Significant
mean_se("grd_bare_ground.pct")

# Cryptogam cover was measured but not observed within plots on either study site

# Surface roughness
diagnose("surface_roughness.cm") # Assumptions met
t.test(surface_roughness.cm ~ treatment, data = coarse) # Not significant
mean_se("surface_roughness.cm")

# Slope
diagnose("slope.pct") # Assumptions met
t.test(slope.pct ~ treatment, data = coarse) # Not significant
mean_se("slope.pct")

# ------------------------------------------------------------------------------
# Table 4 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# 15 L/min ---------------------------------------------------------------------

# Cumulative runoff
diagnose("Runoff_cum_15Lmin.L")
t.test(Runoff_cum_15Lmin.L ~ treatment, data = coarse) # Not significant
mean_se("Runoff_cum_15Lmin.L")

# Cumulative sediment yield
diagnose("Sed_cum_15Lmin.g") # Assumptions not met
boxcox(lm(Sed_cum_15Lmin.g ~ treatment, data = coarse), lambda=seq(-3,3)) # log recommended
diagnose("log(Sed_cum_15Lmin.g)") # Assumptions met
t.test(log(Sed_cum_15Lmin.g) ~ treatment, data = coarse) # Not significant
mean_se("Sed_cum_15Lmin.g")

# Sediment concentration
diagnose("Sed_mean_15Lmin.gL") # Assumptions met
t.test(Sed_mean_15Lmin.gL ~ treatment, data = coarse) # Not significant
mean_se("Sed_mean_15Lmin.gL")

# Velocity
diagnose("velo_mean_1_3m_15Lmin.msec") # Assumptions met
t.test(velo_mean_1_3m_15Lmin.msec ~ treatment, data = coarse) # Not significant
mean_se("velo_mean_1_3m_15Lmin.msec")

# Total rill area width
diagnose("total_rill_area_width_3m_15Lmin.cm") # Assumptions met
t.test(total_rill_area_width_3m_15Lmin.cm ~ treatment, data = coarse) # Not significant
mean_se("total_rill_area_width_3m_15Lmin.cm")

# Depths
diagnose("flow_depth_mean_3m_15Lmin.cm") # Assumptions met
t.test(flow_depth_mean_3m_15Lmin.cm ~ treatment, data = coarse) # Not significant
mean_se("flow_depth_mean_3m_15Lmin.cm")

# 30 L/min ---------------------------------------------------------------------

# Cumulative runoff
diagnose("Runoff_cum_30Lmin.L") # Heterogeneous variance
boxcox(lm(Runoff_cum_30Lmin.L ~ treatment, data = coarse), lambda=seq(-3,3)) # log recommended
diagnose("log(Runoff_cum_30Lmin.L)") # Assumptions met
t.test(log(Runoff_cum_30Lmin.L) ~ treatment, data = coarse) # Not significant
mean_se("Runoff_cum_30Lmin.L")

# Cumulative sediment yield
diagnose("Sed_cum_30Lmin.g") # Assumptions not met
boxcox(lm(Sed_cum_30Lmin.g ~ treatment, data = coarse), lambda=seq(-3,3)) # log recommended
diagnose("log(Sed_cum_30Lmin.g)") # Assumptions met
t.test(log(Sed_cum_30Lmin.g) ~ treatment, data = coarse) # Significant
mean_se("Sed_cum_30Lmin.g")

# Sediment concentration
diagnose("Sed_mean_30Lmin.gL") # Assumptions met
t.test(Sed_mean_30Lmin.gL ~ treatment, data = coarse) # Significant
mean_se("Sed_mean_30Lmin.gL")

# Velocity
diagnose("velo_mean_1_3m_30Lmin.msec") # Assumptions met
t.test(velo_mean_1_3m_30Lmin.msec ~ treatment, data = coarse) # Not significant
mean_se("velo_mean_1_3m_30Lmin.msec")

# Total rill area width
diagnose("total_rill_area_width_3m_30Lmin.cm") # Assumptions met
t.test(total_rill_area_width_3m_30Lmin.cm ~ treatment, data = coarse) # Not significant
mean_se("total_rill_area_width_3m_30Lmin.cm")

# Depths
diagnose("flow_depth_mean_3m_30Lmin.cm") # Assumptions met
t.test(flow_depth_mean_3m_30Lmin.cm ~ treatment, data = coarse) # Not significant
mean_se("flow_depth_mean_3m_30Lmin.cm")

# 40 L/min ---------------------------------------------------------------------

# Cumulative runoff
diagnose("Runoff_cum_40Lmin.L") # Assumptions met
t.test(log(Runoff_cum_40Lmin.L) ~ treatment, data = coarse) # Not significant
mean_se("Runoff_cum_40Lmin.L")

# Cumulative sediment yield
diagnose("Sed_cum_40Lmin.g") # Assumptions not met
boxcox(lm(Sed_cum_40Lmin.g ~ treatment, data = coarse), lambda=seq(-3,3)) # 1/y^0.5 recommended
diagnose("1/(Sed_cum_40Lmin.g)^0.5") # Assumptions met
t.test(1/(Sed_cum_40Lmin.g)^0.5 ~ treatment, data = coarse) # Significant
mean_se("Sed_cum_40Lmin.g")

# Sediment concentration
diagnose("Sed_mean_40Lmin.gL") # Assumptions not met
boxcox(lm(Sed_mean_40Lmin.gL ~ treatment, data = coarse), lambda=seq(-3,3)) # 1/y^0.5 recommended
diagnose("1/(Sed_mean_40Lmin.gL)^0.5") # Assumptions met
t.test(1/(Sed_mean_40Lmin.gL)^0.5 ~ treatment, data = coarse) # Significant
mean_se("Sed_mean_40Lmin.gL")

# Velocity
diagnose("velo_mean_1_3m_40Lmin.msec") # Assumptions met
t.test(velo_mean_1_3m_40Lmin.msec ~ treatment, data = coarse) # Not significant
mean_se("velo_mean_1_3m_40Lmin.msec")

# Total rill area width
diagnose("total_rill_area_width_3m_40Lmin.cm") # Assumptions met
t.test(total_rill_area_width_3m_40Lmin.cm ~ treatment, data = coarse) # Not significant
mean_se("total_rill_area_width_3m_40Lmin.cm")

# Depths
diagnose("flow_depth_mean_3m_40Lmin.cm") # Assumptions met
t.test(flow_depth_mean_3m_40Lmin.cm ~ treatment, data = coarse) # Not significant
mean_se("flow_depth_mean_3m_40Lmin.cm")

# Cumulative runoff and sediment across rates ----------------------------------

# Calculate totals
coarse_all <- coarse %>%
  rowwise() %>%
  mutate(Runoff_cum.L = sum(Runoff_cum_15Lmin.L, Runoff_cum_30Lmin.L, Runoff_cum_40Lmin.L),
         Sed_cum.g = sum(Sed_cum_15Lmin.g, Sed_cum_30Lmin.g, Sed_cum_40Lmin.g))

# Cumulative runoff
model <- lm(Runoff_cum.L ~ treatment, data = coarse_all)
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions met
t.test(Runoff_cum.L ~ treatment, data = coarse_all)
coarse_all %>%
  group_by(treatment) %>%
  summarize(mean = mean(Runoff_cum.L, na.rm = TRUE),
            se = sd(Runoff_cum.L, na.rm = TRUE)/sqrt(n()))

# Cumulative sediment
model <- lm(Sed_cum.g ~ treatment, data = coarse_all)
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions not met
boxcox(model, lambda=seq(-3,3)) # 1/y^0.5 recommended
model <- lm(1/(Sed_cum.g)^0.5 ~ treatment, data = coarse_all)
plot(model)
residuals <- residuals(object = model)
shapiro.test(residuals)
leveneTest(model) # Assumptions met
t.test(1/(Sed_cum.g)^0.5 ~ treatment, data = coarse_all) # Not significant
coarse_all %>%
  group_by(treatment) %>%
  summarize(mean = mean(Sed_cum.g, na.rm = TRUE),
            se = sd(Sed_cum.g, na.rm = TRUE)/sqrt(n()))

# ------------------------------------------------------------------------------
# Figure 3 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

# 25-50 basal gaps
diagnose("basal_gap_25_50cm.pct") # Assumptions met
t.test(basal_gap_25_50cm.pct ~ treatment, data = coarse) # Significant

# 51-100 basal gaps
diagnose("basal_gap_51_100cm.pct") # Assumptions met
t.test(basal_gap_51_100cm.pct ~ treatment, data = coarse) # Significant

# 101-200 basal gaps
diagnose("basal_gap_101_200cm.pct") # Assumptions met
t.test(basal_gap_101_200cm.pct ~ treatment, data = coarse) # Significant

# 201-450 basal gaps
diagnose("basal_gap_201_450cm.pct") # Assumptions met
t.test(basal_gap_201_450cm.pct ~ treatment, data = coarse) # Significant

# ------------------------------------------------------------------------------
# Figure 6 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Panel a
model <- lm(Runoff_cum_40Lmin.L ~ grd_basal.pct, data = coarse)
summary(model)

# Gather all flow rates for cumulative runoff
coarse_runoff <- gather(coarse, key="rate.Lmin", value="Runoff_cum.L", 
                        Runoff_cum_15Lmin.L, 
                        Runoff_cum_30Lmin.L,
                        Runoff_cum_40Lmin.L)%>%
  dplyr::select(plot_ID, treatment, rate.Lmin, Runoff_cum.L)%>%
  mutate(rate.Lmin=recode_factor(rate.Lmin, Runoff_cum_15Lmin.L="15", 
                                 Runoff_cum_30Lmin.L="30",
                                 Runoff_cum_40Lmin.L="40"))

# Gather all flow rates for cumulative sediment
coarse_sed <- gather(coarse, key="rate.Lmin", value="Sed_cum.g", 
                     Sed_cum_15Lmin.g, 
                     Sed_cum_30Lmin.g,
                     Sed_cum_40Lmin.g)%>%
  dplyr::select(plot_ID, rate.Lmin, Sed_cum.g)%>%
  mutate(rate.Lmin=recode_factor(rate.Lmin, Sed_cum_15Lmin.g="15", 
                                 Sed_cum_30Lmin.g="30",
                                 Sed_cum_40Lmin.g="40"))

# Gather all flow rates for velocities
coarse_velo <- gather(coarse, key="rate.Lmin", value="velo_mean_1_3m.msec", 
                     velo_mean_1_3m_15Lmin.msec, 
                     velo_mean_1_3m_30Lmin.msec,
                     velo_mean_1_3m_40Lmin.msec)%>%
  dplyr::select(plot_ID, rate.Lmin, velo_mean_1_3m.msec)%>%
  mutate(rate.Lmin=recode_factor(rate.Lmin, velo_mean_1_3m_15Lmin.msec="15", 
                                 velo_mean_1_3m_30Lmin.msec="30",
                                 velo_mean_1_3m_40Lmin.msec="40"))

#Join combined datasets
coarse_join <- full_join(coarse_runoff, coarse_velo, by=c("plot_ID", "rate.Lmin"))%>%
  full_join(coarse_sed, by=c("plot_ID", "rate.Lmin"))

# Panel b
model <- lm(velo_mean_1_3m.msec ~ Runoff_cum.L, data = coarse_join)
summary(model)

# Panel c
model <- lm(Sed_cum.g ~ velo_mean_1_3m.msec, data = coarse_join)
summary(model)

# ------------------------------------------------------------------------------
# Figure 7 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Panel a
model <- lm(Runoff_cum_40Lmin.L ~ basal_gap_25_50cm.pct, data = coarse)
summary(model)

# Panel b
model <- lm(Runoff_cum_40Lmin.L ~ basal_gap_51_100cm.pct, data = coarse)
summary(model)

# Panel c
model <- lm(Runoff_cum_40Lmin.L ~ basal_gap_101_200cm.pct, data = coarse)
summary(model)

# Panel d
model <- lm(Runoff_cum_40Lmin.L ~ basal_gap_201_450cm.pct, data = coarse)
summary(model)
