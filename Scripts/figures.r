# ------------------------------------------------------------------------------
# Description:  Script to create figures to include in publication of 
#               Grassland Connectivity study in Walnut Gulch Experimental 
#               Watershed (2018).
# Date:         1/6/21
# Author:       Justin Johnson
# ------------------------------------------------------------------------------
# Install and load packages
pkgs <- c('tidyverse','ggpubr', 'grid')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

# ------------------------------------------------------------------------------
# Figure 3 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

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

# Read in coarse-scale data
coarse <- read_csv(filename, col_type = col_types)

#Start by formatting data, calculating mean and standard error, and adding results from t.tests.
basal_gap <- coarse %>%
  dplyr::select(plot_ID, 
         treatment, 
         "25-50"= basal_gap_25_50cm.pct, 
         "51-100"= basal_gap_51_100cm.pct,
         "101-200"= basal_gap_101_200cm.pct,
         "201-450"= basal_gap_201_450cm.pct) %>%
  rename(Treatment = "treatment") %>%
  mutate(Treatment = fct_rev(Treatment)) %>%
  gather("Variable", "Percent", c(-plot_ID, -Treatment)) %>%
  group_by(Treatment, Variable) %>%
  summarize(mean=mean(Percent), 
            standard_error=sd(Percent)/sqrt(length(Percent)),
            low.lim=mean-standard_error,
            high.lim=mean+standard_error)%>%
  add_column(stat_diff=c("a","a","b","b","b","b","a","a"))%>%
  mutate(Variable=fct_relevel(Variable, "25-50","51-100","101-200","201-450"))

#Basal gap figure.
basal_figure <- ggplot(basal_gap, aes(Variable, mean/100, fill=Treatment))+
  geom_col(position=position_dodge())+
  geom_errorbar(aes(ymin=low.lim/100, ymax=high.lim/100), width=.2,
                position=position_dodge(.9),size=1)+
  geom_text(aes(label=stat_diff, y=high.lim/100, vjust=-0.5), position=position_dodge(width=.9), size=8, fontface="bold")+
  theme(
    text = element_text(size=14),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold", size=28),
    plot.subtitle = element_text(size=24),
    axis.text = element_text(size=22),
    axis.title = element_text(face="bold", size=24), #moves y axis title back
    axis.title.y=element_text(angle=90, vjust=2),
    axis.title.x=element_text(vjust=-0.25),
    axis.line = element_line(colour="black", size=1), #add line to bottom and left axis
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), #removes minor grid lines
    legend.key = element_rect(colour = NA), #removes box surrounding colors in key
    legend.position = "bottom", #puts legend on bottom of plot
    legend.direction = "horizontal", #puts legend horizontal
    legend.key.size= unit(0.75, "cm"), #reduces legend size
    legend.text=element_text(size=24),
    legend.title = element_text(face="italic", size=24), #adds italics to legend title
    plot.margin=unit(c(10,5,5,5),"mm"))+ #increases margins around plot
  labs(x="Basal Gap Class (cm)", y="Proportion of Transects")+
  scale_fill_manual(values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,0.5))

#Export figure
ggsave("Figures/Figure_3.png", 
       plot=basal_figure,
       unit="in",
       height=10,
       width=10,
       dpi=300,
       device="png")

# ------------------------------------------------------------------------------
# Figure 4 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Inputs
filename <- "Data/rg47_finescale_rainfall_time_series.csv"

#######################################################################

# Brings in small plot time series and formats variables to fit with old code
small_time <- read_csv(filename, col_types = "fffffnfnnnnn")

small_time <- small_time %>%
  rename(Plot = "plot_ID",
         Rate = "target_rainfall_intensity.mmhr",
         Year = "year",
         Site = "site",
         Treatment = "treatment",
         Microsite_Simplified = "microsite",
         plot_time = "time.sec",
         Inf_mmperhr_30sec = "Inf.mmhr",
         Sed_gpers_30sec = "Sed.gsec",
         Sedcon_gL_30sec = "Sed.gL") %>%
  mutate(plot_time = plot_time/60) %>%
  unite(Microsite, Treatment, Microsite_Simplified, remove = FALSE, sep = " - ")

# Create average hydrographs
small.time.means <- small_time %>%
  group_by(Treatment,Microsite_Simplified, Rate, plot_time) %>%
  summarize(mean_Inf = mean(Inf_mmperhr_30sec))

# Formats rates for faceting.
levels(small.time.means$Rate) <- c("paste(bold('64 mm·h'^{-1}))",
                                   "paste(bold('100 mm·h'^{-1}))", 
                                   "paste(bold('120 mm·h'^{-1}))")

# Create average sedigraphs
small.time.sed.means <- small_time %>%
  group_by(Treatment,Microsite_Simplified, Rate, plot_time) %>%
  summarize(mean_Sed = mean(Sed_gpers_30sec))

# Formats rates for faceting.
levels(small.time.sed.means$Rate) <- c("paste(bold('64 mm·h'^{-1}))",
                                       "paste(bold('100 mm·h'^{-1}))", 
                                       "paste(bold('120 mm·h'^{-1}))")

# Create average sedigraphs
small.time.sedcon.means <- small_time %>%
  group_by(Treatment,Microsite_Simplified, Rate, plot_time) %>%
  summarize(mean_Sed_Con = mean(Sedcon_gL_30sec))

# Formats rates for faceting.
levels(small.time.sedcon.means$Rate) <- c("paste(bold('64 mm·h'^{-1}))",
                                          "paste(bold('100 mm·h'^{-1}))", 
                                          "paste(bold('120 mm·h'^{-1}))")

# Create labels for infiltration facets.
inf_text <- data.frame(
  label = c("a", "b", "c"),
  Rate =  factor(c("paste(bold('64 mm·h'^{-1}))",
                   "paste(bold('100 mm·h'^{-1}))", 
                   "paste(bold('120 mm·h'^{-1}))"), 
                 levels = c("paste(bold('64 mm·h'^{-1}))",
                            "paste(bold('100 mm·h'^{-1}))",
                            "paste(bold('120 mm·h'^{-1}))"))
)

# Create labels for sediment facets.
sed_text <- data.frame(
  label = c("g", "h", "i"),
  Rate =  factor(c("paste(bold('64 mm·h'^{-1}))",
            "paste(bold('100 mm·h'^{-1}))", 
            "paste(bold('120 mm·h'^{-1}))"), 
            levels = c("paste(bold('64 mm·h'^{-1}))",
                       "paste(bold('100 mm·h'^{-1}))",
                       "paste(bold('120 mm·h'^{-1}))"))
)

# Create labels for sediment concentration facets.
sedcon_text <- data.frame(
  label = c("d", "e", "f"),
  Rate =  factor(c("paste(bold('64 mm·h'^{-1}))",
                   "paste(bold('100 mm·h'^{-1}))", 
                   "paste(bold('120 mm·h'^{-1}))"), 
                 levels = c("paste(bold('64 mm·h'^{-1}))",
                            "paste(bold('100 mm·h'^{-1}))",
                            "paste(bold('120 mm·h'^{-1}))"))
)

# Create infiltration plots
infiltration <- ggplot()+
  geom_line(data=small.time.means,aes(plot_time, mean_Inf, color=interaction(Treatment, Microsite_Simplified), 
                                      linetype=interaction(Treatment, Microsite_Simplified)), size=2)+
  facet_wrap(Rate~., 
             labeller=label_parsed, nrow=3)+
  labs(x="", y=bquote(bold('Infiltration rate (mm·'*h^-1*')')))+
  scale_color_manual(name="Microsite", values= rep(c("#fdb462", "#386cb0","#fdb462", "#386cb0"), times = 2), 
                     labels = c("Control - Canopy", "Tebuthiuron - Canopy", "Control - Interspace", "Tebuthiuron - Interspace"))+
  scale_linetype_manual(name="Microsite",values = rep(c(6, 6, 1, 1), times = 2), 
                        labels = c("Control - Canopy", "Tebuthiuron - Canopy", "Control - Interspace", "Tebuthiuron - Interspace"))+
  scale_y_continuous(limits=c(0,140),
                     breaks=c(0,25,50,75,100,125)
  )+
  geom_vline(data=filter(small.time.means, Rate=="paste(bold('100 mm·h'^{-1}))"), aes(xintercept=30), color="grey", linetype="dotted", size=1)+
  geom_vline(data=filter(small.time.means, Rate=="paste(bold('120 mm·h'^{-1}))"), aes(xintercept=25), color="grey", linetype="dotted", size=1)+
  geom_text(data=inf_text, aes(x=1,y=135,label=label), 
            size=8, fontface="bold")+
  theme_bw(base_size=20)+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(0,4,5,4),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        panel.spacing = unit(-4,"mm"),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size =18),
        axis.title = element_text(face="bold", size=22),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=-0.25),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-10,-10,-10,-10),
        legend.key = element_rect(colour = NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom", 
        strip.background=element_blank(),
        strip.text = element_text(size=17, vjust=-1))+
  guides(linetype = guide_legend(nrow=2, reverse = TRUE),
         color= guide_legend(nrow=2, reverse = TRUE))

# Create sedigraph.
sedigraph <- ggplot()+
  geom_line(data=small.time.sed.means,aes(plot_time, mean_Sed, color=interaction(Treatment, Microsite_Simplified), 
                                          linetype=interaction(Treatment, Microsite_Simplified)), size=2)+
  facet_wrap(Rate~., 
             labeller=label_parsed, nrow=3)+
  labs(x="", y=bquote(bold('Sediment discharge (g·'*s^-1*')')))+
  scale_color_manual(name="Microsite", values= rep(c("#fdb462", "#386cb0","#fdb462", "#386cb0"), times = 2), 
                     labels = c("Control - Canopy", "Tebuthiuron - Canopy", "Control - Interspace", "Tebuthiuron - Interspace"))+
  scale_linetype_manual(name="Microsite",values = rep(c(6, 6, 1, 1), times = 2), 
                        labels = c("Control - Canopy", "Tebuthiuron - Canopy", "Control - Interspace", "Tebuthiuron - Interspace"))+
  scale_y_continuous(limits=c(0,NA), position = "left")+
  geom_vline(data=filter(small.time.sed.means, Rate=="paste(bold('100 mm·h'^{-1}))"), aes(xintercept=30), color="grey", linetype="dotted", size=1)+
  geom_vline(data=filter(small.time.sed.means, Rate=="paste(bold('120 mm·h'^{-1}))"), aes(xintercept=25), color="grey", linetype="dotted", size=1)+
  geom_text(data=sed_text, aes(x=1,y=0.01125,label=label), 
            size=8, fontface="bold")+
  theme_bw(base_size=20)+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(0,4,5,0),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        panel.spacing=unit(-4,"mm"),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=18),
        axis.title = element_text(face="bold", size=22),
        axis.title.y=element_text(angle=90, vjust=2, face="bold"),
        axis.title.x=element_text(vjust=-0.25),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-10,-10,-10,-10),
        legend.key = element_rect(colour = NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom", 
        strip.background=element_blank(),
        strip.text = element_text(size=17, vjust=-1))+
  guides(linetype = guide_legend(nrow=2, reverse = TRUE),
         color= guide_legend(nrow=2, reverse = TRUE))

#Create sediment concentration time series figure.
sed_con_figure <- ggplot()+
  geom_line(data=small.time.sedcon.means,aes(plot_time,mean_Sed_Con, color=interaction(Treatment, Microsite_Simplified), 
                                             linetype=interaction(Treatment, Microsite_Simplified)), size=2)+
  facet_wrap(Rate~., 
             labeller=label_parsed, nrow=3)+
  labs(x="Simulation duration (min)", y=bquote(bold('Sediment concentration (g·'*L^-1*')')))+
  scale_color_manual(name="Microsite", values= rep(c("#fdb462", "#386cb0","#fdb462", "#386cb0"), times = 2), 
                     labels = c("Control - Canopy", "Tebuthiuron - Canopy", "Control - Interspace", "Tebuthiuron - Interspace"))+
  scale_linetype_manual(name="Microsite",values = rep(c(6, 6, 1, 1), times = 2), 
                        labels = c("Control - Canopy", "Tebuthiuron - Canopy", "Control - Interspace", "Tebuthiuron - Interspace"))+
  scale_y_continuous(limits=c(0,2))+
  geom_vline(data=filter(small.time.sed.means, Rate=="paste(bold('100 mm·h'^{-1}))"), aes(xintercept=30), color="grey", linetype="dotted", size=1)+
  geom_vline(data=filter(small.time.sed.means, Rate=="paste(bold('120 mm·h'^{-1}))"), aes(xintercept=25), color="grey", linetype="dotted", size=1)+
  theme_bw(base_size=20)+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(0,4,5,4),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        panel.spacing=unit(-4,"mm"),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=18),
        axis.title = element_text(face="bold", size=22),
        axis.title.y=element_text(angle=90, vjust=2, face="bold"),
        axis.title.x=element_text(vjust=-0.25, hjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-10,-10,-10,-10),
        legend.key = element_rect(colour = NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom", 
        strip.background=element_blank(),
        strip.text = element_text(size=17, vjust=-1))+
  geom_text(data=sedcon_text, aes(x=0.75,y=1.85,label=label), 
            size=8, fontface="bold")+
  guides(linetype = guide_legend(nrow=2, reverse = TRUE),
         color= guide_legend(nrow=2, reverse = TRUE))

combined <- ggarrange(infiltration, sed_con_figure, sedigraph,
                      nrow=1, 
                      common.legend=TRUE,
                      legend="bottom")

#Adds space to bottom of plot, so legend isn't cut off.
combined <- annotate_figure(combined, bottom=text_grob(""))

ggsave("Figures/Figure_4.png", 
       plot=combined,
       unit="in",
       height=10,
       width=12,
       dpi=300,
       device="png")

# ------------------------------------------------------------------------------
# Figure 5 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

#Inputs
filename <- "Data/rg47_finescale_main.csv"

# ------------------------------------------------------------------------------
# Set col_types for read_csv
col_typesa <- "fcnfffc"
col_typesb <- paste(rep("n", 82), collapse = "")
col_types <- paste(col_typesa, col_typesb, sep = "")

# Read in fine-scale data
fine <- read_csv(filename, col_type = col_types)

# ------------------------------------------------------------------------------

#Gather all intensities for cumulative runoff
small_cumro <- gather(fine, key="intensity", value="Runoff_cum.mm", 
                      Runoff_cum_64mmhr.mm, Runoff_cum_100mmhr.mm, Runoff_cum_120mmhr.mm)%>%
  dplyr::select(plot_ID, treatment, intensity, microsite, Runoff_cum.mm)%>%
  mutate(intensity=recode_factor(intensity, Runoff_cum_64mmhr.mm ="64 mm/hr", 
                                 Runoff_cum_100mmhr.mm ="100 mm/hr",
                                 Runoff_cum_120mmhr.mm ="120 mm/hr"))

#Gather all intensities for cumulative sediment
small_cumsed <- gather(fine, key="intensity", value="Sed_cum.g", 
                       Sed_cum_64mmhr.g, Sed_cum_100mmhr.g, Sed_cum_120mmhr.g)%>%
  dplyr::select(plot_ID, intensity, Sed_cum.g)%>%
  mutate(intensity=recode_factor(intensity, Sed_cum_64mmhr.g ="64 mm/hr", 
                                 Sed_cum_100mmhr.g ="100 mm/hr",
                                 Sed_cum_120mmhr.g ="120 mm/hr"))

#Join combined datasets
small_all <- full_join(small_cumro, small_cumsed, by=c("plot_ID", "intensity"))

#Create runoff/sediment scatter plot.
runsed <- ggplot(small_all, aes(x=Runoff_cum.mm, y=Sed_cum.g))+
  geom_point(aes(x=Runoff_cum.mm, y=Sed_cum.g, color=interaction(treatment, microsite), 
                 shape=interaction(treatment, microsite)), size=4)+
  geom_smooth(method="lm", color="black", size=2)+
  labs(x="Cumulative Runoff (mm)", y="Cumulative Sediment Yield (g)",
       shape = "Microsite")+
  scale_color_manual(name="Microsite", values= rep(c("#fdb462", "#386cb0","#fdb462","#386cb0"), times = 2), 
                     labels = c("Control - Interspace", "Tebuthiuron - Interspace","Control - Canopy","Tebuthiuron - Canopy"))+
  scale_shape_manual(name="Microsite",values = rep(c(17,17, 16, 16), times = 2), 
                     labels = c("Control - Interspace", "Tebuthiuron - Interspace","Control - Canopy","Tebuthiuron - Canopy"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=20),
        axis.title = element_text(face="bold", size=22),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=1, y=58, label="Sediment = 0.54(Runoff) + 0.34", size=5, fontface="italic", hjust=0)+
  annotate("text", x = 1, y = 53, label = "italic(R ^ 2 == 0.64)", parse=TRUE, size=5, hjust=0)+
  annotate("text", x = 1, y = 48, label = "italic(p < 0.001)", parse=TRUE, size=5, hjust=0)+
  annotate("text", x = 0, y = 68, label = "bold(b)", parse=TRUE, size=8, hjust=0)+
  guides(shape = guide_legend(nrow=4),
         color= guide_legend(nrow=4))

#Create runoff/grass cover scatter plot
grassrun <- ggplot(fine, aes(x=can_grass.pct/100, y=Runoff_cum_120mmhr.mm))+
  geom_point(aes(x=can_grass.pct/100, y=Runoff_cum_120mmhr.mm, color=interaction(treatment, microsite), 
                 shape=interaction(treatment, microsite)), size=4)+
  geom_smooth(method="lm", aes(linetype=microsite), color="black", size=2, show.legend=FALSE)+
  labs(x="Grass Foliar Cover", y="Cumulative Runoff (mm)",
       shape = "Microsite")+
  scale_color_manual(name="Microsite", values= rep(c("#fdb462", "#386cb0","#fdb462","#386cb0"), times = 2), 
                     labels = c("Control - Interspace", "Tebuthiuron - Interspace","Control - Canopy","Tebuthiuron - Canopy"))+
  scale_shape_manual(name="Microsite",values = rep(c(17,17, 16, 16), times = 2), 
                     labels = c("Control - Interspace", "Tebuthiuron - Interspace","Control - Canopy","Tebuthiuron - Canopy"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=20),
        axis.title = element_text(face="bold", size=22),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0.01, y=2, label="Canopy RO = -0.34(Grass) + 36.7", size=5, fontface="italic", hjust=0)+
  annotate(geom="text", x=0.12, y=62, label="Interspace RO = -0.04(Grass) + 48.9", size=5, fontface="italic", hjust=0)+
  annotate("text", x = 0.01, y = 7, label = "italic(R ^ 2 == 0.70)", parse=TRUE, size=5, hjust=0)+
  annotate("text", x = 0.12, y = 58, label = "italic(R ^ 2 == 0.01)", parse=TRUE, size=5, hjust=0)+
  annotate("text", x = 0.73, y = 58, label = "p = 0.60",  fontface="italic", size=5, hjust=0)+
  annotate("text", x = 0.01, y = 12, label = "p < 0.001",  fontface="italic", size=5, hjust=0)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  annotate("text", x = 0, y = 60, label = "bold(a)", parse=TRUE, size=8, hjust=0)+
  guides(shape = guide_legend(nrow=4),
         color= guide_legend(nrow=4))

#Combine plots
combined_small <- ggarrange(grassrun, runsed, 
                            nrow=2, 
                            common.legend=TRUE,
                            legend="bottom")

#Save combined plots
ggsave("Figures/Figure_5.png", 
       plot=combined_small,
       unit="in",
       height=12,
       width=6,
       dpi=300,
       device="png")

# ------------------------------------------------------------------------------
# Figure 6 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

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

#Create runoff and basal cover scatter plot.
basal_cumro <- ggplot(coarse,aes(x=grd_basal.pct/100, y=Runoff_cum_40Lmin.L, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color="black", size=2, show.legend=FALSE)+
  labs(x="Basal Cover", 
       y="Cumulative Runoff (L)",
       color = "Treatment")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0.17, y=270, label="Runoff = -4.54(Basal) + 236", size=7, fontface="italic", hjust=1)+
  annotate("text", x = 0.17, y = 258, label = "italic(R ^ 2) == 0.37", parse=TRUE, size=7, hjust=1)+
  annotate("text", x = 0.17, y= 246, label = "p = 0.034", size=7, fontface="italic", hjust=1)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  annotate("text", x = 0, y = 300, label = "bold(a)", parse=TRUE, size=12, hjust=0)

#Gather all flow rates for cumulative runoff
rill_cumro <- gather(coarse, key="flow_rate", value = "Runoff_cum.L", 
                     Runoff_cum_15Lmin.L, 
                     Runoff_cum_30Lmin.L, 
                     Runoff_cum_40Lmin.L)%>%
  dplyr::select(plot_ID, treatment, flow_rate, Runoff_cum.L)%>%
  mutate(flow_rate=recode_factor(flow_rate, Runoff_cum_15Lmin.L = "15 L/min", 
                                 Runoff_cum_30Lmin.L = "30 L/min",
                                 Runoff_cum_40Lmin.L = "40 L/min"))

#Gather all flow rates for velocities
rill_vel <- gather(coarse, key="flow_rate", value="velo_mean_1_3m.msec", 
                   velo_mean_1_3m_15Lmin.msec, 
                   velo_mean_1_3m_30Lmin.msec, 
                   velo_mean_1_3m_40Lmin.msec)%>%
  dplyr::select(plot_ID, flow_rate, velo_mean_1_3m.msec)%>%
  mutate(flow_rate=recode_factor(flow_rate, velo_mean_1_3m_15Lmin.msec="15 L/min", 
                                 velo_mean_1_3m_30Lmin.msec="30 L/min",
                                 velo_mean_1_3m_40Lmin.msec="40 L/min"))

#Gather all flow rates for cumulative sediment
rill_sed <- gather(coarse, key="flow_rate", value="Sed_cum.g", 
                   Sed_cum_15Lmin.g, 
                   Sed_cum_30Lmin.g, 
                   Sed_cum_40Lmin.g)%>%
  dplyr::select(plot_ID, flow_rate, Sed_cum.g)%>%
  mutate(flow_rate=recode_factor(flow_rate, Sed_cum_15Lmin.g="15 L/min", 
                                 Sed_cum_30Lmin.g="30 L/min",
                                 Sed_cum_40Lmin.g="40 L/min"))

#Join combined datasets
rill_all <- full_join(rill_cumro, rill_vel, by=c("plot_ID", "flow_rate"))%>%
  full_join(rill_sed, by=c("plot_ID", "flow_rate"))

#Create runoff and velocity scatter plot.
cumro_velo <- ggplot(rill_all, aes(x=Runoff_cum.L, y=velo_mean_1_3m.msec, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color="black", size=2, show.legend=FALSE)+
  labs(x="Cumulative Runoff (L)", 
       y=bquote(bold("Flow Velocity (m·"*s^-1*")")),
       color = "Treatment")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=10, y=0.16, label="Velocity = 0.00035(Runoff) + 0.04", size=7, fontface="italic", hjust=0)+
  annotate("text", x = 10, y = 0.15, label = "italic(R ^ 2) == 0.72", parse=TRUE, size=7, hjust=0)+
  annotate("text", x = 10, y= 0.14, label = "p < 0.001", size=7, fontface="italic", hjust=0)+
  annotate("text", x = 1, y = 0.18, label = "bold(b)", parse=TRUE, size=12, hjust=0)


#Create velocity and sediment scatter plot.
velo_sed <- ggplot(rill_all, aes(x=velo_mean_1_3m.msec, y=Sed_cum.g, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color= "black", size=2, show.legend=FALSE)+
  labs(y="Cumulative Sediment (g)", 
       x=bquote(bold("Flow Velocity (m·"*s^-1*")")),
       color = "Treatment")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0.04, y=300, label="Sediment = 1802(Velocity) - 81.17", size=7, fontface="italic", hjust=0)+
  annotate("text", x = 0.04, y = 270, label = "italic(R ^ 2) == 0.63", parse=TRUE, size=7, hjust=0)+
  annotate("text", x = 0.04, y= 240, label = "p < 0.001", size=7, fontface="italic", hjust=0)+
  annotate("text", x = 0.036, y = 410, label = "bold(c)", parse=TRUE, size=12, hjust=0)

#Combine plots
combined_rill <- ggarrange(basal_cumro, cumro_velo, velo_sed,
                           nrow=1, 
                           common.legend=TRUE,
                           legend="bottom")

#Save combined plots
ggsave("Figures/Figure_6.png", 
       plot=combined_rill,
       unit="in",
       height=7,
       width=20,
       dpi=300,
       device="png")

# ------------------------------------------------------------------------------
# Figure 7 ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

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

#Create runoff and 25-50 basal gap class scatter plot.
gap50_cumro <- ggplot(coarse, aes(x=basal_gap_25_50cm.pct/100, y=Runoff_cum_40Lmin.L, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color="black", size=2, show.legend=FALSE)+
  labs(x="% 25-50 cm Basal Gaps", 
       y="Cumulative Runoff (L)",
       color = "Treatment")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0.2, y=260, label="Runoff = -3.43(% Gaps) + 240", size=7, fontface="italic", hjust=1)+
  annotate("text", x = 0.2, y = 240, label = "italic(R ^ 2) == 0.28", parse=TRUE, size=7, hjust=1)+
  annotate("text", x = 0.02, y = 150, label="p = 0.078", size=7, fontface="italic", hjust=0)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  annotate("text", x = 0, y = 300, label = "bold(a)", parse=TRUE, size=12, hjust=0)

#Create runoff and 51-100 cm basal gap class scatter plot.
gap100_cumro <- ggplot(coarse, aes(x=basal_gap_51_100cm.pct/100, y=Runoff_cum_40Lmin.L, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color="black", size=2, show.legend=FALSE)+
  labs(x="% 51-100 cm Basal Gaps", 
       y="Cumulative Runoff (L)",
       color = "Treatment")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0.4, y=270, label="Runoff = -3.35(% Gaps) + 295", size=7, fontface="italic", hjust=1)+
  annotate("text", x = 0.4, y = 250, label = "italic(R ^ 2) == 0.51", parse=TRUE, size=7, hjust=1)+
  annotate("text", x = 0.18, y = 150, label="p = 0.009", size=7, fontface="italic", hjust=0)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0.15,0.42), breaks=c(0.2,0.3,0.4))+
  annotate("text", x = 0.15, y = 300, label = "bold(b)", parse=TRUE, size=12, hjust=0)

#Create runoff and 101-200 cm basal gap class scatter plot.
gap200_cumro <- ggplot(coarse, aes(x=basal_gap_101_200cm.pct/100, y=Runoff_cum_40Lmin.L, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color="black", size=2, show.legend=FALSE)+
  labs(x="% 101-200 cm Basal Gaps", 
       y="Cumulative Runoff (L)",
       color = "Treatment")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0, y=265, label="Runoff = 0.82(% Gaps) + 185", size=7, fontface="italic", hjust=0)+
  annotate("text", x = 0, y = 250, label = "italic(R ^ 2) == 0.12", parse=TRUE, size=7, hjust=0)+
  annotate("text", x = 0.55, y = 150, label = "italic(p) == 0.277", parse=TRUE, size=7, hjust=1, fontface="bold")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  annotate("text", x = 0, y = 300, label = "bold(c)", parse=TRUE, size=12, hjust=0)

#Create runoff and 201-450 cm basal gap class scatter plot.
gap450_cumro <- ggplot(coarse, aes(x=basal_gap_201_450cm.pct/100, y=Runoff_cum_40Lmin.L, color=treatment))+
  geom_point(size=6)+
  geom_smooth(method="lm", color="black", size=2, show.legend=FALSE)+
  labs(x="% 201-450 cm Basal Gaps", 
       y="Cumulative Runoff (L)",
       color = "Tebuthiuron")+
  scale_color_manual(limits=c("Control", "Tebuthiuron"),
                     values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA, color=NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom")+
  annotate(geom="text", x=0, y=275, label="Runoff = 1.68(% Gaps) + 183", size=7, fontface="italic", hjust=0)+
  annotate("text", x = 0, y = 260, label = "italic(R ^ 2) == 0.52", parse=TRUE, size=7, hjust=0)+
  annotate("text", x = 0.35, y = 180, label = "italic(p) == 0.008", parse=TRUE, size=7, hjust=0, fontface="bold")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  annotate("text", x = 0, y = 300, label = "bold(d)", parse=TRUE, size=12, hjust=0)

#Combine plots
combined_gaps <- ggarrange(gap50_cumro, gap100_cumro, gap200_cumro, gap450_cumro,
                           nrow=4, 
                           common.legend=TRUE,
                           legend="bottom")

#Save combined plots
ggsave("Figures/Figure_7.png", 
       plot=combined_gaps,
       unit="in",
       height=20,
       width=7,
       dpi=300,
       device="png")

# ------------------------------------------------------------------------------
# Supplemental Figure 1 --------------------------------------------------------
# ------------------------------------------------------------------------------

# Inputs
filename <- "Data/rg47_finescale_main.csv"

# ------------------------------------------------------------------------------
# Set col_types for read_csv
col_typesa <- "fcnfffc"
col_typesb <- paste(rep("n", 82), collapse = "")
col_types <- paste(col_typesa, col_typesb, sep = "")

# Read in fine-scale data
fine <- read_csv(filename, col_type = col_types)

# ------------------------------------------------------------------------------
fine$wet_2cm.pct
#Gather wetted areas.
wet <- fine %>%
  gather(key="depth.cm", value="Wetted", wet_2cm.pct:wet_20cm.pct) %>%
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
  dplyr::select(plot_ID, microsite, treatment, depth.cm, Wetted)

#Gather rock distribution.
rock <- fine %>%
  gather(key="depth.cm", value="Rock", rock_2cm.pct:rock_20cm.pct)%>%
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
  dplyr::select(plot_ID, depth.cm, Rock)

#Calculate the percent of soil that was available to be wetted (deal with rocks).
soil_map <- full_join(wet,rock, by=c("plot_ID", "depth.cm"))%>%
  mutate(Wet=Wetted/(100-Rock))

#Create bar chart of soil depth. 
soil_pattern <- ggplot(soil_map, aes(fct_rev(depth.cm), Wet, fill=fct_rev(treatment)))+
  geom_bar(position = "dodge", stat = "summary", fun = "mean", width = 0.75)+
  coord_flip()+
  facet_wrap(~rev(microsite))+
  scale_fill_manual(values = c("Control" = "#fdb462", "Tebuthiuron" = "#386cb0"))+
  labs(x="Depth (cm)", y="Proportion of Soil Wetted", fill="Treatment")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(10,5,5,5),"mm"),
        panel.grid = element_line(color="lightgray"),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size=20),
        axis.title = element_text(face="bold", size=22),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=1),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-5,-5,-5,-5),
        legend.key = element_rect(fill = NA),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom", 
        strip.background=element_blank(),
        strip.text = element_text(size=22))

#Save combined plots
ggsave("Figures/Supplemental_Figure_1.png", 
       plot=soil_pattern,
       unit="in",
       height=5,
       width=15,
       dpi=300,
       device="png")

# ------------------------------------------------------------------------------
# Supplemental Figure 2 --------------------------------------------------------
# ------------------------------------------------------------------------------

# Inputs
RHEM_control <- "Scripts/RHEM/Control/Simulation_120mmhr/CONTROL_120MMHR.csv"
RHEM_tebuthiuron <- "Scripts/RHEM/Tebuthiuron/Simulation_120mmhr/TEBUTHIURON_120MMHR.csv"

# ------------------------------------------------------------------------------

#Bring in control RHEM dataset
control <- read_csv(RHEM_control, 
                      skip=2, 
                      col_types="dddddddddd", 
                      col_names=c("Time.min", "Precip.mmhr", "Q.mmhr", "Q.m3s", "Sed.kgs", "6", "7", "8", "9", "10"))%>%
  dplyr::select(-6, -7, -8, -9, -10) %>%
  mutate(Treatment="Control")

#Bring in tebuthiuron RHEM dataset
tebuthiuron<- read_csv(RHEM_tebuthiuron, 
                    skip=2, 
                    col_types="dddddddddd", 
                    col_names=c("Time.min", "Precip.mmhr", "Q.mmhr", "Q.m3s", "Sed.kgs", "6", "7", "8", "9", "10"))%>%
  dplyr::select(-6, -7, -8, -9, -10) %>%
  mutate(Treatment="Tebuthiuron")

#Binds together datasets and filters out storage runoff
RHEM <- bind_rows(control, tebuthiuron)%>%
  filter(Time.min < 45.1)%>%
  mutate(Sed.gs=Sed.kgs*1000)

RHEM_plot <- ggplot(RHEM, aes(x=Time.min, color=Treatment))+
  geom_line(aes(y=Q.mmhr), size=2)+
  geom_line(aes(y=Sed.gs*10), linetype="dashed", size=2)+
  scale_y_continuous(name = bquote(bold('Runoff rate (mm·'*h^-1*')')),
                     sec.axis = sec_axis(~./10, name=bquote(bold('Sediment discharge (g·'*s^-1*')'))))+
  labs(x=bquote(bold("Time (min)")))+
  theme(panel.background = element_blank(),
        plot.margin=unit(c(5,5,5,5),"mm"),
        plot.title = element_text(face = "bold", size=28),
        plot.subtitle = element_text(size=24),
        plot.caption = element_text(size=14, face="italic", color="grey50"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="black", size=1),
        axis.text = element_text(size =22),
        axis.title = element_text(face="bold", size=24),
        axis.title.y=element_text(angle=90, vjust=2),
        axis.title.x=element_text(vjust=-0.25),
        legend.box="vertical",
        legend.direction = "horizontal",
        legend.margin=margin(-10,-10,-10,-10),
        legend.key = element_blank(),
        legend.key.size= unit(1, "cm"),
        legend.text= element_text(size = 16), 
        legend.title=element_text(face="italic", size=20),
        legend.position = "bottom", 
        strip.background=element_rect(colour="white",fill="white"),
        strip.text = element_text(face="bold", vjust=-0.25, size=20))+
  scale_color_manual(values = c("Tebuthiuron" = "#386cb0", "Control" = "#fdb462"), labels=c("Control", "Tebuthiuron"))

#Save plot
ggsave("Figures/Supplemental_Figure_2.png", 
       plot=RHEM_plot,
       unit="in",
       height=6,
       width=9,
       dpi=300,
       device="png")