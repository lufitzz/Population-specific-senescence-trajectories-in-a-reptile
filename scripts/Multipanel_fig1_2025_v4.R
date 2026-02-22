##############################################################################
# MULTIPANEL FIGURE 1 FOR SENESCENCE MS 
##############################################################################

library(ggplot2)
library(rnaturalearth)
library(terra)
library(sf)
library(cowplot)
library(dplyr)
library(geodata)
library(magick)

##############################################################################
# PANEL A: MAP 
##############################################################################

# Download Australia elevation at 30 arc-second resolution
elev <- elevation_30s(country = "AUS", path = tempdir())

# Define Tasmania extent
tas_extent <- ext(144.5, 148.5, -43.7, -40.0)

# Crop to Tasmania
elev_tas <- crop(elev, tas_extent)

# Convert to dataframe for ggplot
elev_df <- as.data.frame(elev_tas, xy = TRUE)
colnames(elev_df) <- c("lon", "lat", "elevation")
elev_df <- elev_df[!is.na(elev_df$elevation), ]

# Get Australia outline
australia <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

# Sites
sites <- data.frame(
  name = c("Cool highland", "Warm lowland"),
  lon = c(146.5722, 147.8489),
  lat = c(-41.8447, -42.5594))

# Create map
map1 <- ggplot() +
  geom_raster(data = elev_df, aes(x = lon, y = lat, fill = elevation)) +
  scale_fill_gradientn(
    colours = terrain.colors(10), 
    name = "Elevation (m)") +
  geom_point(data = sites, aes(x = lon, y = lat), 
             shape = 21, fill = c("cornflowerblue", "coral1"),
             color = "black", size = 5, stroke = 1.2) +
  geom_text(data = sites, aes(x = lon, y = lat, label = name), 
            hjust = 0.6, vjust = 2.4, size = 3.5, fontface = "bold") +
  coord_sf(xlim = c(144.5, 148.5), ylim = c(-43.7, -40.0),
           crs = 4326, expand = FALSE, datum = sf::st_crs(4326)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key.width = unit(1.5, "cm"),  # Make legend bar wider
        legend.key.height = unit(0.3, "cm"), # Make it thinner
        plot.margin = margin(10, 10, 10, 10))

map1

##############################################################################
# PANEL B & C: CLIMATE WITH VARIABILITY 
##############################################################################

# Load and process climate data
bom <- read.csv("BOM.csv")

# Calculate monthly means AND standard deviations
monthly_summary <- bom %>%
  group_by(Site, Month) %>%
  summarise(
    AvgMaxTemp = mean(Maxtemp, na.rm = TRUE),
    SDMaxTemp = sd(Maxtemp, na.rm = TRUE),
    AvgMinTemp = mean(Mintemp, na.rm = TRUE),
    SDMinTemp = sd(Mintemp, na.rm = TRUE),
    TotalRain = sum(Rainfall, na.rm = TRUE),
    SDRain = sd(Rainfall, na.rm = TRUE) * sqrt(n()), # SD of total
    .groups = "drop") %>%
  mutate(Month = factor(Month, levels = 1:12, labels = month.abb, ordered = TRUE))



# Panel B: Temperature with variability ribbons
temp_plot <- ggplot(monthly_summary, aes(x = as.numeric(Month), group = Site, color = Site, fill = Site)) +
  # Max temp ribbon and line
  geom_ribbon(aes(ymin = AvgMaxTemp - SDMaxTemp, ymax = AvgMaxTemp + SDMaxTemp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = AvgMaxTemp), linewidth = 1.2, linetype = "solid") +
  geom_point(aes(y = AvgMaxTemp), size = 2.5) +
  # Min temp ribbon and line
  geom_ribbon(aes(ymin = AvgMinTemp - SDMinTemp, ymax = AvgMinTemp + SDMinTemp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = AvgMinTemp), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = AvgMinTemp), size = 2.5, shape = 1) +
  scale_color_manual(
    values = c("OR" = "coral1", "CP" = "cornflowerblue"),
    labels = c("OR" = "Warm lowland", "CP" = "Cool highland")) +
  scale_fill_manual(
    values = c("OR" = "coral1", "CP" = "cornflowerblue"),
    labels = c("OR" = "Warm lowland", "CP" = "Cool highland")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(y = "Average daily temperature (\u00B0C)", 
       x = NULL, 
       color = "Population",
       fill = "Population") +
  theme_classic(base_size = 11) +
  theme(legend.position = "none",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = "black"),
        axis.text = element_text(color = "black"),
        aspect.ratio = 0.6) 

temp_plot

# Panel C: Rainfall with error bars
rain_plot <- ggplot(monthly_summary, aes(x = as.numeric(Month), y = TotalRain, fill = Site)) +
  geom_col(aes(x = as.numeric(Month) + ifelse(Site == "OR", -0.2, 0.2)), 
           position = "identity", width = 0.35) +
  geom_errorbar(aes(x = as.numeric(Month) + ifelse(Site == "OR", -0.2, 0.2),
                    ymin = TotalRain, ymax = TotalRain + SDRain), 
                width = 0.15, linewidth = 0.5) +
  scale_fill_manual(
    values = c("OR" = "coral1", "CP" = "cornflowerblue"),
    labels = c("OR" = "Warm lowland", "CP" = "Cool highland")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(monthly_summary$TotalRain + monthly_summary$SDRain, na.rm = TRUE) * 1.1)) +
  labs(y = "Total monthly rainfall (mm)",
       x = NULL,
       fill = "Population") +
  theme_classic(base_size = 11) +
  theme(legend.position = "none", 
        axis.text = element_text(color = "black"),
        aspect.ratio = 0.6)

rain_plot



########################################################################
## PANEL D ACTIVITY TIMELINE
########################################################################

# Convert dates to day-of-year and month decimals
# OR: Entry Jun 1 (day 152), Emergence Sep 4 (day 247)
# CP: Entry May 14 (day 134), Emergence Oct 3 (day 276)

# Create daily activity data
create_daily_activity <- function(entry_day, emergence_day, site_name) {
  data.frame(
    Site = site_name,
    Day = 1:365,
    Status = ifelse(1:365 >= entry_day & 1:365 <= emergence_day, "Hibernation", "Active")
  )
}

# Mean hibernation periods
daily_activity <- rbind(
  create_daily_activity(entry_day = 152, emergence_day = 247, site_name = "OR"),  # Jun 1 - Sep 4
  create_daily_activity(entry_day = 134, emergence_day = 276, site_name = "CP")   # May 14 - Oct 3
)

# Add month labels for x-axis
daily_activity$Month <- as.numeric(format(as.Date(daily_activity$Day - 1, origin = "2024-01-01"), "%m"))
daily_activity$MonthDay <- as.Date(daily_activity$Day - 1, origin = "2024-01-01")

# Hibernation timing with ranges
hiber_timing <- data.frame(
  Site = c("OR", "OR", "CP", "CP"),
  Event = c("Entry", "Emergence", "Entry", "Emergence"),
  Day = c(152, 247, 134, 276),  # Mean days
  SD_days = c(10.9, 12.3, 12.5, 12.4),
  MinDay = c(129, 215, 105, 250),  # Min from ranges: May 9, Aug 3, Apr 15, Sep 7
  MaxDay = c(173, 267, 154, 295),  # Max from ranges: Jun 22, Sep 24, Jun 3, Oct 22
  y = c(2, 2, 1, 1)  # CP=1, OR=2
)

activity_plot <- ggplot(daily_activity, aes(x = Day, y = Site, fill = interaction(Site, Status))) +
  geom_tile(color = NA, height = 0.8, alpha =0.4) +
  
  # Add error bars showing actual range
  geom_errorbarh(data = hiber_timing, 
                 aes(xmin = MinDay, xmax = MaxDay, 
                     y = y, color = Site),
                 height = 0.4, linewidth = 1.2, inherit.aes = FALSE) +
  geom_point(data = hiber_timing,
             aes(x = Day, y = y, color = Site),
             size = 3, inherit.aes = FALSE) +
  
  scale_fill_manual(
    values = c("OR.Active" = "white", "OR.Hibernation" = "coral1",
               "CP.Active" = "white", "CP.Hibernation" = "cornflowerblue"),
    guide = "none") +  # Remove legend
  scale_color_manual(
    values = c("OR" = "coral1", "CP" = "cornflowerblue"),
    guide = "none") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb,
    expand = expansion(mult = 0.01)) +
  scale_y_discrete(labels = NULL) +
  labs(x = NULL, y = 'Dormant season') +  # No title
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(color = "black"),
        panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
        aspect.ratio = 0.3)

activity_plot





##############################################################################
# PANEL E: SPECIES PHOTO (SMALLER)
##############################################################################

photo_species <- magick::image_read("ocellatus.jpg")
p_species <- ggdraw() + draw_image(photo_species)

##############################################################################
# COMBINE: 5 PANEL WITH PHOTO
##############################################################################

# Climate plots without legends
temp_plot_no_legend <- temp_plot + 
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5))

rain_plot_no_legend <- rain_plot + 
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5))

# Add consistent margins to activity plot
activity_plot <- activity_plot + 
  theme(plot.margin = margin(5, 5, 5, 5))



# Fix label positions - move them away from the plots
context_column <- plot_grid(p_species, map1, 
                            ncol = 1, 
                            labels = c("A", "B"),
                            label_x = 0.02,      # Move labels to the left
                            label_y = 0.98,      # Move labels to the top
                            hjust = 0,           # Left-align
                            vjust = 1,           # Top-align
                            rel_heights = c(0.85, 1.15))

data_column <- plot_grid(temp_plot_no_legend, 
                         rain_plot_no_legend,
                         activity_plot, 
                         ncol = 1,
                         labels = c("C", "D", "E"),
                         label_x = 0.02,
                         label_y = 0.98,
                         hjust = 0,
                         vjust = 1,
                         align = "hv",
                         axis = "lr",
                         rel_heights = c(1, 1, 1))


# Combine
final_figure <- plot_grid(context_column, data_column,
                          ncol = 2, 
                          rel_widths = c(1, 1.2))

final_figure

#test actual print size 
# For double-column width
ggsave("Figure_StudySystem_test.jpg", final_figure,
       width = 7, height = 6.5, units = "in", dpi = 300)

##############################################################################
# 4-PANEL WITHOUT SPECIES PHOTO
##############################################################################

# Align climate plots
aligned_climate <- align_plots(temp_plot, rain_plot, align = "v", axis = "l")
climate_column <- plot_grid(aligned_climate[[1]], aligned_climate[[2]], 
                            ncol = 1, labels = c("B", "C"))

# Add legend
legend <- get_legend(
  temp_plot + 
    theme(legend.position = "bottom",
          legend.justification = "center")
)

climate_with_legend <- plot_grid(climate_column, legend, ncol = 1, rel_heights = c(1, 0.1))

# Right column: Climate + Activity stacked
data_column <- plot_grid(climate_with_legend, activity_plot_daily,
                         ncol = 1, labels = c("", "D"),
                         rel_heights = c(1.2, 0.5))

# Combine: Map on left, data on right
final_figure_4panel <- plot_grid(map1, data_column,
                                 ncol = 2, 
                                 labels = c("A", ""),
                                 rel_widths = c(1, 1.4))

final_figure_4panel

ggsave("Figure_StudySystem_4panel.jpg", final_figure_4panel,
       width = 12, height = 6, units = "in", dpi = 600)
##############################################################################