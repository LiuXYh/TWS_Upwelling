#### Spatial distribution of sampling stations across the Taiwan Strait during multiple research cruises conducted under the southwest monsoon

library(ggplot2)
library(maps)
library(mapproj)


# The investigated sea area spans 114°E–122°E and 21°N–28°N
par(mar = rep(0,4))
map('world', proj = 'ortho', orient = c(23, 120, 0), col = 'gray90', fill = TRUE)
lon <- c(114, 122, 122, 114, 114)
lat <- c(21, 21, 28, 28, 21)
proj_xy <- mapproject(lon, lat)
lines(proj_xy$x, proj_xy$y, lwd = 2, col = 'red')


# Mapping the Taiwan Strait
map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.2, 0.8), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)')

p


# Add sampling stations
dat <- read.csv('../data/TWS_data.csv')

p +
geom_point(data = dat, size = 2.5, aes(x = Longitude, y = Latitude, color = Cruise)) +
scale_color_manual(values = c('#F12200', '#FBC201', '#BEFF00', '#00B40B', '#01FBF1', '#2900D6', '#B605D7'))


# Note: The bathymetric Taiwan Strait map in the manuscript was generated via Ocean Data View and composited with the section’s station map


