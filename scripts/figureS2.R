#### Distributions of sea surface temperature, sea surface salinity, and sea surface chlorophyll-a

library(reshape2)
library(doBy)
library(ggplot2)
library(MBA)


# Map of the Taiwan Strait showing the water mass affiliation of each station
dat <- read.csv('../data/TWS_data.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
geom_point(data = dat, size = 2.5, aes(x = Longitude, y = Latitude, color = Watermass)) +
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.25, 0.8), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)')

p


# Distribution of sea surface temperature of all sampling stations (Fig. S2a)
T <- dat[c('Longitude', 'Latitude', 'SST')]
names(T) <- c('lon', 'lat', 'value')

T <- mba.surf(na.omit(T[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(T$xyz.est$z) <- list(T$xyz.est$x, T$xyz.est$y)
T <- melt(T$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
T <- na.omit(T)

T[which(T$value<=26),'value'] <- 25
T[which(T$value>26 & T$value<=27),'value'] <- 27
T[which(T$value>27 & T$value<=28),'value'] <- 28
T[which(T$value>28 & T$value<=29),'value'] <- 29
T[which(T$value>29 & T$value<=30),'value'] <- 30
T[which(T$value>30),'value'] <- 31

T <- ggplot() +
geom_raster(data = T, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#fffcfd', '#fcf0e5', '#f8c9a3', '#f98a54', '#bf2929', '#a81f2c')) + 
annotate(geom = 'polygon', x = c(118.5, 121.5, 121.5), y = c(22.2, 22.2, 25.9), fill = 'white', color = 'white') +
geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Longitude, y = Latitude, color = Watermass), size = 1.5) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.25, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Sea surface temperature (°C)')

T

#ggsave('SST.pdf', T, width = 5, height = 5)


# Distribution of sea surface salinity of all sampling stations (Fig. S2b)
S <- dat[c('Longitude', 'Latitude', 'SSS')]
names(S) <- c('lon', 'lat', 'value')

S <- mba.surf(na.omit(S[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(S$xyz.est$z) <- list(S$xyz.est$x, S$xyz.est$y)
S <- melt(S$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
S <- na.omit(S)

S[which(S$value<=30),'value'] <- 29
S[which(S$value>30 & S$value<=31),'value'] <- 30
S[which(S$value>31 & S$value<=32),'value'] <- 31
S[which(S$value>32 & S$value<=33),'value'] <- 32
S[which(S$value>33 & S$value<=34),'value'] <- 33
S[which(S$value>34),'value'] <- 34

S <- ggplot() +
geom_raster(data = S, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#FFFFFF', '#e2f1fa', '#abd9e7', '#5b9cc3', '#4475b4', '#323596')) + 
annotate(geom = 'polygon', x = c(118.5, 121.5, 121.5), y = c(22.2, 22.2, 25.9), fill = 'white', color = 'white') +
geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Longitude, y = Latitude, color = Watermass), size = 1.5) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.2, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Sea surface salinity')

S

#ggsave('SSS.pdf', S, width = 5, height = 5)


# Distribution of sea surface chlorophyll concentrations of all sampling stations (Fig. S2c)
Chla <- dat[c('Longitude', 'Latitude', 'SSChla')]
names(Chla) <- c('lon', 'lat', 'value')

Chla <- mba.surf(na.omit(Chla[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(Chla$xyz.est$z) <- list(Chla$xyz.est$x, Chla$xyz.est$y)
Chla <- melt(Chla$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
Chla <- na.omit(Chla)

Chla[which(Chla$value<=0.5),'value'] <- 0
Chla[which(Chla$value>0.5 & Chla$value<=1),'value'] <- 0.5
Chla[which(Chla$value>1 & Chla$value<=1.5),'value'] <- 1
Chla[which(Chla$value>1.5 & Chla$value<=2),'value'] <- 1.5
Chla[which(Chla$value>2 & Chla$value<=2.5),'value'] <- 2
Chla[which(Chla$value>2.5),'value'] <- 2.5

Chla <- ggplot() +
geom_raster(data = Chla, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#FDFDFD', '#A0D9CC', '#70C4AC', '#33AB81', '#087F46', '#1C4D1D')) + 
annotate(geom = 'polygon', x = c(118.5, 121.5, 121.5), y = c(22.2, 22.2, 25.9), fill = 'white', color = 'white') +
geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Longitude, y = Latitude, color = Watermass), size = 1.5) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.3, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Sea surface chlorophyll-a (mg m-3)')

Chla

#ggsave('SSChla.pdf', Chla, width = 5, height = 5)


# Climatological sea surface temperature for the same region and season (Fig. S2d)
T <- read.csv('../data/Climatological_SST.csv')
T <- mba.surf(na.omit(T[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(T$xyz.est$z) <- list(T$xyz.est$x, T$xyz.est$y)
T <- melt(T$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
T <- na.omit(T)

T[which(T$value<=26),'value'] <- 25
T[which(T$value>26 & T$value<=27),'value'] <- 27
T[which(T$value>27 & T$value<=28),'value'] <- 28
T[which(T$value>28 & T$value<=29),'value'] <- 29
T[which(T$value>29 & T$value<=30),'value'] <- 30
T[which(T$value>30),'value'] <- 31

T <- ggplot() +
geom_raster(data = T, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#fffcfd', '#fcf0e5', '#f8c9a3', '#f98a54', '#bf2929', '#a81f2c')) + 
geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Longitude, y = Latitude, color = Watermass), size = 1.5) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.25, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Sea surface temperature (°C)')

T

#ggsave('Climatological_SST.pdf', T, width = 5, height = 5)


# Climatological sea surface salinity for the same region and season (Fig. S2e)
S <- read.csv('../data/Climatological_SSS.csv')
S <- mba.surf(na.omit(S[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(S$xyz.est$z) <- list(S$xyz.est$x, S$xyz.est$y)
S <- melt(S$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
S <- na.omit(S)

S[which(S$value<=30),'value'] <- 29
S[which(S$value>30 & S$value<=31),'value'] <- 30
S[which(S$value>31 & S$value<=32),'value'] <- 31
S[which(S$value>32 & S$value<=33),'value'] <- 32
S[which(S$value>33 & S$value<=34),'value'] <- 33
S[which(S$value>34),'value'] <- 34

S <- ggplot() +
geom_raster(data = S, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#FFFFFF', '#e2f1fa', '#abd9e7', '#5b9cc3', '#4475b4', '#323596')) + 
geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Longitude, y = Latitude, color = Watermass), size = 1.5) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.2, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Sea surface salinity')

S

#ggsave('Climatological_SSS.pdf', S, width = 5, height = 5)


# Climatological sea surface chlorophyll concentrations for the same region and season (Fig. S2f)
Chla <- read.csv('../data/Climatological_SSChla.csv')
Chla <- mba.surf(na.omit(Chla[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(Chla$xyz.est$z) <- list(Chla$xyz.est$x, Chla$xyz.est$y)
Chla <- melt(Chla$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
Chla <- na.omit(Chla)

Chla[which(Chla$value<=0.5),'value'] <- 0
Chla[which(Chla$value>0.5 & Chla$value<=1),'value'] <- 0.5
Chla[which(Chla$value>1 & Chla$value<=1.5),'value'] <- 1
Chla[which(Chla$value>1.5 & Chla$value<=2),'value'] <- 1.5
Chla[which(Chla$value>2 & Chla$value<=2.5),'value'] <- 2
Chla[which(Chla$value>2.5),'value'] <- 2.5

Chla <- ggplot() +
geom_raster(data = Chla, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#FDFDFD', '#A0D9CC', '#70C4AC', '#33AB81', '#087F46', '#1C4D1D')) + 
geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = '#c8c8c8') +
geom_point(data = dat, aes(x = Longitude, y = Latitude, color = Watermass), size = 1.5) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.position = c(0.3, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Sea surface chlorophyll-a (mg m-3)')

Chla

#ggsave('Climatological_SSChla.pdf', Chla, width = 5, height = 5)

