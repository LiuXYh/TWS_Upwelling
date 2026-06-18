#### Environmental characteristics of different ecological regimes in the Taiwan Strait

library(reshape2)
library(doBy)
library(ggplot2)
library(MBA)
library(rstatix)
library(tibble)
library(multcompView)


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
	legend.position = c(0.2, 0.8), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)')

p


# Distribution of average temperature in the euphotic layer of all sampling stations (Fig. 2b)
T <- dat[c('Longitude', 'Latitude', 'T')]
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
	legend.position = c(0.2, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Temperature (°C)')

T

#ggsave('T.pdf', T, width = 5, height = 5)


# Distribution of average salinity in the euphotic layer of all sampling stations (Fig. 2c)
S <- dat[c('Longitude', 'Latitude', 'S')]
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Salinity')

S

#ggsave('S.pdf', S, width = 5, height = 5)


# Distribution of average chlorophyll concentrations in the euphotic layer of all sampling stations (Fig. 2d)
Chla <- dat[c('Longitude', 'Latitude', 'Chla')]
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
	legend.position = c(0.2, 0.65), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Total chlorophyll-a (mg m-3)')

Chla

#ggsave('Chla.pdf', Chla, width = 5, height = 5)


# Proportion of microphytoplankton chlorophyll relative to total chlorophyll in the euphotic layer across all stations
dat$Chla_Micro_prop <- dat$Chla_Micro / dat$Chla * 100
Chla_Micro_prop <- dat[c('Longitude', 'Latitude', 'Chla_Micro_prop')]
names(Chla_Micro_prop) <- c('lon', 'lat', 'value')

Chla_Micro_prop <- mba.surf(na.omit(Chla_Micro_prop[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(Chla_Micro_prop$xyz.est$z) <- list(Chla_Micro_prop$xyz.est$x, Chla_Micro_prop$xyz.est$y)
Chla_Micro_prop <- melt(Chla_Micro_prop$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
Chla_Micro_prop <- na.omit(Chla_Micro_prop)

Chla_Micro_prop[which(Chla_Micro_prop$value<=0),'value'] <- 0
Chla_Micro_prop[which(Chla_Micro_prop$value>0 & Chla_Micro_prop$value<=20),'value'] <- 20
Chla_Micro_prop[which(Chla_Micro_prop$value>20 & Chla_Micro_prop$value<=40),'value'] <- 40
Chla_Micro_prop[which(Chla_Micro_prop$value>40 & Chla_Micro_prop$value<=60),'value'] <- 60
Chla_Micro_prop[which(Chla_Micro_prop$value>60 & Chla_Micro_prop$value<=80),'value'] <- 80
Chla_Micro_prop[which(Chla_Micro_prop$value>80),'value'] <- 100

Chla_Micro_prop <- ggplot() +
geom_raster(data = Chla_Micro_prop, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#fbfbf7', '#dfe0c2', '#c3c58b', '#aeb062', '#92962d', '#808409')) + 
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Microphytoplankton (%)')

Chla_Micro_prop

#ggsave('Chla_Micro_prop.pdf', Chla_Micro_prop, width = 5, height = 5)


# Proportion of nanophytoplankton chlorophyll relative to total chlorophyll in the euphotic layer across all stations
dat$Chla_Nano_prop <- dat$Chla_Nano / dat$Chla * 100
Chla_Nano_prop <- dat[c('Longitude', 'Latitude', 'Chla_Nano_prop')]
names(Chla_Nano_prop) <- c('lon', 'lat', 'value')

Chla_Nano_prop <- mba.surf(na.omit(Chla_Nano_prop[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(Chla_Nano_prop$xyz.est$z) <- list(Chla_Nano_prop$xyz.est$x, Chla_Nano_prop$xyz.est$y)
Chla_Nano_prop <- melt(Chla_Nano_prop$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
Chla_Nano_prop <- na.omit(Chla_Nano_prop)

Chla_Nano_prop[which(Chla_Nano_prop$value<=0),'value'] <- 0
Chla_Nano_prop[which(Chla_Nano_prop$value>0 & Chla_Nano_prop$value<=20),'value'] <- 20
Chla_Nano_prop[which(Chla_Nano_prop$value>20 & Chla_Nano_prop$value<=40),'value'] <- 40
Chla_Nano_prop[which(Chla_Nano_prop$value>40 & Chla_Nano_prop$value<=60),'value'] <- 60
Chla_Nano_prop[which(Chla_Nano_prop$value>60 & Chla_Nano_prop$value<=80),'value'] <- 80
Chla_Nano_prop[which(Chla_Nano_prop$value>80),'value'] <- 100

Chla_Nano_prop <- ggplot() +
geom_raster(data = Chla_Nano_prop, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#f8faf7', '#c6dac0', '#9bbe90', '#74a565', '#4b8a38', '#23710b')) +
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Nanophytoplankton (%)')

Chla_Nano_prop

#ggsave('Chla_Nano_prop.pdf', Chla_Nano_prop, width = 5, height = 5)


# Proportion of microphytoplankton and nanophytoplankton chlorophyll relative to total chlorophyll in the euphotic layer across all stations (Fig. 2e)
dat$Chla_Micro_Nano_prop <- (dat$Chla_Micro + dat$Chla_Nano) / dat$Chla * 100
Chla_Micro_Nano_prop <- dat[c('Longitude', 'Latitude', 'Chla_Micro_Nano_prop')]
names(Chla_Micro_Nano_prop) <- c('lon', 'lat', 'value')

Chla_Micro_Nano_prop <- mba.surf(na.omit(Chla_Micro_Nano_prop[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(Chla_Micro_Nano_prop$xyz.est$z) <- list(Chla_Micro_Nano_prop$xyz.est$x, Chla_Micro_Nano_prop$xyz.est$y)
Chla_Micro_Nano_prop <- melt(Chla_Micro_Nano_prop$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
Chla_Micro_Nano_prop <- na.omit(Chla_Micro_Nano_prop)

Chla_Micro_Nano_prop[which(Chla_Micro_Nano_prop$value<=0),'value'] <- 0
Chla_Micro_Nano_prop[which(Chla_Micro_Nano_prop$value>0 & Chla_Micro_Nano_prop$value<=20),'value'] <- 20
Chla_Micro_Nano_prop[which(Chla_Micro_Nano_prop$value>20 & Chla_Micro_Nano_prop$value<=40),'value'] <- 40
Chla_Micro_Nano_prop[which(Chla_Micro_Nano_prop$value>40 & Chla_Micro_Nano_prop$value<=60),'value'] <- 60
Chla_Micro_Nano_prop[which(Chla_Micro_Nano_prop$value>60 & Chla_Micro_Nano_prop$value<=80),'value'] <- 80
Chla_Micro_Nano_prop[which(Chla_Micro_Nano_prop$value>80),'value'] <- 100

Chla_Micro_Nano_prop <- ggplot() +
geom_raster(data = Chla_Micro_Nano_prop, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#f8faf7', '#c6dac0', '#9bbe90', '#74a565', '#4b8a38', '#23710b')) +
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Micro+Nanophytoplankton (%)')

Chla_Micro_Nano_prop

#ggsave('Chla_Micro_Nano_prop.pdf', Chla_Micro_Nano_prop, width = 5, height = 5)


# Proportion of picophytoplankton chlorophyll relative to total chlorophyll in the euphotic layer across all stations (Fig. 2f)
dat$Chla_Pico_prop <- dat$Chla_Pico / dat$Chla * 100
Chla_Pico_prop <- dat[c('Longitude', 'Latitude', 'Chla_Pico_prop')]
names(Chla_Pico_prop) <- c('lon', 'lat', 'value')

Chla_Pico_prop <- mba.surf(na.omit(Chla_Pico_prop[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(Chla_Pico_prop$xyz.est$z) <- list(Chla_Pico_prop$xyz.est$x, Chla_Pico_prop$xyz.est$y)
Chla_Pico_prop <- melt(Chla_Pico_prop$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
Chla_Pico_prop <- na.omit(Chla_Pico_prop)

Chla_Pico_prop[which(Chla_Pico_prop$value<=0),'value'] <- 0
Chla_Pico_prop[which(Chla_Pico_prop$value>0 & Chla_Pico_prop$value<=20),'value'] <- 20
Chla_Pico_prop[which(Chla_Pico_prop$value>20 & Chla_Pico_prop$value<=40),'value'] <- 40
Chla_Pico_prop[which(Chla_Pico_prop$value>40 & Chla_Pico_prop$value<=60),'value'] <- 60
Chla_Pico_prop[which(Chla_Pico_prop$value>60 & Chla_Pico_prop$value<=80),'value'] <- 80
Chla_Pico_prop[which(Chla_Pico_prop$value>80),'value'] <- 100

Chla_Pico_prop <- ggplot() +
geom_raster(data = Chla_Pico_prop, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#f7fafb', '#bdd5d8', '#96bcc2', '#639ba4', '#38808b', '#0b6370')) + 
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Picophytoplankton (%)')

Chla_Pico_prop

#ggsave('Chla_Pico_prop.pdf', Chla_Pico_prop, width = 5, height = 5)


# Distribution of average dissolved inorganic nitrogen (DIN) concentrations in the euphotic layer at all sampling stations (Fig. 2g)
DIN <- dat[c('Longitude', 'Latitude', 'DIN')]
names(DIN) <- c('lon', 'lat', 'value')

DIN <- mba.surf(na.omit(DIN[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(DIN$xyz.est$z) <- list(DIN$xyz.est$x, DIN$xyz.est$y)
DIN <- melt(DIN$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
DIN <- na.omit(DIN)

DIN[which(DIN$value<=2),'value'] <- 2
DIN[which(DIN$value>2 & DIN$value<5),'value'] <- 5
DIN[which(DIN$value>5 & DIN$value<=10),'value'] <- 10
DIN[which(DIN$value>10 & DIN$value<=15),'value'] <- 15
DIN[which(DIN$value>15 & DIN$value<=20),'value'] <- 20
DIN[which(DIN$value>20),'value'] <- 25

DIN <- ggplot() +
geom_raster(data = DIN, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#f8f9fc', '#bcc5e7', '#92a0d8', '#6479c7', '#3852b7', '#0c2ca7')) + 
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Nitrate (μmol L-1)')

DIN

#ggsave('DIN.pdf', DIN, width = 5, height = 5)


# Distribution of average dissolved inorganic phosphate (DIP) concentrations in the euphotic layer at all sampling stations (Fig. 2h)
DIP <- dat[c('Longitude', 'Latitude', 'DIP')]
names(DIP) <- c('lon', 'lat', 'value')

DIP <- mba.surf(na.omit(DIP[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(DIP$xyz.est$z) <- list(DIP$xyz.est$x, DIP$xyz.est$y)
DIP <- melt(DIP$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
DIP <- na.omit(DIP)

DIP[which(DIP$value<=0.1),'value'] <- 0.1
DIP[which(DIP$value>0.1 & DIP$value<=1.5),'value'] <- 0.5
DIP[which(DIP$value>0.5 & DIP$value<=1),'value'] <- 1
DIP[which(DIP$value>1 & DIP$value<=1.5),'value'] <- 1.5
DIP[which(DIP$value>1.5 & DIP$value<=2),'value'] <- 2
DIP[which(DIP$value>2),'value'] <- 2.5

DIP <- ggplot() +
geom_raster(data = DIP, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#faf6fa', '#cabcd8', '#a48cbc', '#8364a4', '#62398c', '#3d0c71')) + 
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Phosphate (μmol L-1)')

DIP

#ggsave('DIP.pdf', DIP, width = 5, height = 5)


# Distribution of average dissolved inorganic silicate (DSi) concentrations in the euphotic layer at all sampling stations (Fig. 2i)
DSi <- dat[c('Longitude', 'Latitude', 'DSi')]
names(DSi) <- c('lon', 'lat', 'value')

DSi <- mba.surf(na.omit(DSi[c('lon', 'lat', 'value')]), no.X = 50, no.Y = 50, extend = TRUE)
dimnames(DSi$xyz.est$z) <- list(DSi$xyz.est$x, DSi$xyz.est$y)
DSi <- melt(DSi$xyz.est$z, varnames = c('lon', 'lat'), value.name = 'value')
DSi <- na.omit(DSi)

DSi[which(DSi$value<=10),'value'] <- 10
DSi[which(DSi$value>10 & DSi$value<20),'value'] <- 20
DSi[which(DSi$value>20 & DSi$value<=30),'value'] <- 30
DSi[which(DSi$value>30 & DSi$value<=40),'value'] <- 40
DSi[which(DSi$value>40 & DSi$value<=50),'value'] <- 50
DSi[which(DSi$value>50),'value'] <- 60

DSi <- ggplot() +
geom_raster(data = DSi, aes(x = lon, y = lat, fill = value), interpolate = TRUE) + 
scale_fill_gradientn(colors = c('#fcf9fa', '#e4c9d4', '#c790a5', '#b26582', '#99345b', '#840a39')) + 
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
labs(x = 'Longitude (°E)', y = 'Latitude (°N)', fill = 'Silicate (μmol L-1)')

DSi

#ggsave('DSi.pdf', DSi, width = 5, height = 5)


# Nonparametric test (kruskal + dunn test), with significance defined as p < 0.05
# The generated box plots were overlaid onto the above spatial distribution maps of environmental factors to form the composite figure presented in the main manuscript
env <- c('T', 'S', 'Chla', 'Chla_Micro_prop', 'Chla_Nano_prop', 'Chla_Micro_Nano_prop', 'Chla_Pico_prop', 'DIN', 'DIP', 'DSi')
dat1 <- dat[c('Watermass', env)]
dat1 <- na.omit(melt(dat1, id = c('Watermass')))
dat1$Watermass <- factor(dat1$Watermass, levels = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed'))

stat <- NULL
for (i in env) {
	dat2 <- subset(dat1, variable == i)
	dunn_test <- dunn_test(dat2, value~Watermass, p.adjust.method = 'none')
	p_value_matrix <- as.matrix(column_to_rownames(spread(select(dunn_test, group1, group2, p.adj), group2, p.adj), 'group1'))
	
	dat3 <- matrix(NA, length(unique(c(rownames(p_value_matrix), colnames(p_value_matrix)))), length(unique(c(rownames(p_value_matrix), colnames(p_value_matrix)))))
	rownames(dat3) <- unique(dat2$Watermass)
	colnames(dat3) <- unique(dat2$Watermass)
	for (x in rownames(p_value_matrix)) {
		for(y in colnames(p_value_matrix)) {
			if (!is.na(p_value_matrix[x,y])) dat3[x,y] <- p_value_matrix[x,y]
		}
	}
	for (x in rownames(dat3)) {
		for(y in colnames(dat3)) {
			if (is.na(dat3[x,y])) dat3[x,y] <- dat3[y,x]
		}
	}
	
	dat2 <- summaryBy(value~Watermass, dat2, FUN = mean)
	dat2 <- dat2[order(dat2$value.mean, decreasing = TRUE), ]
	dat3 <- dat3[as.character(dat2$Watermass),as.character(dat2$Watermass)]
	
	kw <- data.frame(multcompLetters(dat3)$Letters)
	names(kw) <- 'groups'
	kw$Watermass <- rownames(kw)
	kw$variable <- i
	stat <- rbind(stat, kw)
}
stat <- merge(stat, summaryBy(value~variable, dat1, FUN = max), by = 'variable', all = TRUE)
names(stat)[4] <- 'value'
stat$Watermass <- factor(stat$Watermass, levels = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed'))

ggplot(dat1, aes(Watermass, value, fill = Watermass)) +
facet_wrap(~variable, scale = 'free') +
geom_boxplot(width = 0.6, size = 0.5, outlier.size = 1) +
scale_fill_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
geom_text(data = stat, aes(Watermass, value*1.3, label = groups)) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.position = 'none') +
scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
labs(x = '', y = '')

