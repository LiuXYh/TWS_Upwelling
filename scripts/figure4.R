#### Phytoplankton primary production across ecological regimes in the Taiwan Strait

library(reshape2)
library(doBy)
library(ggplot2)
library(scatterpie)
library(rstatix)
library(tibble)
library(multcompView)


## Map of the Taiwan Strait
dat <- read.csv('../data/TWS_data.csv')

map <- map_data('world')
map <- subset(map, long > 95 & long < 135)
map <- subset(map, lat > 0 & lat < 45)

p <- ggplot() +
geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
theme_bw() +
theme(panel.grid = element_blank(), 
	legend.background = element_blank(), 
	legend.key = element_blank()) +
coord_cartesian(xlim = c(114, 122),  ylim = c(21, 28)) +
scale_x_continuous(breaks = seq(114, 122, 2), expand = c(0, 0)) +
scale_y_continuous(breaks = seq(22, 28, 2), expand = c(0, 0)) +
labs(x = 'Longitude (°E)', y = 'Latitude (°N)')

p


## Pie charts illustrating the composition of total primary production (dissolved primary production and particulate primary production) (Fig. 4a)
dat1 <- dat[c('No', 'Longitude', 'Latitude', 'DPP', 'PPP', 'TPP')]
dat1 <- melt(dat1, id = c('No', 'Longitude', 'Latitude', 'TPP'))

# Several pie charts were appended outside the Taiwan Strait map, with their sizes corresponding to total primary production of 50, 100, 250, and 500 mg C m-3 day-1, respectively
# These charts can be adjusted later to serve as the legend
dat2 <- data.frame(
	No = c('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd'), 
	Longitude = c(116, 116, 116, 116, 116, 116, 116, 116), 
	Latitude = c(27, 27, 26, 26, 25, 25, 24, 24), 
	TPP = c(50, 50, 100, 100, 250, 250, 500, 500), 
	variable = c('DPP', 'PPP', 'DPP', 'PPP', 'DPP', 'PPP', 'DPP', 'PPP'),
	value = c(50, 0, 100, 0, 250, 0, 500, 0)
)
dat1 <- rbind(dat1, dat2)
dat1$variable <- factor(dat1$variable, levels = c('DPP', 'PPP'))

dat1$TPP2 <- (dat1$TPP)^0.5/ 80
dat1 <- dat1[order(dat1$TPP2, dat1$variable, decreasing = TRUE), ]

p +
geom_scatterpie(data = dat1, aes(x = Longitude, y = Latitude, group = No, r = TPP2), alpha = 0.5, cols = 'variable', long_format = TRUE) + 
scale_fill_manual(limits = c('DPP', 'PPP'), values = c('#f23a3a', '#aac1ff')) +
labs(fill = '')


## Pie charts illustrating the distribution of the proportion of dissolved primary production relative to total primary production (Fig. 4b)
dat1 <- dat[c('No', 'Longitude', 'Latitude', 'PER')]
dat1$variable <- 'a'
dat1$value <- dat1$PER

# Several pie charts were appended outside the Taiwan Strait map, with their sizes corresponding to percent extracellular release of 0.15, 0.3, and 0.45, respectively
# These charts can be adjusted later to serve as the legend
dat2 <- data.frame(
	No = c('a', 'b', 'c'), 
	Longitude = c(116, 116, 116), 
	Latitude = c(27, 26, 25), 
	PER = c(0.15, 0.3, 0.45), 
	variable = c('a', 'a', 'a'),
	value = c(0.15, 0.3, 0.45)
)
dat1 <- rbind(dat1, dat2)
dat1$PER2 <- (dat1$PER)^1.3
dat1 <- dat1[order(dat1$PER2, dat1$variable, decreasing = TRUE), ]

p +
geom_point(data = dat1, aes(x = Longitude, y = Latitude, size = PER2), alpha = 0.5, color = '#ffc196') +
scale_size(range = c(1, 10)) +
theme(legend.position = 'none')


## Nonparametric test (kruskal + dunn test), with significance defined as p < 0.05 (Fig. 4c-f)
env <- c('TPP', 'DPP', 'PPP', 'PER')
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

