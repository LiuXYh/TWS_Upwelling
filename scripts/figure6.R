#### Contribution of phytoplankton dissolved primary production to bacterial carbon demand

library(reshape2)
library(doBy)
library(ggplot2)
library(ggpubr)
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


## Pie charts illustrating the distribution of the ratios of dissolved primary production to bacterial carbon demand (Fig. 6a)
dat$DPP_BCD <- dat$DPP / dat$BCD
dat$BCD2 <- 1
dat$DPP2 <- dat$BCD2 - dat$DPP_BCD

dat1 <- dat[c('No', 'Longitude', 'Latitude', 'DPP_BCD', 'DPP2', 'BCD2')]
dat1 <- melt(dat1, id = c('No', 'Longitude', 'Latitude', 'BCD2'))

# Several pie charts were appended outside the Taiwan Strait map, with their sizes representing the ratios of dissolved primary production to bacterial carbon demand, corresponding to values of 0.12, 0.25, 0.50, and 0.75 respectively.
# These charts can be adjusted later to serve as the legend
dat2 <- data.frame(
	No = c('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd'), 
	Longitude = c(116, 116, 116, 116, 116, 116, 116, 116), 
	Latitude = c(27, 27, 26, 26, 25, 25, 24, 24), 
	BCD2 = c(1, 1, 1, 1, 1, 1, 1, 1), 
	variable = c('DPP_BCD', 'DPP2', 'DPP_BCD', 'DPP2', 'DPP_BCD', 'DPP2', 'DPP_BCD', 'DPP2'),
	value = c(0.12, 0.88, 0.25, 0.75, 0.50, 0.50, 0.75, 0.25)
)

dat1 <- rbind(dat1, dat2)
dat1$variable <- factor(dat1$variable, levels = c('DPP_BCD', 'DPP2'))
dat1 <- dat1[order(dat1$variable, decreasing = TRUE), ]
dat1$BCD2 <- dat1$BCD2/5

p +
geom_scatterpie(data = dat1, aes(x = Longitude, y = Latitude, group = No, r = BCD2), alpha = 0.5, cols = 'variable', long_format = TRUE) + 
scale_fill_manual(limits = c('DPP_BCD', 'DPP2'), values = c('#db0119', 'transparent')) + 
theme(legend.position = 'none')


## Probability density distribution of the DPP-to-BCD ratio (Fig. 6b)
ggdensity(dat, x = 'DPP_BCD', y = '..density..', color = '#db0119', add = 'mean', rug = TRUE) +
labs(x = 'DPP:BCD', y = 'Probability density')


## Nonparametric test (kruskal + dunn test), with significance defined as p < 0.05 (Fig. 6c)
dat1 <- dat[c('Watermass', 'DPP_BCD')]
dat1 <- melt(dat1, id = c('Watermass'))
dat1$Watermass <- factor(dat1$Watermass, levels = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed'))

dat2 <- subset(dat1, variable == 'DPP_BCD')
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

stat <- data.frame(multcompLetters(dat3)$Letters)
names(stat) <- 'groups'
stat$Watermass <- rownames(stat)
stat$variable <- 'DPP_BCD'

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
labs(x = '', y = 'DPP:BCD')





