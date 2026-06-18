library(vegan)
library(FactoMineR)
library(ggplot2)
library(pheatmap)


# 使用各站位的表层温度、表层盐度、表层叶绿素浓度和离岸距离，对各站位进行生态聚类
dat <- read.csv('水团划分.csv', row.names = 1)

#dat$SST <- log10(dat$SST)
#dat$SSS <- log10(dat$SSS)
dat$SSChla <- log10(dat$SSChla)  # 非正态分布的数据进行log转化以服从正态分布
dat$Distco <- log10(dat$Distco)


#聚类热图
pheatmap(
	t(scale(dat[c('SST', 'SSS', 'Distco', 'SSChla')])), 
	color = colorRampPalette(c('#323596', '#4475b4', '#5b9cc3', '#abd9e7', '#e2f1fa', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'))(100), 
	clustering_method = 'ward.D2', 
	cluster_rows = FALSE, 
	annotation_col = data.frame(Group1 = dat$cluster2, Group2 = dat$cluster1, row.names = rownames(dat), stringsAsFactors = TRUE), 
	annotation_colors = list(Group1 = c(Plume = '#db0119', Oligotrophic = '#67a6d0', Upwelling = '#0aa33e', Mixed = '#f39727', X = 'black'), Group2 = c(Plume = '#db0119', Oligotrophic = '#67a6d0', Upwelling = '#0aa33e', Mixed = '#f39727'))
)


#PCA
pca <- PCA(dat[c('SST', 'SSS', 'Distco', 'SSChla')], ncp = 2, scale.unit = TRUE, graph = FALSE)
pca_eig1 <- round(pca$eig[1,2], 2)
pca_eig2 <- round(pca$eig[2,2], 2)
pca_sample <- data.frame(pca$ind$coord[ ,1:2])
pca_sample <- cbind(pca_sample, dat[rownames(pca_sample), ])
pca_var <- data.frame(pca$var$coord[ ,1:2])
pca_var$label <- rownames(pca_var)

ggplot(data = pca_sample, aes(x = Dim.1, y = Dim.2)) +
geom_point(aes(color = cluster1), size = 1) + 
scale_color_manual(values = c('#db0119', '#67a6d0', '#0aa33e', '#f39727'), limits = c('Plume', 'Oligotrophic', 'Upwelling', 'Mixed')) +
geom_segment(data = pca_var, aes(x = 0, y = 0, xend = Dim.1*6, yend = Dim.2*6), color = 'blue', size = 0.3) +
geom_text(data = pca_var, aes(x = Dim.1*6.5, y = Dim.2*6.5, label = label), color = 'blue', size = 3) + 
theme(panel.grid = element_blank(), 
    panel.background = element_rect(color = 'black', fill = 'transparent'), 
    legend.key = element_rect(fill = 'transparent'), 
    legend.text = element_text(color = 'black'),
    axis.text = element_text(color = 'black'), 
    axis.ticks = element_line(color = 'black'), 
    axis.title = element_text(color = 'black')) +
labs(x = paste('PCA1 (', pca_eig1, '% )'), y = paste('PCA2 (', pca_eig2, '% )'), color = '', shape = '') + 
geom_vline(xintercept = 0, color = 'black', size = 0.5, linetype = 2) + 
geom_hline(yintercept = 0, color = 'black', size = 0.5, linetype = 2)


#地图投影
ggplot() +
geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), color = '#e6e6e6', fill = '#c8c8c8') +
geom_point(data = dat, size = 2.5, aes(x = Longitude, y = Latitude, color = cluster2)) +
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


