#### Correlation analysis of bacterial carbon metabolism, primary production, and environmental variables across stations

library(reshape2)
library(linkET)
library(ggplot2)
library(ggpubr)
library(plspm)


## Heatmap of Spearman rank correlation coefficients (Fig. 5a)
dat <- read.csv('../data/TWS_data.csv')

dat$N_P <- dat$DIN / dat$DIP
dat$Chla_Micro_prop <- dat$Chla_Micro / dat$Chla
dat$Chla_Nano_prop <- dat$Chla_Nano / dat$Chla
dat$Chla_Pico_prop <- dat$Chla_Pico / dat$Chla
dat$BCD_TPP <- dat$BCD / dat$TPP

dat1 <- dat[c('T', 'S', 'DIN', 'DIP', 'DSi', 'N_P', 'Chla', 'Chla_Micro_prop', 'Chla_Nano_prop', 'Chla_Pico_prop', 'TPP', 'DPP', 'PPP', 'PER', 'BCD', 'BP', 'BR', 'BGE', 'BCD_TPP')]

qcorrplot(correlate(dat1, method = 'spearman', engine = 'Hmisc'), type = 'upper', diag = FALSE) +  
geom_shaping() +  
geom_mark(sep = '\n', size = 2.5, sig.thres = 0.05) +  
scale_fill_gradientn(colors = c('#323596', '#4475b4', '#5b9cc3', '#abd9e7', '#e2f1fa', 'white', '#fce0d2', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), limits = c(-1, 1)) + 
theme(legend.key = element_blank())


## Scatter plots showing correlations of bacterial production with primary production (Fig. 5b)
dat1 <- dat[c('BP', 'DPP', 'PPP')]
dat1 <- melt(dat1, id = 'BP')

dat1$PP <- log10(dat1$value)
dat1$BP <- log10(dat1$BP)

ggplot(dat1, aes(PP, BP, color = variable)) +
geom_point() +
scale_color_manual(values = c('#c10000', '#0019b1'), limits = c('DPP', 'PPP')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
stat_smooth(method = 'lm', formula = y~poly(x, 1), se = TRUE) +
stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
geom_abline() +
scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
labs(x = 'Log10 PP (mg C m-3 day-1)', y = 'Log10 BP (mg C m-3 day-1)')


## Scatter plots showing correlations of bacterial respiration with primary production (Fig. 5c)
dat1 <- dat[c('BR', 'DPP', 'PPP')]
dat1 <- melt(dat1, id = 'BR')

dat1$PP <- log10(dat1$value)
dat1$BR <- log10(dat1$BR)

ggplot(dat1, aes(PP, BR, color = variable)) +
geom_point() +
scale_color_manual(values = c('#c10000', '#0019b1'), limits = c('DPP', 'PPP')) +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
stat_smooth(method = 'lm', formula = y~poly(x, 1), se = TRUE) +
stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
geom_abline() +
scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
labs(x = 'Log10 PP (mg C m-3 day-1)', y = 'Log10 BR (mg C m-3 day-1)')


## Scatter plots showing correlations of bacterial growth efficiency with percent extracellular release (Fig. 5d)
dat1 <- dat[c('PER', 'BGE')]

ggplot(dat1, aes(PER, BGE)) +
geom_point(color = '#c10000') +
theme(panel.grid = element_blank(), 
	panel.background = element_rect(color = 'black', fill = 'white'), 
	axis.ticks = element_line(color = 'black', size = 0.5), 
	axis.text.y = element_text(color = 'black', size = 9), 
	axis.text.x = element_text(color = 'black', size = 9),
	legend.key = element_blank()) +
stat_smooth(method = 'lm', formula = y~poly(x, 1), se = TRUE, color = '#c10000') +
stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', label.x.npc = 'left', label.y.npc = 'top', size = 2.7) +
scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
scale_y_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
labs(x = 'PER', y = 'BGE')


## Partial least squares path modeling (Fig. 5e)

# Construct the PLS-PM model
dat_blocks <- list(
	Nutrient = c('DIN', 'DIP', 'DSi', 'N_P'), 
	Chla = c('Chla_Micro', 'Chla_Nano', 'Chla_Pico'), 
    TPP = c('DPP', 'PPP', 'PER'), 
    BCD = c('BP', 'BR', 'BGE')
)

Nutrient <- c( 0, 0, 0, 0)
Chla <- c(1, 0, 0, 0)
TPP <- c(1, 1, 0, 0)
BCD <- c(1, 1, 1, 0)
dat_path <- rbind(Nutrient, Chla, TPP, BCD)
colnames(dat_path) <- rownames(dat_path)

dat_pls <- plspm(dat, dat_path, dat_blocks, modes = rep('A', 4))


# Examine the path coefficients
summary(dat_pls)

innerplot(dat_pls, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray', box.lwd = 0)
dat_pls$path_coefs
dat_pls$inner_model
dat_pls$gof


# Direct and indirect effects of each latent variable on bacterial metabolism
effect <- data.frame(dat_pls$effects, stringsAsFactors = FALSE)
effect1 <- effect[grepl('BCD', effect$relationships), ]
effect2 <- effect[grepl('TPP', effect$relationships), ]
effect2 <- effect2[!grepl('BCD', effect2$relationships), ]
effect <- rbind(effect1, effect2)
effect <- reshape2::melt(effect, id = c('relationships', 'total'))
effect <- effect[order(effect$total, decreasing = TRUE), ]
effect$relationships <- factor(effect$relationships, levels = unique(effect$relationships))

ggplot(effect, aes(relationships, value)) +
geom_col(aes(fill = variable), position = 'stack', width = 0.5) +
theme(panel.grid = element_blank(), panel.background = element_blank(),
    axis.line = element_line(colour = 'black')) +
labs(x = '', y = 'effects', fill = '') +
geom_hline(yintercept = 0)


# Examine the factor weights
dat_pls$outer_model
outerplot(dat_pls, what = 'loadings', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
outerplot(dat_pls, what = 'weights', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')

ggplot(dat_pls$outer_model, aes(name, block, fill = weight)) +
geom_tile() +
scale_fill_gradientn(colors = c('#323596', '#4475b4', '#5b9cc3', '#abd9e7', 'gray90', '#fbbd9e', '#fc694a', '#c8181b', '#a80029'), limits = c(-1, 1)) + 
theme(legend.key = element_blank())


