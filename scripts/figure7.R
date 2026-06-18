#### Calculate the mean ± standard deviation of euphotic-layer-integrated primary production and bacterial carbon metabolism for each ecological regime

library(reshape2)
library(doBy)


dat <- read.csv('../data/TWS_data.csv')

dat$DPP_TPP <- dat$DPP / dat$TPP * 100
dat$PPP_TPP <- dat$PPP / dat$TPP * 100
dat$BCD_TPP <- dat$BCD / dat$TPP * 100
dat$BP_BCD <- dat$BP / dat$BCD * 100
dat$BR_BCD <- dat$BR / dat$BCD * 100

env <- c('TPP', 'DPP', 'DPP_TPP', 'PPP', 'PPP_TPP', 'BCD_TPP', 'BCD', 'BP', 'BP_BCD', 'BR', 'BR_BCD')

dat <- melt(dat[c('Watermass', env)])
dat <- summaryBy(value~Watermass+variable, dat, FUN = c(mean, sd))

dat


# The above calculations only involve key numerical parameters, and the conceptual diagram in the manuscript was drawn using Adobe Illustrator

