#### Comparison of euphotic-zone-averaged environmental variables among ecological regimes

library(reshape2)
library(doBy)


# Calculate the mean ± standard deviation of each environmental factor for individual water masses.
dat <- read.csv('../data/TWS_data.csv')

dat$Chla_Micro_prop <- dat$Chla_Micro / dat$Chla * 100
dat$Chla_Nano_prop <- dat$Chla_Nano / dat$Chla * 100
dat$Chla_Pico_prop <- dat$Chla_Pico / dat$Chla * 100

env <- c('T', 'S', 'DIN', 'DIP', 'DSi', 'Chla', 'Chla_Micro_prop', 'Chla_Nano_prop', 'Chla_Pico_prop')

dat <- melt(dat[c('Watermass', env)])
dat <- summaryBy(value~Watermass+variable, dat, FUN = c(mean, sd))

dat


# Boxplots in "figure2.R" illustrate the inter-water-mass comparisons of all environmental factors

