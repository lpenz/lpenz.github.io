#!/usr/bin/Rscript

source('init.R')

png('lmplotspike.png')
plot(usd ~ day)
abline(model)

png('boxplot.png')
boxplot(dudelta)

png('daysleft.png')
plot(density(daysleft, bw=0.5))


