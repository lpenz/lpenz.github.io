#!/usr/bin/Rscript

source('init.R')

png('lmplotspike.png')
plot(usd ~ day)
abline(model)

png('daysleft.png')
source('daysleftplot.R')

