#!/usr/bin/Rscript

source('init.R')

png('pointplot.png')
plot(usd ~ day)

sink(file='lm.txt')
model
sink()

sink(file='lmsummary.txt')
summary(model)
sink()

png('lmplot.png')
plot(usd ~ day)
abline(model)

drand <- read.table('drand.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
sink(file='lmrandsummary.txt')
summary(lm(drand$usd ~ drand$day))
sink()

