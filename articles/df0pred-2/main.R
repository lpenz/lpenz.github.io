#!/usr/bin/Rscript

duinfo <- read.table('duinfospike.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
attach(duinfo)
model <- lm(usd ~ day)

png('lmplotspike.png')
plot(usd ~ day)
abline(model)

