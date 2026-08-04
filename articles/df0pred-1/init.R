#!/usr/bin/Rscript

source('datain.R')
model <- lm(usd ~ day)
model2 <- lm(day ~ usd)

drand <- read.table('drand.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))

