#!/usr/bin/Rscript

duinfo <- read.table('duinfo.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
attach(duinfo)
model <- lm(usd ~ day)


