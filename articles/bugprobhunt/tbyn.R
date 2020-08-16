#!/usr/bin/Rscript

library('reshape', quietly=TRUE, warn.conflicts=FALSE)
library('ggplot2', quietly=TRUE)

a <- commandArgs(TRUE)

input  <- a[1]
output <- a[2]
num    <- as.integer(a[3])
maxy   <- as.integer(a[4])

dat <- read.table(input, header=T, sep=',')
dat <- dat[0:num,]

dat$t <- log(0.05) / log(1 - dat$p)
dat$tn <- dat$t + dat$n

png(output, width=800)
ggplot(data=dat, aes(x=n)) +
	geom_line(aes(y=t, color="t")) +
	geom_line(aes(y=t+n, color="t+n")) +
	scale_y_continuous(limits = c(0, maxy)) +
	theme(legend.title = element_blank()) +
	labs(title = 't by n for 95% significance')

