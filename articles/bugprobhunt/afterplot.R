#!/usr/bin/Rscript

library('reshape', quietly=TRUE, warn.conflicts=FALSE)
library('ggplot2', quietly=TRUE)

a <- commandArgs(TRUE)

input  <- a[1]
output <- a[2]
num    <- as.integer(a[3])

dat <- read.table(input, header=T, sep=',')
dat <- dat[0:num,]

pmin <- tail(dat$p, 1)

dat <- data.frame(t = seq(1, 20))
dat$significance <- 1 - (1-pmin) ^ dat$t

png(output, width=800)
ggplot(data=dat, aes(x=t)) + geom_line(aes(y=significance, color="significance")) +
	scale_y_continuous(limits = c(0, 1)) +
	geom_hline(aes(yintercept=0.95, color="95%")) +
	geom_hline(aes(yintercept=0.99, color="99%")) +
	theme(legend.title = element_blank()) +
	labs(title = 'Significance for t')
#+ opts(x=xlab='p', ylab='L(p)', main=paste('p density given k', k, 'n', n))

