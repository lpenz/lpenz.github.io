#!/usr/bin/Rscript

library('reshape', quietly=TRUE, warn.conflicts=FALSE)
library('ggplot2', quietly=TRUE)

a <- commandArgs(TRUE)

input  <- a[1]
output <- a[2]
num    <- as.integer(a[3])

dat <- read.table(input, header=T, sep=',')
dat <- dat[0:num,]

png(output, width=800)
ggplot(data=dat, aes(x=n)) +
	geom_line(aes(y=ksn, color="k/n")) +
	geom_line(aes(y=p, color="p min")) +
	geom_hline(aes(yintercept=0.25, color="true p")) +
	theme(legend.title = element_blank()) + ylab('k/n') +
	labs(title = 'k/n and p min estimate evolution')

