#!/usr/bin/Rscript

library('reshape', quietly=TRUE, warn.conflicts=FALSE)
library('ggplot2', quietly=TRUE)

a <- commandArgs(TRUE)

red    <- as.integer(a[1])
green  <- as.integer(a[2])
output <- a[3]

x <- seq(0, 1, 0.001)

calc <- function(mult) {
	k <- mult * red
	n <- mult * (red+green)
	dbeta(x, k+1, n-k+1)
}

labl <- function(mult) {
	k <- mult * red
	n <- mult * (red+green)
	paste('k',k,'n',n)
}

d <- data.frame(x = c(x,x,x,x,x,x,x,x,x),
	y = c(
		calc(1),
		calc(2),
		calc(3),
		calc(4),
		calc(5),
		calc(6),
		calc(7),
		calc(8),
		calc(9)),
	params=c(
		replicate(1001, labl(1)),
		replicate(1001, labl(2)),
		replicate(1001, labl(3)),
		replicate(1001, labl(4)),
		replicate(1001, labl(5)),
		replicate(1001, labl(6)),
		replicate(1001, labl(7)),
		replicate(1001, labl(8)),
		replicate(1001, labl(9)))
		)

png(output, width=800)
ggplot(data=d, aes(x=x, y=y)) + geom_line(aes(color=params)) +
	opts(title=paste('p density for k=n/4')) +
	scale_x_continuous(name='p') +
	scale_y_continuous(name='PDF B(k=1, n=4, p=p)')

