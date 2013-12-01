#!/usr/bin/Rscript

library('reshape', quietly=TRUE, warn.conflicts=FALSE)
library('ggplot2', quietly=TRUE)

a <- commandArgs(TRUE)

red    <- as.integer(a[1])
green  <- as.integer(a[2])
output <- a[3]

k <- red
n <- red+green

x <- seq(0, 1, 0.01)
df <- data.frame(x=x, y=dbinom(k, n, x))

png(output, width=800)
ggplot(df, aes(x = x, y = y)) +
	opts(title=paste('p distribution for', red, 'red and', green, 'green balls')) +
	scale_x_continuous(name='p') +
	scale_y_continuous(name='L(k=1, n=4, p=p)') +
	geom_point(size=2) + geom_linerange(aes(ymin = 0, ymax = y))
#qplot(x, y, geom='line', xlab='p', ylab='P(p)', main=paste('p distribution for', red, 'red and', green, 'green balls'))

