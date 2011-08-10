#!/usr/bin/Rscript

library('reshape', quietly=TRUE, warn.conflicts=FALSE)
library('ggplot2', quietly=TRUE)

a <- commandArgs(TRUE)

red    <- as.integer(a[1])
green  <- as.integer(a[2])
output <- a[3]

k <- red
n <- red+green

x <- seq(0, 1, 0.001)
y <- dbeta(x, k+1, n-k+1) / (100 * beta(k+1, n-k+1))

png(output, width=800)
qplot(x, y, geom='line', xlab='p', ylab='P(p)')
dev.off()

