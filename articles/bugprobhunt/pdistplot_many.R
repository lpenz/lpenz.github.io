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
	dbeta(x, k+1, n-k+1) # / (100 * beta(k+1, n-k+1))
}

#d <- data.frame(x = c(x), y = c(calc(2)), mult=c(replicate(1001, 1)))
#d <- data.frame(x = c(x, x), y = c(calc(1), calc(2)), mult=c(replicate(1001, 1), replicate(1001,2)))
d <- data.frame(x = c(x,x,x,x,x), y = c(calc(1), calc(2), calc(3), calc(4), calc(5)),
	n=c(
		replicate(1001, toString(1*(red+green))),
		replicate(1001, toString(2*(red+green))),
		replicate(1001, toString(3*(red+green))),
		replicate(1001, toString(4*(red+green))),
		replicate(1001, toString(5*(red+green))))
		)

png(output, width=800)
ggplot(data=d, aes(x=x, y=y)) + geom_line(aes(color=n))
dev.off()

