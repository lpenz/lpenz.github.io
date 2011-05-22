q <- 25
f <- function(i) {
	if (i <= numdaysTrain)
		replicate(q+1, 0)
	else {
		qa <- quantile(dssim2[,i-numdaysTrain], seq(0, 1, 1.0/q))
		c(qa[1], diff(qa))
	}
}
qvals <- mapply(f, seq(1, length(day)))
colors <- c('white', colorsDouble(heat.colors, q))
allvals <- c(usd, dssim2)
barplot(qvals, ylab='usd', xlab='day', col=colors, border=NA, space=0,
	names.arg=day, ylim=c(min(allvals), max(allvals)))
points(usd)
abline(h=totalspace, col='gray')
abline(v=numdaysTrain, col='gray')
