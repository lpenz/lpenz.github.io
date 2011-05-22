q <- 25
f <- function(i) {
	if (i <= length(day))
		replicate(q+1, 0)
	else {
		qa <- quantile(dssim[,i-length(day)], seq(0, 1, 1.0/q))
		c(qa[1], diff(qa))
	}
}
qvals <- mapply(f, seq(1, length(day)+numdays+1))
colors <- c('white', colorsDouble(heat.colors, q))
barplot(qvals, ylab='usd', xlab='day', col=colors, border=NA, space=0,
	names.arg=c(day, seq(min(fday), max(fday), 'day')), ylim=c(min(c(usd, dssim)), max(dssim)))
points(usd)
abline(h=totalspace, col='gray')
