q <- 25
f <- function(i) {
	qa <- quantile(dssim[,i], seq(0, 1, 1.0/q))
	c(qa[1], mapply(function(j) qa[j] - qa[j-1], seq(2, q+1)))
}
qvals <- mapply(f, seq(1, numdays+1))
colors <- c('white', colorsDouble(heat.colors, q))
barplot(qvals, ylab='usd', xlab='day', names.arg=seq(min(fday), max(fday), 'day'), col=colors, ylim=c(min(dssim), max(dssim)), border=NA, space=0)
abline(h=totalspace, col='gray')
