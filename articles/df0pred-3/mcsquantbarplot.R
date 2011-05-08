q <- 6
f <- function(i) {
	qa <- quantile(dssim[,i], seq(0, 1, 1.0/(q-1)))
	c(qa[1], mapply(function(j) qa[j] - qa[j-1], seq(2, q)))
}
qvals <- mapply(f, seq(1, numdays+1))
colors <- c('white', rainbow(q/2), tail(rev(rainbow(q/2)), -1))
barplot(qvals, ylab='usd', xaxt='n', col=colors, ylim=c(min(dssim), max(dssim)), border=NA, space=0)
axis.Date(1, day, at=seq(min(fday), max(fday), 'week'), format='%F')
lines(seq(0, numdays), replicate(numdays+1, totalspace), col='gray')
