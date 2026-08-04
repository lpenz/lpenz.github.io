q <- 6
f <- function(i) quantile(dssim[,i], seq(0, 1, 1.0/q))
qvals <- mapply(f, seq(1, numdays+1))
colors <- colorsDouble(rainbow, q+1)
plot(fday, qvals[1,], ylab='usd', xlab='day', xaxt='n', type='l', col=colors[1], ylim=c(min(qvals), max(qvals)))
mapply(function(i) lines(fday, qvals[i,], col=colors[i]), seq(2, q+1))
axis.Date(1, day, at=seq(min(fday), max(fday), 'week'), format='%F')
abline(h=totalspace, col='gray')
