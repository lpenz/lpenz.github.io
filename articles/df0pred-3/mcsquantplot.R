q <- 7
f <- function(i) quantile(dssim[,i], seq(0, 1, 1.0/(q-1)))
qvals <- mapply(f, seq(1, numdays+1))
colors <- c(rainbow(1+q/2), tail(rev(rainbow(1+q/2)), -1))
plot(fday, qvals[1,], ylab='usd', xaxt='n', type='l', col=colors[1], ylim=c(min(qvals), max(qvals)))
mapply(function(i) lines(fday, qvals[i,], col=colors[i]), seq(2, q))
axis.Date(1, day, at=seq(min(fday), max(fday), 'week'), format='%F')
lines(seq(0, numdays), replicate(numdays+1, totalspace), col='gray')
