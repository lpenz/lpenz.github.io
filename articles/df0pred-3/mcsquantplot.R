q <- 6
f <- function(i) quantile(dssim[,i], seq(0, 1, 1.0/q))
qvals <- mapply(f, seq(1, numdays+1))
colors0 <- rainbow(1+q/2)
colors <- c(colors0, rev(if (q %% 2) colors0 else head(colors0, -1)))
plot(fday, qvals[1,], ylab='usd', xaxt='n', type='l', col=colors[1], ylim=c(min(qvals), max(qvals)))
mapply(function(i) lines(fday, qvals[i,], col=colors[i]), seq(2, q+1))
axis.Date(1, day, at=seq(min(fday), max(fday), 'week'), format='%F')
lines(seq(0, numdays), replicate(numdays+1, totalspace), col='gray')
