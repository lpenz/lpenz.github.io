dssimchosen <- list(density(dssim[,5]), density(dssim[,15]), density(dssim[,45]), density(dssim[,120]))
colors <- rainbow(length(dssimchosen))
xs <- c(mapply(function(d) d$x, dssimchosen))
ys <- c(mapply(function(d) d$y, dssimchosen))
plot(dssimchosen[[1]], xlab='usd', ylab='dens',
	xlim=c(min(xs),max(xs)), ylim=c(min(ys),max(ys)), col=colors[1], main='')
lines(dssimchosen[[2]], col=colors[2])
lines(dssimchosen[[3]], col=colors[3])
lines(dssimchosen[[4]], col=colors[4])
abline(v=totalspace, col='gray')
legend('top', c('5 day', '15 days', '45 days', '120 days'), fill=colors)
