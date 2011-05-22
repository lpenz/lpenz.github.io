quantheatplot <- function(x, sim, ylim) {
	q <- 25
	simstart <- length(x) - length(sim[1,])
	f <- function(i) {
		if (i < simstart)
			replicate(q+1, 0)
		else {
			qa <- quantile(sim[,i-simstart], seq(0, 1, 1.0/q))
			c(qa[1], diff(qa))
		}
	}
	qvals <- mapply(f, seq(1, length(x)))
	colors <- c('white', colorsDouble(heat.colors, q))
	barplot(qvals, ylab='usd', xlab='day', col=colors, border=NA, space=0,
		names.arg=x, ylim=ylim)
	abline(h=totalspace, col='gray')
}
