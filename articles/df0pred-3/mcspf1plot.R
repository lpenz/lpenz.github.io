quantheatplot(c(day, seq(min(fday), max(fday), 'day')), dssim, ylim=c(min(c(usd, dssim)), max(dssim)))
points(usd)
abline(h=totalspace, col='gray')
