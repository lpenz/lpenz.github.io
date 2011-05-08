boxplot(dssim, outline=F, names=seq(today, as.Date(today+numdays), by='day'))
lines(seq(0, numdays), replicate(numdays+1, totalspace), col='gray')
