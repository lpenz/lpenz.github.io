boxplot(dssim, outline=F, names=seq(today, as.Date(today+numdays), by='day'), ylab='usd', xlab='day', xaxt='n')
lines(seq(0, numdays), replicate(numdays+1, totalspace), col='gray')
