boxplot(dssim, outline=F, names=seq(today, as.Date(today+numdays), by='day'), ylab='usd', xlab='day', xaxt='n')
abline(h=totalspace, col='gray')
