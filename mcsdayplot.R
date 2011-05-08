plot(dssim[,1], xlab='simulation', ylab='usd', col='blue')
points(dssim[,10], col='brown')
points(dssim[,100], col='green')
legend('top', c('1 day', '10 days', '100 days'), fill=c('blue', 'brown', 'green'))
