plot(dens$x, cumsum(dens$y * c(0, diff(dens$x))), xaxt='n', type='l')
axis.Date(1, densday, at=seq(densdays[1], tail(densdays, 1), 'month'), format='%F')
