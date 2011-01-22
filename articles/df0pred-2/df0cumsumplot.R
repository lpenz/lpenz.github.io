plot(dens$x, cumsum(dens$y * c(0, diff(dens$x))), xaxt='n', type='l')
axis.Date(1, densday, at=seq(min(densdays), max(densdays), 'year'), format='%F')
