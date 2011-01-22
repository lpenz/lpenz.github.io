plot(dens$x, cumdens, xaxt='n', type='l')
axis.Date(1, densday, at=seq(min(densdays), max(densdays), 'year'), format='%F')
