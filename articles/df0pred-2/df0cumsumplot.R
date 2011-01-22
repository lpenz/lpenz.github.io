plot(dens$x, cumsum(dens$y * c(0, diff(dens$x))), xaxt='n', type='l')
axis.Date(1, as.Date(dens$x, origin="1970-01-01"), format='%F')
