plot(df0day, df0prob, xaxt='n', type='l')
axis.Date(1, df0day, at=seq(min(df0day), max(df0day), 'year'), format='%F')
