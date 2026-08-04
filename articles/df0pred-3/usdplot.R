plot(usd~day, xaxt='n')
axis.Date(1, day, at=seq(min(day), max(day), 'week'), format='%F')
