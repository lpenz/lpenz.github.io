h <- hist(today+daysleft, 20, ylim=c(0,1)) # Trick to generates the date axis
lines(h$mids, cumsum(h$intensities)*diff(h$breaks))
