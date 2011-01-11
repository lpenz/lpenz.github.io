hist(today+daysleft, 20) # Trick to generates the date axis
lines(density(daysleft+as.numeric(today)))
