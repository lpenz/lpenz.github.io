
freespace <- 1e9 - duinfo$usd[length(duinfo$usd)]
daysleft <- replicate(100, f(freespace))
plot(density(daysleft))


