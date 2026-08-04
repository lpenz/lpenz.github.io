freespace <- totalspace - tail(usd, 1)
daysleft <- replicate(5000, f(freespace))
