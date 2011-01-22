freespace <- totalspace - tail(usd, 1)
daysleft <- replicate(100, f(freespace))
