boxplot(mapply(function(i) usd[seq(1, i)], seq(1, length(usd))), names=day)
points(usd, col='blue')
