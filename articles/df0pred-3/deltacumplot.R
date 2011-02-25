boxplot(mapply(function(i) dudelta[seq(1, i)], seq(1, length(dudelta))), names=tail(day, -1))
