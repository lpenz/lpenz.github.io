
duinfo <- read.table('duinfospike.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
attach(duinfo)
totalspace <- 400000
today <- tail(day, 1)

