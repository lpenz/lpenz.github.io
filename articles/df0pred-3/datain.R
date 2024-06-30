
duinfo <- read.table('duinfospike.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
attach(duinfo)
totalspace <- 450000
today <- tail(day, 1)

