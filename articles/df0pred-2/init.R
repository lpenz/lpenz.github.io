
duinfo <- read.table('duinfospike.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
attach(duinfo)
model <- lm(usd ~ day)

dudelta <- diff(usd)

f <- function(spaceleft) {
    days <- 0
    while(spaceleft > 0) {
        days <- days + 1
        spaceleft <- spaceleft - sample(dudelta, 1, replace=TRUE)
    }
    days
}

freespace <- 1e9 - duinfo$usd[length(duinfo$usd)]
daysleft <- replicate(100, f(freespace))

# vim: ft=r

