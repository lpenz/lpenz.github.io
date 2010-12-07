
source('datain.R')

png('deltaplot.png')
source('deltaplot.R')

model <- lm(usd ~ day)

source('func.R')

freespace <- 1e9 - duinfo$usd[length(duinfo$usd)]
daysleft <- replicate(50, f(freespace))

# vim: ft=r

