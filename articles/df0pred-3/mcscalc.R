# Number of Monte Carlo samples
numsimulations <- 10000

# Number of days to simulate
numdays    <- 240

# Simulate:
simulate <- function(data, ndays) {
	delta <- diff(data)
	dssimtmp0 <- replicate(numsimulations, tail(data, 1))
	dssimtmp  <- dssimtmp0
	f <- function(i) dssimtmp <<- dssimtmp + replicate(numsimulations, sample(delta, 1, replace=TRUE))
	cbind(dssimtmp0, mapply(f, seq(1, ndays)))
}
dssim <- simulate(usd, numdays)

# Future days:
fday <- seq(today, today+numdays, by='day')

