# Number of Monte Carlo samples
numsimulations <- 10000

# Number of days to simulate
numdays    <- 240

# Simulate:
simulate <- function() {
	dssimtmp0 <- replicate(numsimulations, tail(usd, 1))
	dssimtmp  <- dssimtmp0
	f <- function(i) dssimtmp <<- dssimtmp + replicate(numsimulations, sample(dudelta, 1, replace=TRUE))
	cbind(dssimtmp0, mapply(f, seq(1, numdays)))
}
dssim <- simulate()

# Future days:
fday <- seq(today, today+numdays, by='day')

