numsamples <- 5000 # Number of Monte Carlo samples
numdays    <- 120  # Number of days to simulate

dudelta <- diff(usd)

dssimtmp <- replicate(numsamples, tail(usd, 1))
f <- function(i) {
	dssimtmp <<- dssimtmp - replicate(numsamples, sample(dudelta, 1, replace=TRUE))
}
dssim <- mapply(f, seq(1, numdays))

