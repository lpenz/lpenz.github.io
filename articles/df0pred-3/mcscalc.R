numsimulations <- 10000 # Number of Monte Carlo samples
numdays    <- 240   # Number of days to simulate

dssimtmp0 <- replicate(numsimulations, tail(usd, 1))
dssimtmp  <- dssimtmp0
f <- function(i) dssimtmp <<- dssimtmp + replicate(numsimulations, sample(dudelta, 1, replace=TRUE))
dssim <- cbind(dssimtmp0, mapply(f, seq(1, numdays)))

fday <- seq(today, today+numdays, by='day') # Future days.

