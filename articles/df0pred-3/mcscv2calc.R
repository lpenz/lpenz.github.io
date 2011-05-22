# Number of days to use in the training set
numdaysTrain <- 30
numdaysVal   <- length(day) - numdaysTrain

dssim3 <- simulate(usd[seq(1, numdaysTrain)], numdaysVal-1)
