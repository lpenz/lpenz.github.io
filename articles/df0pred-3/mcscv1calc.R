# Number of days to use in the training set
numdaysTrain <- 60
numdaysVal   <- length(day) - numdaysTrain

dssim2 <- simulate(usd[seq(1, numdaysTrain)], numdaysVal-1)
