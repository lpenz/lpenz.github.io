plot(usd ~ day, xaxt='n')
axis.POSIXct(1, at=seq(day[1], max(day)+6, "months"), format="%Y-%m-%d")
abline(model)
