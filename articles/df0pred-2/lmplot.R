plot(usd ~ day, xaxt='n')
axis.Date(1, day, format="%F")
abline(model)
