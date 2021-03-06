#!/usr/bin/Rscript

load('main.RData')

png('pointplot.png')
plot(usd ~ day, xaxt='n')
axis.Date(1, day, format='%F')

sink(file='lm.txt')
model
sink()

sink(file='lmsummary.txt')
summary(model)
sink()

png('lmplot.png')
plot(usd ~ day, xaxt='n')
axis.Date(1, day, format='%F')
abline(model)

sink(file='predict.txt')
cat('> predict(model2, data.frame(usd = totalspace))\n')
predict(model2, data.frame(usd = totalspace))
sink()

sink(file='predictdate.txt')
cat('> as.Date(predict(model2, data.frame(usd = totalspace)), origin="1970-01-01")\n')
as.Date(predict(model2, data.frame(usd = totalspace)), origin="1970-01-01")
sink()

sink(file='lmrandsummary.txt')
summary(lm(drand$usd ~ drand$day))
sink()

