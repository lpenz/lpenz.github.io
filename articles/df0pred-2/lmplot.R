#!/usr/bin/Rscript

source('datain.R')

model <- lm(usd ~ day)

png('lmplot.png')
plot(usd ~ day)
abline(model)

