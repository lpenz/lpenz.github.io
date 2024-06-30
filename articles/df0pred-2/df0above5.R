#!/usr/bin/Rscript

load('main.RData')
sink(file='df0above5.txt')
cat('df0day[which(df0prob > 0.05)[1]]\n')
df0day[which(df0prob > 0.05)[1]]
sink()

