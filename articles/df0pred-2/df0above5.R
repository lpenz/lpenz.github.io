#!/usr/bin/Rscript

load('main.RData')
sink(file='df0above5.txt')
cat('densdays[which(cumdens > 0.05)[1]]\n')
densdays[which(cumdens > 0.05)[1]]
sink()

