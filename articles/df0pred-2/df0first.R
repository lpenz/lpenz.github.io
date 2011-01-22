#!/usr/bin/Rscript

load('main.RData')
sink(file='df0first.txt')
cat('densdays[which(cumdens > 0.001)[1]]\n')
densdays[which(cumdens > 0.001)[1]]
sink()

