#!/usr/bin/Rscript

load('main.RData')
sink(file='deltasummary.txt')
cat('summary(dudelta)\n')
summary(dudelta)
sink()

