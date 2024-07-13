#!/usr/bin/Rscript

load('main.RData')
sink(file='df0first.txt')
cat('df0day[1]\n')
df0day[1]
cat('df0ecdfunc(df0day[1])\n')
df0ecdfunc(df0day[1])
sink()

