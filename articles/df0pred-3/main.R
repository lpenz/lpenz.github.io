#!/usr/bin/Rscript

source('datain.R')
source('mcscalc.R')

save(list=c('usd', 'day', 'totalspace', 'today', 'numsimulations', 'numdays', 'fday', 'dssim'), file='main.RData')

warnings()

