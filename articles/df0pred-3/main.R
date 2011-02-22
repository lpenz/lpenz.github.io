#!/usr/bin/Rscript

source('datain.R')
source('mcscalc.R')
source('df0calc.R')

save(list=c('usd', 'day', 'today', 'numsamples', 'numdays', 'dudelta', 'dssim', 'df0prob'), file='main.RData')

warnings()

