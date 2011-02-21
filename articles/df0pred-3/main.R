#!/usr/bin/Rscript

source('datain.R')
source('mcscalc.R')

save(list=c('usd', 'day', 'today', 'numsamples', 'numdays', 'dudelta', 'dssim'), file='main.RData')

warnings()

