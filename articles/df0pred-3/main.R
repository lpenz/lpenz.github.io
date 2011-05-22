#!/usr/bin/Rscript

source('datain.R')
source('mcscalc.R')
source('mcscv1calc.R')
source('mcscv2calc.R')

save(list=c('usd', 'day', 'totalspace', 'today', 'numsimulations', 'numdays', 'fday', 'dssim', 'dssim2', 'dssim3'), file='main.RData')

warnings()

