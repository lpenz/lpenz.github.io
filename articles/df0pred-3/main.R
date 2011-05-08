#!/usr/bin/Rscript

source('datain.R')
source('deltacalc.R')
source('mcscalc.R')
source('df0daysleftcalc.R')
source('df0calc.R')

save(list=c('usd', 'day', 'totalspace', 'today', 'numsimulations', 'numdays', 'fday', 'dudelta', 'dssim', 'daysleft', 'df0day', 'df0prob'), file='main.RData')

warnings()

