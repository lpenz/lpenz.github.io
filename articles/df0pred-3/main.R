#!/usr/bin/Rscript

source('datain.R')
source('mcscalc.R')
source('mcscvcalc.R')

save(list=c('usd', 'day', 'totalspace', 'today', 'numsimulations', 'numdays', 'fday', 'dssim', 'dssim2', 'numdaysTrain', 'numdaysVal'), file='main.RData')

warnings()

