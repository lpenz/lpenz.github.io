#!/usr/bin/Rscript

source('datain.R')
source('func.R')
source('deltacalc.R')
source('daysleftcalc.R')
source('df0densitycalc.R')
source('df0cumsumcalc.R')

model <- lm(usd ~ day)

save(list=c('usd', 'day', 'f', 'dudelta', 'freespace', 'daysleft', 'model', 'today', 'dens', 'densdays', 'cumdens'), file='main.RData')

