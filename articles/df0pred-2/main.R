#!/usr/bin/Rscript

source('datain.R')
source('func.R')
source('deltacalc.R')
source('daysleftcalc.R')
source('df0densitycalc.R')

model <- lm(usd ~ day)

save(list=c('usd', 'day', 'f', 'dudelta', 'freespace', 'daysleft', 'model', 'today', 'dens', 'densdays'), file='main.RData')

