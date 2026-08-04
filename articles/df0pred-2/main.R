#!/usr/bin/Rscript

source('datain.R')
source('func.R')
source('deltacalc.R')
source('daysleftcalc.R')
source('df0ecdfcalc.R')

model <- lm(usd ~ day)

save(list=c('usd', 'day', 'f', 'dudelta', 'freespace', 'daysleft', 'model', 'today', 'df0day', 'df0ecdfunc', 'df0prob'), file='main.RData')

