#!/usr/bin/Rscript

source('datain.R')

source('func.R')

source('deltaboxcalc.R')

source('daysleftcalc.R')

model <- lm(usd ~ day)

save(list=c('usd', 'day', 'f', 'dudelta', 'freespace', 'daysleft', 'model'), file='main.RData')

