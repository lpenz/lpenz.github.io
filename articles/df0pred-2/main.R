#!/usr/bin/Rscript

source('datain.R')
source('func.R')
source('deltaboxcalc.R')
source('daysleftcalc.R')
source('df0datecalc.R')

model <- lm(usd ~ day)

save(list=c('usd', 'day', 'f', 'dudelta', 'freespace', 'daysleft', 'model', 'df0date'), file='main.RData')
#axis.POSIXct(1, at=seq(day[1], max(day)+6, "months"), format="%Y-%m-%d")

