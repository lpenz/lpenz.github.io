#!/usr/bin/Rscript

source('datain.R')

source('func.R')

dudelta <- diff(usd)
png('daysleft.png')
source('daysleftcode.R')

