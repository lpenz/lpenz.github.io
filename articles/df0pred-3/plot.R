#!/usr/bin/Rscript

a <- commandArgs(TRUE)

load('main.RData')
source('funcs.R')

png(paste(a, '.png', sep=''))
source(paste(a, 'plot.R', sep=''))

