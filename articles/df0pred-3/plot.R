#!/usr/bin/Rscript

a <- commandArgs(TRUE)

load('main.RData')
source('funcs.R')

png(paste(a, '.png', sep=''), width=640)
source(paste(a, 'plot.R', sep=''))

