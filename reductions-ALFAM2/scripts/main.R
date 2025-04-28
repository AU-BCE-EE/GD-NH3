# ALFAM2 predictions of relative emission reductions for guidance document

rm(list = ls())

source('packages.R')
source('functions.R')
source('load.R')
source('clean.R')
knit('ALFAM2_calcs.Rmd', output = '../logs/ALFAM2_calcs_log.md')
source('summ.R')
source('export.R')
source('plot.R')


