# ALFAM2 predictions of relative emission reductions for GD Table 13 scenarios

rm(list = ls())

source('packages.R')
source('functions.R')
source('load.R')
source('clean.R')
knit('pred_emis.Rmd', output = '../log/pred_emis.md')
source('summ.R')
source('export.R')
source('plot.R')


