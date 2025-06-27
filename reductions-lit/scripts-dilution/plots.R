

ggplot(df, aes(dil.fac, red, colour = study)) + 
  geom_point() + 
  theme_bw() + 
  xlab('Dilution factor (quantity of water compared to slurry)') + ylab('Reduction due to dilution (%)') +
  theme(legend.title = element_blank())

# What needs to be done: 
# Make legends the same as for pH and separation plot.
# Adding ALFAM2 predictions, obs that TAN concentration should also half as DM half. And app mount double (probably this was already done for the predictions for the tables, didn't check
