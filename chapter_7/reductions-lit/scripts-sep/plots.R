

ggplot(dw, aes(rdDM.lf, rdemis, colour = source)) + 
  geom_line(data = pred3, aes(rdm, red), colour = 'gray35') + 
  geom_line(data = predci, aes(rdm, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(rdm, red.upr), lty = 2, colour = 'gray55') + 
  geom_point() + 
  theme_bw() + 
  xlab('Reduction in LF DM compared to RS DM (%)') + ylab('Reduction in emissons from application of LF compared to RS (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/separation_1', height = 4.5, width = 5)

ggplot(dw, aes(DM.lf, rdemis, colour = source)) + 
  geom_point() + 
  theme_bw() + 
  xlab('DM of liquid fraction (%)') + ylab('Reduction in emissons from application of LF compared to RS (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/separation_2', height = 4.5, width = 5)

# Add name for other plot
dl2[, frac.stud.nm := factor(frac.studA, levels = c('raw', 'lf'), labels = c('Raw', 'LF'))]

# And add information for plotting ALFAM2 results
pred3e[, man.dm := 6 * (100 + rdm) / 100]
pred3e[, frac.stud.nm := factor(ifelse(rdm < 0, 'Raw', 'LF'))]

ggplot(dl2, aes(DM, value)) +
  geom_line(aes(colour = source, group = interaction(source, set, app.meth)), arrow = arrow(ends = 'first', length = unit(0.2, 'cm')), alpha = 0.8) +
  geom_line(data = pred3e, aes(man.dm, 100*er), colour = 'gray35', lwd = 1.1, arrow = arrow(ends = 'first', length = unit(0.3, 'cm'))) +
  geom_line(data = pred3e, aes(man.dm, 100*er.lwr), colour = 'gray35', lty = 2) +
  geom_line(data = pred3e, aes(man.dm, 100*er.upr), colour = 'gray35', lty = 2) +
  theme_bw() +
  theme(legend.position = 'top', legend.text = element_text(size = 7)) +
  guides(colour = guide_legend(ncol = 3)) +
  #scale_colour_manual(values = viridis(4)) +
  labs(x = 'Raw slurry or liquid fraction DM (%)', y = 'Emission factor (% of TAN)', shape = '', colour = '', lty = '')
ggsave2x('../plots/separation_3', height = 7.5, width = 5)

# What needs to be done: 
# Are any of these plots informative at all? Shows that the end DM or relative change in DM is not enough to predict reduction efficiency. 
# Should we instead add a plot like in Pedersen et al. 2022 with the arrows from RS EF to LF EF? 
# Whatever plots we have must be combined, one common legend. 
# Remove () around years to be consistent with acid plots 
# Change 'Anderson et al., in prep as it is now published. 
# Add ALFAM2 something.... depending on which plots we will show 

