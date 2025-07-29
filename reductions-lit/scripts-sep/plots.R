

ggplot(dw, aes(rdDM.lf, rdemis, colour = source)) + 
  geom_line(data = pred3, aes(rdm, red), colour = 'gray35') + 
  geom_line(data = predci, aes(rdm, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(rdm, red.upr), lty = 2, colour = 'gray55') + 
  geom_point() + 
  theme_bw() + 
  xlab('Reduction in LF DM compared to RS DM (%)') + ylab('Reduction in emissons from application of LF compared to RS (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/separation_1', height = 6, width = 8)

ggplot(dw, aes(DM.lf, rdemis, colour = source)) + 
  geom_point() + 
  theme_bw() + 
  xlab('DM of liquid fraction (%)') + ylab('Reduction in emissons from application of LF compared to RS (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/separation_2', height = 6, width = 8)

# What needs to be done: 
# Are any of these plots informative at all? Shows that the end DM or relative change in DM is not enough to predict reduction efficiency. 
# Should we instead add a plot like in Pedersen et al. 2022 with the arrows from RS EF to LF EF? 
# Whatever plots we have must be combined, one common legend. 
# Remove () around years to be consistent with acid plots 
# Change 'Anderson et al., in prep as it is now published. 
# Add ALFAM2 something.... depending on which plots we will show 

