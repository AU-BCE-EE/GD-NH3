

ggplot(df, aes(pHr, r)) + 
  geom_line(data = pred3, aes(dph, red), colour = 'gray35') + 
  geom_line(data = predci, aes(dph, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dph, red.upr), lty = 2, colour = 'gray55') + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = 'skyblue', lwd = 0.8) +
  geom_smooth(data = df[pHr <= 2.5, ], method = lm, formula = y ~ x - 1, se = FALSE, col = 'pink', lwd = 0.8) +
  geom_smooth(data = df[pHr <= 3, ], se = FALSE, col = 'orange', lwd = 0.8) +
  geom_point(aes(colour = ref)) + 
  theme_bw() + 
  xlab('Change in pH due to acidification') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/acid_1', height = 6, width = 8)

ggplot(df, aes(pH.a, r, colour = ref)) + 
  geom_point() + 
  theme_bw() + 
  xlab('pH of acidified slurry') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/acid_2', height = 6, width = 8)

# What needs to be done: 
# Combining figures with one legend. 
# For ALFAM2 predictions, decide: Which inputs to use? What duration? Especially for slurry....? 

