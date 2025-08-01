

ggplot(df, aes(pHr, r)) + 
  geom_smooth(method = lm, aes(colour = ref, fill = ref), formula = y ~ poly(x, 2), se = TRUE, lty = 0, show.legend = FALSE, level = 0.8) +
  geom_point(aes(colour = ref)) + 
  geom_point(aes(fill = ref), pch = 21, size = 3, colour = 'gray45', show.legend = FALSE) + 
  geom_line(data = pred3, aes(dph, red), colour = 'gray45', lwd = 1.5) + 
  geom_line(data = predci, aes(dph, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dph, red.upr), lty = 2, colour = 'gray55') + 
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

df[, pH.u := as.numeric(pH.u)]
ggplot(df, aes(pHr, r)) + 
  geom_point(aes(colour = pH.u)) + 
  scale_color_viridis(option = 'D') +  
  scale_fill_viridis(option = 'D') +  
  geom_point(aes(fill = pH.u), pch = 21, size = 3, colour = 'gray45', show.legend = FALSE) + 
  geom_line(data = pred3, aes(dph, red), colour = 'gray45', lwd = 1.5) + 
  geom_line(data = predci, aes(dph, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dph, red.upr), lty = 2, colour = 'gray55') + 
  theme_bw() + 
  theme(legend.position = 'top') +
  xlab('Change in pH due to acidification') + ylab('Reduction due to acidification (%)') +
  labs(colour = 'Untreated slurry pH') 
ggsave2x('../plots/acid_3', height = 6, width = 8)

ggplot(df, aes(pHr, r)) + 
  geom_point(aes(colour = ref)) + 
  geom_line(data = pred3, aes(dph, red), colour = 'gray45', lwd = 1.5) + 
  geom_line(data = predci, aes(dph, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dph, red.upr), lty = 2, colour = 'gray55') + 
  theme_bw() + 
  xlab('Change in pH due to acidification') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/acid_4', height = 6, width = 8)

# What needs to be done: 
# Combining figures with one legend. 
# For ALFAM2 predictions, decide: Which inputs to use? What duration? Especially for slurry....? 

