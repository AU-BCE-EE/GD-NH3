
isub[, ct.min := min(ct), by = pmid]
t0 <- isub[ct == ct.min, ]
t0[, ct := 0]
t0[, e.rel := 0]
isub <- rbind(isub, t0)

fp <- ggplot(isub, aes(ct/24, j.rel, colour = factor(pmid))) +
	     geom_step() +
	     theme_bw() +
	     labs(x = 'Time after slurry application (d)', y = expression('Relative flux'~(h^'-1'))) +
	     theme(legend.position = 'none')

ep <- ggplot(isub, aes(ct/24, 100 * e.rel, colour = factor(pmid))) +
	     geom_line() +
	     theme_bw() +
	     labs(x = 'Time after slurry application (d)', y = 'Cum. emission (% TAN)') +
	     theme(legend.position = 'none')

pp <- grid.arrange(fp, ep, ncol = 2)

ggsave2x('../plots/emis_plot', pp, height = 2.5, width = 6)
