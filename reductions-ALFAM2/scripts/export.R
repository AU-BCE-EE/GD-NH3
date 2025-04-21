
# Create table for document (easy copy/paste)
tab <- summ[man.source.pig == 0, .(group, lwr, rred, upr)]
tab <- rounddf(tab, digits = 0, trans = function(x) 100 * x)
tab[, val := paste0(rred, ' [', lwr, ', ', upr, ']')]
# Remove CIs for incorporation
tab[grepl('incorp', group), val := sub(' \\[.+\\]', '', val)]

# Export results
fwrite(summ, '../output/reductions.csv')
fwrite(tab, '../output/reductions_table.csv')
fwrite(predm, '../output/predictions.csv')

