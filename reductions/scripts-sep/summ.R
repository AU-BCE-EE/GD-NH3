
# Summarize data

# Long data frame for use below
dl <- melt(dat, id.vars = c('source', 'set', 'slurry.source', 'frac.studA', 'app.meth'), 
           measure.vars = c('DM', 'emis.perc'), na.rm = TRUE)

# Spread out fractions
dw <- dcast(dl, source + set + slurry.source + app.meth ~ variable + frac.studA, value.var = 'value')
names(dw) <- gsub('_', '.', names(dw))

# Change in DM due to separation
dw$dDM.lf <- dw$DM.lf - dw$DM.raw
dw$rdDM.lf <- 100 * dw$dDM.lf / dw$DM.raw

# Change in emission due to separation
dw$rdemis <- 100 * dw$emis.perc.lf / dw$emis.perc.raw

dw <- as.data.table(dw)

dfsumm <- dw[, .(red.avg = mean(rdemis), 
                 red.sd = sd(rdemis), 
                 DM.red.avg = mean(na.omit(rdDM.lf)), 
                 DM.red.sd = sd(na.omit(rdDM.lf)), 
                 study.no = length(unique(source)))]

