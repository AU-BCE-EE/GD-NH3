
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
dw$rdemis <- (dw$emis.perc.raw - dw$emis.perc.lf) / dw$emis.perc.raw * 100

dw <- as.data.table(dw)

summ1 <- dw[, .(rdemis = mean(rdemis), 
                red.sd = sd(rdemis), 
                rdDM.lf = mean(na.omit(rdDM.lf)), 
                DM.red.sd = sd(na.omit(rdDM.lf)), 
                DM.lf = mean(na.omit(DM.lf)),
                study.no = length(unique(source))), by = source]

dfsumm <- summ1[, .(red.avg = mean(rdemis), 
                    red.lwr = t.test(rdemis)$conf.int[1],
                    red.upr = t.test(rdemis)$conf.int[2],
                    red.sd = sd(rdemis), 
                    DM.red.avg = mean(na.omit(rdDM.lf)), 
                    DM.red.sd = sd(na.omit(rdDM.lf)), 
                    DM.lf.avg = mean(na.omit(DM.lf)),
                    DM.lf.sd = sd(na.omit(DM.lf)),
                    study.no = length(unique(source)))]