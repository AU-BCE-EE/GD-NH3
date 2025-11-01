
df$DMr <- df$ref.dm - df$dil.dm

# calculating the average reduction pr ref 
summ1 <- df[, .(red = mean(red), 
                 red.sd = sd(red), 
                 dil.fac = mean(dil.fac), 
                 dil.fac.sd = sd(dil.fac), 
                ref.dm = mean(ref.dm), 
                ref.dm.sd = sd(ref.dm), 
                dil.dm = mean(dil.dm), 
                dil.dm.sd = mean(dil.dm), 
                DM.r = mean(DMr), 
                DM.r.sd = mean(DMr)) 
                 , by = study]

dfsumm <- summ1[, .(red.avg = mean(red), 
                 red.lwr = t.test(red)$conf.int[1],
                 red.upr = t.test(red)$conf.int[2],
                 red.sd = sd(red), 
                 dil.fac.avg = mean(dil.fac), 
                 dil.fac.sd = sd(dil.fac), 
                 ref.dm.avg = mean(ref.dm), 
                 dil.dm.avg = mean(dil.dm), 
                 DM.r.avg = mean(DM.r),
                 study.no = length(unique(study)))]


# Count application methods
appsumm <- table(df$app.meth)
