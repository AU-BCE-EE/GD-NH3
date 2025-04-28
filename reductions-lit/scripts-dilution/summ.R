
# calculating the average reduction pr ref 
summ1 <- df[, .(red = mean(red), 
                 red.sd = sd(red), 
                 dil.fac = mean(dil.fac), 
                 dil.fac.sd = sd(dil.fac)) 
                 , by = study]

dfsumm <- summ1[, .(red.avg = mean(red), 
                 red.lwr = t.test(red)$conf.int[1],
                 red.upr = t.test(red)$conf.int[2],
                 red.sd = sd(red), 
                 dil.fac.avg = mean(dil.fac), 
                 dil.fac.sd = sd(dil.fac), 
                 study.no = length(unique(study)))]

