
# calculating the average reduction pr ref 
dfsumm <- df[, .(red.avg = mean(red), 
                 red.sd = sd(red), 
                 dil.fac.avg = mean(dil.fac), 
                 dil.fac.sd = sd(dil.fac)) 
                 , by = study]

dfsumm <- df[, .(red.avg = mean(red), 
                 red.sd = sd(red), 
                 dil.fac.avg = mean(dil.fac), 
                 dil.fac.sd = sd(dil.fac), 
                 study.no = length(unique(study)))]
