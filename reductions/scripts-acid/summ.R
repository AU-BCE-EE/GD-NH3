
df$r <- as.numeric(df$r)
df$pH.a <- as.numeric(df$pH.a)
df$pHr <- as.numeric(df$pH.u) - as.numeric(df$pH.a)

# calculating the average reduction pr ref 
summ1 <- df[, .(red = mean(r), 
                 red.sd = sd(r), 
                 pHr = mean(pHr), 
                 pHr.sd = sd(pHr),
                 pH.a = mean(pH.a), 
                 pH.a.sd = sd(pH.a)), by = ID]

dfsumm <- summ1[, .(red.avg = mean(red), 
                    red.sd = sd(red), 
                    red.lwr = t.test(red)$conf.int[1],
                    red.upr = t.test(red)$conf.int[2],
                    pH.red.avg = mean(na.omit(pHr)), 
                    pH.red.sd = sd(na.omit(pHr)),                 
                    pH.acid.avg = mean(na.omit(pH.a)), 
                    pH.acid.sd = sd(na.omit(pH.a)), 
                    study.no = length(unique(ID)))]
