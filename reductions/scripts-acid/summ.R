
df$r <- as.numeric(df$r)
df$pHr <- as.numeric(df$pH.u) - as.numeric(df$pH.a)

# calculating the average reduction pr ref 
dfsumm <- df[, .(red.avg = mean(r), 
                 red.sd = sd(r), 
                 pH.red.avg = mean(pHr), 
                 pH.red.sd = sd(pHr)) 
                 , by = ID]

dfsumm <- df[, .(red.avg = mean(r), 
                 red.sd = sd(r), 
                 pH.red.avg = mean(na.omit(pHr)), 
                 pH.red.sd = sd(na.omit(pHr)), 
                 study.no = length(unique(ID)))]