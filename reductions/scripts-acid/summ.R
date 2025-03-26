
df$r <- as.numeric(df$r)
df$pH.a <- as.numeric(df$pH.a)
df$pHr <- as.numeric(df$pH.u) - as.numeric(df$pH.a)

# calculating the average reduction pr ref 
# dfsumm <- df[, .(red.avg = mean(r), 
#                  red.sd = sd(r), 
#                  pH.red.avg = mean(pHr), 
#                  pH.red.sd = sd(pHr),
#                  pH.acid.avg = mean(pH.a), 
#                  pH.acid.sd = sd(pH.a)) 
#                  , by = ID]

dfsumm <- df[, .(red.avg = mean(r), 
                 red.sd = sd(r), 
                 pH.red.avg = mean(na.omit(pHr)), 
                 pH.red.sd = sd(na.omit(pHr)),                 
                pH.acid.avg = mean(na.omit(pH.a)), 
                pH.acid.sd = sd(na.omit(pH.a)), 
                 study.no = length(unique(ID)))]
