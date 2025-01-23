
# calculating the average reduction pr ref and treat combination   ### OBS is it 'red2' that should be used? There is also a 'red' in the df
df$red.2 <- as.numeric(df$red.2)
dfsumm <- df[, .(red.avg = mean(red.2), 
                 red.sd = sd(red.2), 
                 study.no = length(unique(study.num)))]


# Compared with ranges from Fanguerio et al., 2015? 