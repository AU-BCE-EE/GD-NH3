
# Exclude incorporation from literature data
dat <- subset(dat, app.meth %in% c('band spread on slots', 'broadcast', 'trailing hose') & frac.studA != 'sf')


