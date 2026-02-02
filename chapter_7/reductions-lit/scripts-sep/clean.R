
# Exclude incorporation from literature data
# Exclude laboratory emission measurements
dat <- dat[app.meth %in% c('band spread on slots', 'broadcast', 'trailing hose') & frac.studA != 'sf' & !grepl('laboratory', meas.meth), ]


