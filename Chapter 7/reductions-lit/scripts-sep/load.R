
dat <- as.data.frame(read_xlsx('../data/separation_dat_Pedersen2022.xlsx', sheet = 1, skip = 2, na = 'NA'))
dat <- as.data.table(dat)
