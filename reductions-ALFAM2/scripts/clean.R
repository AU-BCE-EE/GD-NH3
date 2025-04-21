# Clean data before running ALFAM2 model

# Add group
dat[, group := tolower(paste(sid, descrip, man.source))]

# Add bogus TAN.app column for now to avoid error (see issue #101)
dat[, TAN.app := 1]


