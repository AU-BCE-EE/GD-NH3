
# selecting category 1 application techniques
df <- df[df$app.treat.general %in% c('injection', 'broadcast / incorporation', 'banding', 'Trailing shoe'), ]

# trailing hose, trailing shoe, shallow injection, deep injection - OBS, in the above we don't distinguish between 'deep and shallow' injection

# data frame for application techniques
df <- df[! df$app.ref2 == df$app.treat.general , ]

# selecting observations without slurry treatments (UI, acid, dilution etc.)
df <- df[df$additive.class == 'n/a' | is.na(df$additive.class), ]

# overview of data 
table(df$app.ref2, df$app.treat.general)
