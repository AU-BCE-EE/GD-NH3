
# selecting category 1 application techniques
df <- df[df$app.treat.general %in% c('open slot injection', 'closed slot injection', 'broadcast / incorporation', 'banding', 'Trailing shoe', 'Banded / aeration', 'broadcast / aeration'), ]

# trailing hose, trailing shoe, shallow injection, deep injection

# data frame for application techniques
df <- df[! df$app.ref2 == df$app.treat.general , ]

unique(df$additive.class)
# selecting observations without slurry treatments (UI, acid, dilution etc.)
df <- df[df$additive.class == 'n/a' | is.na(df$additive.class), ]

# overview of data 
table(df$app.ref2, df$app.treat.general)



# table for solid data 
df.solid <- df[df$man.tex == 'Solid', ]

