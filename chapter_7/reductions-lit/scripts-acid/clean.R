
# removing one study with man.type.broad = NA as the man.type is mineral fertilizer + urea
df1 <- df1[! is.na(df1$man.type.broad), ]

# removing study with 'green manure' 
df1 <- df1[! df1$man.type.broad == 'green manure', ]

# removing studies with 'irrigation' and 'stubble coverage' as application technique (app.tech.general)
df1 <- df1[! df1$app.treat.general %in% c('irrigation', 'stubble coverage'), ]

# removing rows with 'NA' in red2 (reduction)
df1 <- df1[! df1$red.2 == 'N/A', ]

# Add () to dates
df1[, study := gsub(' ([12])', ' (\\1', study)]
df1[, study := gsub('([0-9ab])$', '\\1)', study)]

df2[, ref := gsub(' ([12])', ' (\\1', ref)]
df2[, ref := gsub('([0-9ab])$', '\\1)', ref)]

# And remove commas
df1[, study := gsub(',', '', study)]
df2[, ref := gsub(',', '', ref)]


