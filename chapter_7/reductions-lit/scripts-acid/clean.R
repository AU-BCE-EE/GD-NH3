
# removing one study with man.type.broad = NA as the man.type is mineral fertilizer + urea
df1 <- df1[! is.na(df1$man.type.broad), ]

# removing study with 'green manure' 
df1 <- df1[! df1$man.type.broad == 'green manure', ]

# removing studies with 'irrigation' and 'stubble coverage' as application technique (app.tech.general)
df1 <- df1[! df1$app.treat.general %in% c('irrigation', 'stubble coverage'), ]

# removing rows with 'NA' in red2 (reduction)
df1 <- df1[! df1$red.2 == 'N/A', ]
