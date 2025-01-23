
# removing one study with man.type.broad = NA as the man.type is mineral fertilizer + urea
df <- df[! is.na(df$man.type.broad), ]

# removing study with 'green manure' 
df <- df[! df$man.type.broad == 'green manure', ]

# removing studies with 'irrigation' and 'stubble coverage' as application technique (app.tech.general)
df <- df[! df$app.treat.general %in% c('irrigation', 'stubble coverage'), ]

# removing rows with 'NA' in red2 (reduction)
df <- df[! df$red.2 == 'N/A', ]

# instances of 'Surface broadcast' changed to 'surface broadcast'
df$app.treat.general <- gsub('Surface broadcast', 'surface broadcast', df$app.treat.general)

# instances of 'Injection' changed to 'injection'
df$app.treat.general <- gsub('Injection', 'injection', df$app.treat.general)

