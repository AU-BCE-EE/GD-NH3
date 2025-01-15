
# removing one study with man.type.broad = NA as the man.type is mineral fertilizer + urea
df <- df[! is.na(df$man.type.broad), ]
