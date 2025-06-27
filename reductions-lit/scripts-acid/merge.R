

# selecting the column with article ID and reductions
df1 <- df1[, c('study', 'study.num', 'red.2', 'pH', 'pH.treat')]
df2 <- df2[, c('ref', 'ID', 'r', 'pH.u', 'pH.a')]

# converting from fraction to % reduction
df2$r <- df2$r * 100

colnames(df1) <- c('ref', 'ID', 'r', 'pH.u', 'pH.a')

df <- rbind(df1, df2)
