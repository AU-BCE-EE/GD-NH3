# Sort out date text

df <- df[, study := gsub(' ([12])', ' (\\1', study)]
df <- df[, study := gsub('([0-9ab])$', '\\1)', study)]
