# Sort out date text

df[, study := gsub(' ([12])', ' (\\1', study)]
df[, study := gsub('([0-9ab])$', '\\1)', study)]
df[, study := gsub(',', '', study)]


# Exclude laboratory emission measurements
df <- df[!grepl("beudert et al. \\(1988\\)", df$study, ignore.case = TRUE), ]
